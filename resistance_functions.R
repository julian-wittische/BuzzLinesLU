######################## PROJECT: BuzzLines connectivity
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: Axel Hochkirch/Alexander Weigand/Balint Andrasi
# Start: Spring 2026
# Data: MNHNL
# Script objective : Function to transform environmental values in resistance values

############ Description ----

# =============================================================================
# resistance_functions.R
#
# Build resistance surfaces (1–10000, log10 scale) in R/terra for use in
# Omniscape.jl. Two transformation paths:
#
#   - resist_categorical() : discrete land-cover-type classes -> resistance
#   - resist_continuous()  : continuous covariate -> resistance, using
#                             user-supplied "anchor points" (original value ->
#                             target resistance). Anchors do not need to be
#                             monotonic, so this same function also produces
#                             parabolic (U-shaped / inverted-U) relationships
#                             when you give it a center point plus values on
#                             both sides.
#
# All resistance transforms are done by LINEAR interpolation in log10(R)
# space between anchors, then back-transformed (10^...). That is what makes
# 1, 10, 100, 1000, 10000 sit at even, equally-spaced "steps" instead of the
# curve being dominated by the huge 1->10000 range.
# =============================================================================

library(terra)

# -----------------------------------------------------------------------------
# 1. CATEGORICAL TRANSFORM
# -----------------------------------------------------------------------------
#' Reclassify a categorical raster into resistance values
#'
#' @param r        SpatRaster of integer/categorical codes (e.g. land cover)
#' @param lookup   data.frame or matrix with two columns: original value,
#'                 resistance value. e.g.
#'                 data.frame(value = c(1,2,3,4),
#'                            resistance = c(1, 100, 1000, 10000))
#' @param others   value to assign to any raster cell not listed in `lookup`
#'                 (default NA -- forces you to notice unmapped classes)
#' @param filename optional path to write the output GeoTIFF
#' @return SpatRaster of resistance values
resist_categorical <- function(r, lookup, others = NA, filename = NULL, ...) {
  lookup <- as.data.frame(lookup)
  stopifnot(ncol(lookup) == 2)
  bad <- lookup[[2]] <= 0
  if (any(bad)) stop("Resistance values must be > 0 (you gave: ",
                     paste(lookup[[2]][bad], collapse = ", "), ")")
  
  rcl <- as.matrix(lookup)
  out <- classify(r, rcl = rcl, others = others)
  names(out) <- "resistance"
  
  if (!is.null(filename)) {
    writeRaster(out, filename, overwrite = TRUE, datatype = "FLT4S", ...)
  }
  out
}

# -----------------------------------------------------------------------------
# 2. CONTINUOUS TRANSFORM (also covers parabolic relationships)
# -----------------------------------------------------------------------------
#' Build a log-scale interpolating function from anchor points
#'
#' Internal helper. x = original covariate values, y = target resistance at
#' those values. x need not be monotonic in y (that's what allows parabolas).
#' Outside the range of x, the function is flat-extrapolated (clamped) by
#' default, i.e. the resistance at the nearest anchor is reused.
make_log_interpolator <- function(x, y, clamp_ends = TRUE) {
  stopifnot(length(x) == length(y), length(x) >= 2)
  if (any(y <= 0)) stop("Resistance anchor values must be > 0 (log10 scale).")
  
  ord <- order(x)
  x <- x[ord]
  y <- y[ord]
  if (any(duplicated(x))) stop("Duplicate x (original-value) anchors are not allowed.")
  
  logy <- log10(y)
  approxfun(x, logy, method = "linear",
            rule = if (clamp_ends) 2 else 1, ties = "ordered")
}

#' Transform a continuous raster into resistance values via anchor points
#'
#' @param r          SpatRaster of a continuous covariate (e.g. slope, distance
#'                    to road, elevation, temperature...)
#' @param x           numeric vector: original covariate values ("anchors")
#' @param y           numeric vector: resistance value desired at each x.
#'                    Must be same length as x, values in 1..10000.
#'                    For a simple monotonic ramp you might supply, e.g.:
#'                       x = c(0,  5,  20,  60)
#'                       y = c(1, 10, 1000, 10000)
#'                    For a PARABOLIC relationship (low resistance near some
#'                    optimal value, rising on both sides), just give anchors
#'                    on both sides of the optimum, e.g. an "optimal"
#'                    temperature of 15C:
#'                       x = c(-5, 0, 5, 10, 15, 20, 25, 30, 35)
#'                       y = c(10000,1000,100,10,1,10,100,1000,10000)
#'                    (see resist_parabolic() below for a convenience wrapper
#'                    that builds x/y like this for you)
#' @param clamp_ends  if TRUE (default), covariate values beyond the min/max
#'                    anchor are held flat at the nearest anchor's resistance.
#'                    If FALSE, they become NA -- useful if you want to force
#'                    yourself to define the full data range explicitly.
#' @param hard_clamp  additionally force final output into [1, 10000] to
#'                    guard against any interpolation/extrapolation overshoot
#'                    (default TRUE; only matters if clamp_ends = FALSE and
#'                    you allow rule = 1 extrapolation elsewhere)
#' @param filename    optional path to write the output GeoTIFF
#' @return SpatRaster of resistance values
resist_continuous <- function(r, x, y, clamp_ends = TRUE, hard_clamp = TRUE,
                              filename = NULL, ...) {
  interp <- make_log_interpolator(x, y, clamp_ends = clamp_ends)
  
  out <- app(r, fun = function(v) 10^interp(v))
  
  if (hard_clamp) {
    out <- clamp(out, lower = 1, upper = 10000, values = TRUE)
  }
  names(out) <- "resistance"
  
  if (!is.null(filename)) {
    writeRaster(out, filename, overwrite = TRUE, datatype = "FLT4S", ...)
  }
  out
}

# -----------------------------------------------------------------------------
# 2b. PARABOLIC CONVENIENCE WRAPPER
# -----------------------------------------------------------------------------
#' Build anchor points for a parabolic (U-shaped or inverted-U) resistance
#' relationship around some "optimal" covariate value, and run the transform.
#'
#' Example: temperature optimum at 15, resistance climbs to 10/100/1000/10000
#' as you move away from 15 in steps of 5 on both sides, symmetric:
#'
#'   resist_parabolic(r, center_x = 15,
#'                     steps_right = c(20, 25, 30, 35),
#'                     resistance_steps = c(10, 100, 1000, 10000))
#'
#' Asymmetric version (different step locations and/or resistances on each
#' side -- e.g. cold side punishes faster than hot side):
#'
#'   resist_parabolic(r, center_x = 15,
#'                     steps_left  = c(10, 5, 0, -5),
#'                     steps_right = c(20, 25, 30, 35),
#'                     resistance_left  = c(100, 1000, 5000, 10000),
#'                     resistance_right = c(10, 100, 1000, 10000))
#'
#' @param r                 SpatRaster of the continuous covariate
#' @param center_x          covariate value where resistance = 1 (the optimum)
#' @param steps_right       covariate values above center_x, ASCENDING order,
#'                          each further from center_x than the last
#' @param steps_left        covariate values below center_x, DESCENDING order
#'                          (i.e. also moving progressively further from
#'                          center_x). Defaults to mirroring steps_right
#'                          around center_x if not supplied.
#' @param resistance_right  resistance value at each steps_right anchor
#'                          (default c(10,100,1000,10000))
#' @param resistance_left   resistance value at each steps_left anchor
#'                          (defaults to same as resistance_right)
#' @param ...               passed on to resist_continuous()
#' @return SpatRaster of resistance values
resist_parabolic <- function(r, center_x,
                             steps_right,
                             steps_left = NULL,
                             resistance_right = c(10, 100, 1000, 10000),
                             resistance_left = NULL,
                             ...) {
  if (is.null(steps_left)) {
    steps_left <- center_x - (steps_right - center_x)
  }
  if (is.null(resistance_left)) {
    resistance_left <- resistance_right
  }
  stopifnot(length(steps_left) == length(resistance_left),
            length(steps_right) == length(resistance_right))
  
  x <- c(rev(steps_left), center_x, steps_right)
  y <- c(rev(resistance_left), 1, resistance_right)
  
  resist_continuous(r, x, y, ...)
}

# -----------------------------------------------------------------------------
# 2c. DIAGNOSTIC: preview a curve BEFORE applying it to a raster
# -----------------------------------------------------------------------------
#' Plot the resistance curve implied by a set of anchor points
#'
#' Lets you sanity-check x/y anchors (monotonic or parabolic) before running
#' resist_continuous()/resist_parabolic() on an actual raster. Draws the
#' interpolated curve on a log10 y-axis, marks your anchor points, and prints
#' a small table of the anchors for a quick numeric check.
#'
#' @param x, y        same anchor vectors you'd pass to resist_continuous()
#' @param range_x     optional c(min, max) for the x-axis of the preview;
#'                    defaults to a bit wider than the anchor range so you can
#'                    see the flat-extrapolation behavior at the edges
#' @param n           number of points used to draw the smooth curve
#' @param clamp_ends  same meaning as in resist_continuous()/make_log_interpolator()
#' @param sample_values optional vector of specific x values to test and
#'                      print the resulting resistance for (e.g. real
#'                      quantiles from your actual raster, via
#'                      values(r) |> quantile())
plot_resistance_curve <- function(x, y, range_x = NULL, n = 500,
                                  clamp_ends = TRUE, sample_values = NULL) {
  interp <- make_log_interpolator(x, y, clamp_ends = clamp_ends)
  
  if (is.null(range_x)) {
    pad <- diff(range(x)) * 0.15
    range_x <- c(min(x) - pad, max(x) + pad)
  }
  
  xx <- seq(range_x[1], range_x[2], length.out = n)
  yy <- 10^interp(xx)
  
  op <- par(mar = c(4.5, 4.5, 2, 1))
  on.exit(par(op))
  plot(xx, yy, type = "l", lwd = 2, log = "y",
       xlab = "original value", ylab = "resistance (log10 scale)",
       yaxt = "n",
       main = "IS THIS WHAT YOU WANT?")
  axis(2, at = c(1, 10, 100, 1000, 10000),
       labels = c("1", "10", "100", "1000", "10000"), las = 1)
  abline(h = c(1, 10, 100, 1000, 10000), col = "grey85", lty = 3)
  points(x, y, pch = 19, col = "firebrick", cex = 1.3)
  text(x, y, labels = y, pos = 3, col = "firebrick", cex = 0.8, xpd = TRUE)
  
  cat("Anchor points:\n")
  print(data.frame(x = x, resistance = y)[order(x), ], row.names = FALSE)
  
  if (!is.null(sample_values)) {
    cat("\nSample lookups:\n")
    print(data.frame(x = sample_values,
                     resistance = round(10^interp(sample_values), 1)),
          row.names = FALSE)
  }
  
  invisible(list(x = xx, resistance = yy))
}

# -----------------------------------------------------------------------------
# 3. TOP-LEVEL DISPATCHER
# -----------------------------------------------------------------------------
#' One entry point that routes to the right transform by type
#'
#' @param r     SpatRaster
#' @param type  "categorical", "continuous", or "parabolic"
#' @param ...   arguments passed to the corresponding resist_*() function
#'              (lookup= for categorical; x=,y= for continuous;
#'              center_x=, steps_right=, ... for parabolic)
build_resistance <- function(r, type = c("categorical", "continuous", "parabolic"), ...) {
  type <- match.arg(type)
  switch(type,
         categorical = resist_categorical(r, ...),
         continuous  = resist_continuous(r, ...),
         parabolic   = resist_parabolic(r, ...))
}

# =============================================================================
# EXAMPLE WORKFLOW
# =============================================================================
if (FALSE) {
  
  landcover <- rast("landcover.tif")   # categorical
  slope     <- rast("slope.tif")       # continuous, monotonic
  tmean     <- rast("tmean.tif")       # continuous, parabolic (optimum climate)
  
  # --- ALWAYS preview a curve before running it on the raster --------------
  plot_resistance_curve(
    x = c(0, 5, 15, 30, 50),
    y = c(1, 10, 100, 1000, 10000),
    sample_values = c(2, 10, 40)   # e.g. real values pulled from your raster
  )
  
  plot_resistance_curve(
    x = c(-5, 0, 5, 10, 15, 20, 25, 30, 35),
    y = c(10000, 1000, 100, 10, 1, 10, 100, 1000, 10000)
  )
  
  # --- categorical: land cover class -> resistance -------------------------
  lc_lookup <- data.frame(
    value      = c(1,    2,   3,    4,     5),
    resistance = c(1,    10,  100,  1000,  10000)
  )
  r_lc <- resist_categorical(landcover, lc_lookup, others = 10000)
  
  # --- continuous, monotonic: steeper slope = more resistance --------------
  # "I want it to reach 10 at slope=5, 100 at slope=15, 1000 at slope=30,
  #  10000 at slope=50, and resistance=1 at slope=0"
  r_slope <- resist_continuous(
    slope,
    x = c(0, 5, 15, 30, 50),
    y = c(1, 10, 100, 1000, 10000)
  )
  
  # --- continuous, parabolic: optimum mean temperature at 15C --------------
  r_tmean <- resist_parabolic(
    tmean,
    center_x    = 15,
    steps_right = c(20, 25, 30, 35),
    steps_left  = c(10, 5, 0, -5)
    # resistance_right defaults to c(10,100,1000,10000), mirrored to left
  )
  
  # --- combine layers into one resistance surface ---------------------------
  # Circuit-theory resistance surfaces are commonly combined multiplicatively
  # (barriers compound) or by taking the max/min depending on your ecological
  # reasoning. Multiplicative example, re-clamped to the 1-10000 range:
  resistance_total <- r_lc * r_slope * r_tmean
  resistance_total <- clamp(resistance_total, lower = 1, upper = 10000, values = TRUE)
  
  # --- write out for Omniscape.jl -------------------------------------------
  # Omniscape.jl reads GeoTIFF. Float32 is a safe, compact datatype; make sure
  # the CRS is projected (equal-area or otherwise appropriate for your study
  # region) and cell size matches what you plan to declare in the Omniscape
  # .ini config.
  writeRaster(resistance_total, "resistance_surface.tif",
              overwrite = TRUE, datatype = "FLT4S")
}

############ Tests ----
# CONTINUOUS
#IMP <- rast("C:/Users/YNM724/Desktop/BuzzLine variables/IMP_10km_mask.tif")
plot_resistance_curve(x=c(0,45,75,95,100), y=c(10^0, 10^1, 10^2, 10^3, 10^4), range_x=c(0,100))
lolcont <- resist_continuous(IMP, x=c(0,45,75,95,100), y=c(10^0, 10^1, 10^2, 10^3, 10^4))

# CATEGORICAL
#GRA <- rast("C:/Users/YNM724/Desktop/BuzzLine variables/GRA_10km_mask.tif")
lc_lookup <- data.frame(
  value      = c(0, 1),
  resistance = c(10^3, 1)
)
lolcat <- resist_categorical(GRA, lookup=lc_lookup)

# PARABOLIC
#TMP <- rast("C:/Users/YNM724/Desktop/BuzzLine variables/summer_tempmax_10km_mask.tif")
plot_resistance_curve(x=c(19.26,20, 21, 22, 23.5), y=c(10^4, 10^2, 10^0, 10^2, 10^4), range_x = c(minmax(TMP)))
