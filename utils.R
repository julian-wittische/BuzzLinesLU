######################## PROJECT: BuzzLines connectivity
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: Axel Hochkirch/Alexander Weigand/Balint Andrasi
# Start: Spring 2026
# Data: MNHNL
# Script objective : utility functions

############ Resistance transformation ----
res.tran_JW <- function(x, k = 0.5, ymax = 1, shape) {
  # x    — vector of resource values
  # k    — half-saturation constant (value of x where f(x) = ymax/2) for S; unitless for A (1/k)
  # ymax — asymptote / maximum output (default 1)
  # shape — "S" for saturating or "A" for accelerating or "U" for
  # Normalise x to [0, 1] — handles any units and any starting value
  
  x_norm <- (x - min(x)) / (max(x) - min(x))
  
  if (shape == "S") {
    # Monod — concave, saturating
    # k: fraction of x-range where f(x) = ymax/2
    return(ymax * x_norm / (k + x_norm))
    
  } else if (shape == "A") {
    # Power function — purely convex, accelerating throughout
    # exponent n = 1/k: smaller k → steeper acceleration
    # k=0.5 → n=2 (quadratic), k=0.3 → n=3.3, k=0.2 → n=5
    n <- 1 / k
    return(ymax * x_norm^n)
    
  } else if (shape == "U") {
    # k = minimum value (at centre of x range)
    # ymax = maximum value (at both edges)
    return(k + (ymax - k) * (2 * (x_norm - 0.5))^2)
    
  } else {
    stop(paste0("Unknown shape '", shape, "'. Use 'S' or 'A'."))
  }
}



###### Test
xlol <- seq(0.1, 10, 0.5)
# Saturating
lol <- res.tran_JW(x=xlol, k=0.3, ymax=10, shape="S")
plot((xlol - min(xlol)) / (max(xlol) - min(xlol)),lol)

# Accelerating
lol2 <- res.tran_JW(x=xlol, k=0.25, ymax=10, shape="A")
plot((xlol - min(xlol))/(max(xlol) - min(xlol)),lol2)

# U-shaped
lol3 <- res.tran_JW(x=xlol, k=0.25, ymax=10, shape="U")
plot((xlol - min(xlol))/(max(xlol) - min(xlol)),lol3)

