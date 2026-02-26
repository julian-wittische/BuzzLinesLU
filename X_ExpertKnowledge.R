######################## PROJECT: BuzzLines connectivity
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: Axel Hochkirch/Alexander Weigand/Balint Andrasi
# Start: Spring 2026
# Data: MNHNL
# Script objective : Create an expert knowledge-based heuristic resistance layer

#########
# additive for overlap
w_GRA <- 1
w_PRO_n2000 <- 2
w_PRO_zpin <- 5  
w_BTP <- 10
######
w_matrix <- 5# Resistance

# /!\ THIS IS NOW A RESISTANCE LAYER, NOT CONUDCTANCE /!\  
EKR <- 1/(GRA*w_GRA +
       PRO_n2000_r*w_PRO_n2000 +
       PRO_zpin_r*w_PRO_zpin +
       BTP_r*w_BTP)

# Set high resistance for NA cells (not covered by anything useful)
EKR[is.na(EKR),] <- max(EKR)*w_matrix
plot(EKR)

