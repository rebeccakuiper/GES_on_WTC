
if (!require("lavaan")) install.packages("lavaan") # install this package first (once)
library(lavaan)

if (!require("lavaanPlot")) install.packages("lavaanPlot") # install this package first (once)
library(lavaanPlot)

# Als lower trinagular matrix, dan:
#https://lavaan.ugent.be/tutorial/cov.html

# Nu upper triangular
lower <- c(1,
           .64, 1,
           .33, .37, 1,
           .33, .20, .21, 1,
           -.42, -.68, -.52, -.11, 1,
           .39, .41, .27, .19, -.36, 1,
           .56, .63, .45, .32, -.64, .38, 1,
           .43, .47, .36, .17, -.40, .34, .58, 1,
           .39, .49, .22, .07, -.40, .41, .51, .44, 1)

# Zorg wel voor een 1 op de diagonaal


covmx <- 
  getCov(lower, names = c("MOT", "SC",
                          "RT", "G",
                          "SA", "VIE",
                          "WTCIC", "WTCOC", 
                          "WTCDS"))


covmx # Check with corr matrix in article


SEM.model <- '
    # latent variables
  WTC =~ WTCIC + WTCOC + WTCDS
  
  # regressions
  WTC ~ MOT + SA + SC 
  
'

fit <- sem(SEM.model, 
           sample.cov = covmx, 
           sample.nobs = 176) # sample size van de study!
summary(fit, standardized = TRUE)


# Plot
lavaanPlot(model = fit, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), coefs = T, stand = T, covs = T, 
           stars = TRUE)



