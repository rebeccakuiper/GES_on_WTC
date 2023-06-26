
if (!require("lavaan")) install.packages("lavaan") # install this package first (once)
library(lavaan)

if (!require("lavaanPlot")) install.packages("lavaanPlot") # install this package first (once)
library(lavaanPlot)

# Als lower trinagular matrix, dan:
#https://lavaan.ugent.be/tutorial/cov.html

# Nu upper triangular
lower <- c(1,
           .65, 1,
           .30, .27, 1,
           .26, .27, .34, 1,
           .42, .37, .20, .18, 1,
           .31, .24, .07, .05, .58, 1,
           .47, .43, .23, .20, .79, .48, 1,
           -.39, -.42, -.35, -.15, -.35, -.23, -.39, 1)

# Zorg wel voor een 1 op de diagonaal


covmx <- 
  getCov(lower, names = c("WTCIC", "WTCOC",
                          "PC", "LP",
                          "MOT", "ALS",
                          "I", "ANX"))


covmx # Check with corr matrix in article


SEM.model <- '
    # latent variables
  WTC =~ NA*WTCIC + WTCOC 
  PC =~ NA*LP
  WTC ~~ 1*WTC
  PC ~~ 1*PC
  
  
  # regressions
  WTC ~ I + PC + MOT + ANX
  PC ~ ANX 
  MOT ~ ALS + I
  I ~ ANX
  LP ~ MOT
  ALS ~ I

'

fit <- sem(SEM.model, 
           sample.cov = covmx, 
           sample.nobs = 1117) # sample size van de study!
summary(fit, standardized = TRUE)


# Plot
lavaanPlot(model = fit, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), coefs = T, stand = T, covs = T, 
           stars = TRUE)



