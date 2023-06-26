
if (!require("lavaan")) install.packages("lavaan") # install this package first (once)
library(lavaan)

if (!require("lavaanPlot")) install.packages("lavaanPlot") # install this package first (once)
library(lavaanPlot)

# Als lower trinagular matrix, dan:
#https://lavaan.ugent.be/tutorial/cov.html

# Nu upper triangular
lower <- c(1,
           .39, 1,
           .59, .26, 1,
           -.33, -.05, -.46, 1,
           .50, .36, .38, -.27, 1)

# Zorg wel voor een 1 op de diagonaal



covmx <- 
  getCov(lower, names = c("AMTB", "WTC",
                          "PC", "ANX",
                          "FREQ"))
covmx # Check with corr matrix in article



SEM.model <- '
   # regressions
  FREQ ~ AMTB + WTC
  WTC ~ ANX + PC + AMTB
  PC ~ ANX
  AMTB ~ PC
'

fit <- sem(SEM.model, 
           sample.cov = covmx, 
           sample.nobs = 56) # sample size van de study!
summary(fit, standardized = TRUE)



#Plot
lavaanPlot(model = fit, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), coefs = T, stand = T, covs = T, 
           stars = TRUE)



