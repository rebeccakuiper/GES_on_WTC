
if (!require("lavaan")) install.packages("lavaan") # install this package first (once)
library(lavaan)

if (!require("lavaanPlot")) install.packages("lavaanPlot") # install this package first (once)
library(lavaanPlot)

# Als lower trinagular matrix, dan:
#https://lavaan.ugent.be/tutorial/cov.html

# Nu upper triangular
lower <- c(1,
           .48, 1,
           -.15, -.02, 1,
           .31, .09, -.51, 1,
           .16, .17, -.14, .16, 1,
           .06, .14, -.15, .12, .22, 1,
           .19, .29, -.09, .09, .30, .40, 1,
           .27, .12, -.21, .20, .24, .33, .24, 1,
           .28, .21, -.26, .33, .40, .30, .31, .37, 1, 
           .18, .09, -.51, .41, .01, -.09, .09, -.01, .14, 1,
           .09, .12, -.27, .27, .05, .01, .06, .06, .09, .38, 1, 
           -.01, .00, -.25, .25, .11, .01, .11, .11, .06, .13, .38, 1)

# Zorg wel voor een 1 op de diagonaal


covmx <- 
  getCov(lower, names = c("WTCF", "WTCM", 
                          "ANX", "PC", 
                          "RAI", "TE", "TA",  
                          "ST", "AT", 
                          "SP", "RE", "WR"))


covmx # Check with corr matrix in article


SEM.model <- '
  # latent variables
  WTC =~ NA*WTCF + WTCM 
  CE =~ NA*ST + TE + TA
  RAM =~ NA*RAI
  #CC =~ NA*ANX + PC
  FL =~ NA*RE + SP + WR
  WTC ~~ 1*WTC
  CE ~~ 1*CE 
  RAM ~~ 1*RAM 
  #CC ~~ 1*CC
  FL ~~ 1*FL
  
  # regressions
  #WTC ~ CC + CE + RAM
  WTC ~ a1*ANX + a2*PC + c1*CE + b*RAM
  #CC ~ FL + CE + RAM
  ANX ~ FL + CE + b*RAM
  PC ~ FL + CE + b*RAM
  RAM ~ AT + CE
  AT ~ CE

  # indirect Effect (a*b)
  c := c1
  a1b := a1*b
  a2b := a2*b
'

fit <- sem(SEM.model, 
           sample.cov = covmx, 
           sample.nobs = 243) # sample size van de study!
summary(fit, standardized = TRUE)
lavInspect(fit, "vcov.def") # geeft cov. matrix van ab en c

fit.std <- standardizedSolution(fit)
which(fit.std[, 'label'] == "c")
which(fit.std[, 'label'] == "a1b")
which(fit.std[, 'label'] == "a2b")
indices <- 34:35 # Dit obv bovenstaande bepaald, dus per study ws aanpassen!
est <- fit.std[indices, 'est.std']
VCOV <- lavInspect(fit, "vcov.def.std.all") # geeft cov. matrix van ab en c

# Plot
lavaanPlot(model = fit, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), coefs = T, stand = T, covs = T, 
           stars = TRUE)



