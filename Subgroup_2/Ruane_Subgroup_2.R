
if (!require("lavaan")) install.packages("lavaan") # install this package first (once)
library(lavaan)

if (!require("lavaanPlot")) install.packages("lavaanPlot") # install this package first (once)
library(lavaanPlot)

# Als lower trinagular matrix, dan:
#https://lavaan.ugent.be/tutorial/cov.html

# Nu upper triangular
lower <- c(1,
           -.59, 1,
           .72, -.79, 1,
           .12, -.06, .07, 1,
           .15, -.08, .24, .25, 1,
           .21, -.23, .25, .21, .33, 1,
           .27, -.12, .15, .21, .46, .30, 1,
           .26, -.23, .24, .09, .43, .23, .69, 1,
           .51, -.49, .48, .17, .12, .29, .33, .41, 1)

# Zorg wel voor een 1 op de diagonaal


covmx <- 
  getCov(lower, names = c("WTC", "CA",
                          "PC", "IFO",
                          "IVA", "IFA",
                          "MI", "DLE", 
                          "FREQ"))


covmx # Check with corr matrix in article


SEM.model <- '
    # latent variables
  IP =~ NA*IFA + IVA + IFO
  #CC =~ NA*PC + CA
  MOT =~ NA*DLE + MI
  IP ~~ 1*IP
  #CC ~~ 1*CC
  MOT ~~ 1*MOT
  
  
  # regressions
  #WTC ~ IP + CC + MOT
  WTC ~ c1*IP + a1*CA + a2*PC + b*MOT
  #CC ~ MOT 
  CA ~ b*MOT
  PC ~ b*MOT 
  MOT ~ IP
  FREQ ~ IP + WTC

  # indirect Effect (a*b)
  c := c1
  a1b := a1*b
  a2b := a2*b
'

fit <- sem(SEM.model, 
           sample.cov = covmx, 
           sample.nobs = 117) # sample size van de study!
summary(fit, standardized = TRUE)

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



