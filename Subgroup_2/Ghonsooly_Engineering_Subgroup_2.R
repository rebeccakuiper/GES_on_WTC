
if (!require("lavaan")) install.packages("lavaan") # install this package first (once)
library(lavaan)

if (!require("lavaanPlot")) install.packages("lavaanPlot") # install this package first (once)
library(lavaanPlot)

# Als lower trinagular matrix, dan:
#https://lavaan.ugent.be/tutorial/cov.html

# Nu upper triangular
lower <- c(1,
           .64, 1,
           -.38, -.48, 1,
           .39, .44, -.28, 1,
           .41, .39, -.33, .50, 1,
           .27, .18, -.11, .40, .35, 1,
           .31, .32, -.27, .35, .52, .30, 1,
           .17, .14, -.02, .38, .13, .21, .07, 1,
           .39, .33, -.30, .46, .40, .33, .23, .20, 1)

# Zorg wel voor een 1 op de diagonaal


covmx <- 
  getCov(lower, names = c("WTC", "PCC",
                          "ANX", "IFO",
                          "AAT", "IVA",
                          "IFA", "OTE", 
                          "MOT"))


covmx # Check with corr matrix in article


SEM.model <- '
    # latent variables
  IP =~ NA*IFA + AAT + IVA + IFO
  #CON =~ NA*PCC + ANX
  IP ~~ 1*IP
  #CON ~~ 1*CON 
  
  
  # regressions
  #WTC ~ IP + CON
  WTC ~ c1*IP + a1*ANX + a2*PCC + b*MOT
  #CON ~ MOT 
  ANX ~ b*MOT 
  PCC ~ b*MOT
  MOT ~ IP
  
  # indirect Effect (a*b)
  c := c1
  a1b := a1*b
  a2b := a2*b
  
   # correlated residuals # dit moet je ook uit path model halen!
  IP ~~ OTE
'

fit <- sem(SEM.model, 
           sample.cov = covmx, 
           sample.nobs = 80) # sample size van de study!
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



