
if (!require("lavaan")) install.packages("lavaan") # install this package first (once)
library(lavaan)

if (!require("lavaanPlot")) install.packages("lavaanPlot") # install this package first (once)
library(lavaanPlot)

# Als lower trinagular matrix, dan:
#https://lavaan.ugent.be/tutorial/cov.html

# Nu upper triangular
lower <- c(1,
           .50, 1,
           -.36, -.23, 1,
           .46, .28, -.46, 1,
           .12, .11, -.11, .16, 1,
           .35, .26, -.20, .37, .47, 1,
           .31, .27, -.23, .32, .22, .51, 1,
           .33, .25, -.24, .26, -.01, .21, .17, 1,
           .27, .22, -.23, .21, .01, .10, .06, .45, 1, 
           .27, .25, -.16, .20, .11, .20, .14, .09, .29, 1,
           .27, .23, -.17, .27, .14, .23, .15, .15, .23, .42, 1, 
           .33, .23, -.17, .31, .17, .23, .17, .07, .20, .60, .45, 1)

# Zorg wel voor een 1 op de diagonaal


covmx <- 
  getCov(lower, names = c("WTCMF", "WTCFF",
                          "CA", "PC",
                          "ER", "IR",
                          "IM", "TS", 
                          "SC", "TO", "BAEL", 
                          "BACC"))


covmx # Check with corr matrix in article


SEM.model <- '
    # latent variabeles
  CE =~ NA*SC + TS + TO
  LB =~ NA*BACC + BAEL 
  Mot =~ NA*ER + IR + IM 
  #CC =~ NA*CA + PC 
  WTC =~ NA*WTCMF + WTCFF
  CE ~~ 1*CE
  LB ~~ 1*LB 
  Mot ~~ 1*Mot 
  #CC ~~ 1*CC
  WTC ~~ 1*WTC
  
  #regressions 
  #WTC ~ CC + CE + Mot
  WTC ~ a1*CA + a2*PC + c1*CE + b*Mot
  #CC ~ CE + Mot + LB
  CA ~ CE + b*Mot + LB
  PC ~ CE + b*Mot + LB
  Mot ~ CE
  LB ~ CE
  
  # indirect Effect (a*b)
  c := c1
  a1b := a1*b
  a2b := a2*b
'

fit <- sem(SEM.model, 
           sample.cov = covmx, 
           sample.nobs = 330) # sample size van de study!
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



