
if (!require("lavaan")) install.packages("lavaan") # install this package first (once)
library(lavaan)

if (!require("lavaanPlot")) install.packages("lavaanPlot") # install this package first (once)
library(lavaanPlot)

# Als lower trinagular matrix, dan:
#https://lavaan.ugent.be/tutorial/cov.html

# Nu upper triangular
lower <- c(1,
           .98, 1,
           .98, .91, 1,
           -.25, -.25, -.23, 1,
           .53, .49, .54, -.26, 1,
           .39, .39, .37, -.23, .25, 1,
           .26, .25, .26, -.23, .24, .71, 1,
           .38, .38, .37, -.25, .24, .43, .46, 1,
           .33, .36, .29, -.16, .25, .39, .36, .52, 1, 
           .27, .27, .25, -.15, .19, .25, .32, .26, .28, 1,
           .43, .43, .41, -.23, .24, .37, .29, .40, .22, .29, 1, 
           .05, .04, .06, -.15, .04, .04, .02, -.01, -.05, .04, .02, 1)

# Zorg wel voor een 1 op de diagonaal


covmx <- 
  getCov(lower, names = c("WTC_L2", "WTC_L21",
                          "WTC_L22", "CA",
                          "PCC", "MI",
                          "DTL2", "IAT", 
                          "IIV", "IFA", "Freq", 
                          "TOEFL"))


covmx # Check with corr matrix in article


SEM.model <- '
  # latent variables
  WTC_L2 =~ NA*WTC_L21 + WTC_L22 
  Mot =~ NA*MI + DTL2
  #CC =~ NA*CA + PCC
  International =~ NA*IFA + IAT + IIV
  Freq =~ NA*TOEFL
  WTC_L2 ~~ 1*WTC_L2
  Mot ~~ 1*Mot 
  #CC ~~ 1*CC 
  International ~~ 1*International
  Freq ~~ 1*Freq

  # regressions
  #WTC_L2 ~ a*CC + c1*International
  WTC_L2 ~ a1*CA + a2*PCC + c1*International + b*Mot
  #CC ~ b*Mot
  CA ~ b*Mot
  PCC ~ b*Mot
  Mot ~ International
  Freq ~ International + WTC_L2

  # indirect Effect (a*b)
  c := c1
  a1b := a1*b
  a2b := a2*b

'

fit <- sem(SEM.model, 
           sample.cov = covmx, 
           sample.nobs = 154) # sample size van de study!
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



