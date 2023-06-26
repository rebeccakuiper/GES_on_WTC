
if (!require("lavaan")) install.packages("lavaan") # install this package first (once)
library(lavaan)

if (!require("lavaanPlot")) install.packages("lavaanPlot") # install this package first (once)
library(lavaanPlot)

# Als lower trinagular matrix, dan:
#https://lavaan.ugent.be/tutorial/cov.html

# Nu upper triangular
lower <- c(1,
           -.39, 1,
           .56, -.32, 1,
           .16, -.19, .16, 1,
           .13, -.10, .17, .31, 1,
           .19, -.13, .21, .28, .52, 1,
           .41, -.31, .29, .18, .32, .24, 1,
           .41, -.23, .27, .12, .28, .20, .77, 1,
           .41, -.19, .22, .13, .20, .07, .60, .61, 1, 
           .37, -.23, .20, .13, .16, .14, .57, .49, .65, 1,
           .24, -.12, .13, .11, .11, .08, .24, .27, .31, .32, 1, 
           .40, -.28, .22, .07, .16, .10, .54, .50, .65, .61, .24, 1)

# Zorg wel voor een 1 op de diagonaal


covmx <- 
  getCov(lower, names = c("WTC", "CA",
                          "PCC", "Listening",
                          "Gram_Vocab", "Reading",
                          "MI", "DLE", 
                          "IFO", "IIV", "IFA", 
                          "IAAT"))


covmx # Check with corr matrix in article


SEM.model <- '
     # latent variables
  Inter_Posture =~ NA*IFO + IIV + IFA + IAAT
  LM =~ NA*MI + DLE
  #CC =~ NA*CA + PCC
  Prof =~ NA*Listening + Gram_Vocab + Reading
  Inter_Posture ~~ 1*Inter_Posture
  LM ~~ 1*LM 
  #CC ~~ 1*CC 
  Prof ~~ 1*Prof
  
  
  # regressions
  #WTC ~ a*CC + c1*Inter_Posture
  WTC ~ a1*CA + a2*PCC + c1*Inter_Posture
  #CC ~ b*LM + Prof
  CA ~ b*LM + Prof
  PCC ~ b*LM + Prof
  LM ~ Inter_Posture
  
  # indirect Effect (a*b)
  c := c1
  a1b := a1*b
  a2b := a2*b

'

# Nu zeg je met:
#  CA ~ b*LM + Prof
# PCC ~ b*LM + Prof
#Dat de realtie tussen CA met LM en tussen PCC en LM gelijk zijn, 
#ik denk niet dat je dat wilt.
# Ik vermoed dat je b1 en b2 wilt en dan die combineren met a1 en a2:
SEM.model <- '
  # latent variables
  Inter_Posture =~ NA*IFO + IIV + IFA + IAAT
  LM =~ NA*MI + DLE
  #CC =~ NA*CA + PCC
  Prof =~ NA*Listening + Gram_Vocab + Reading
  Inter_Posture ~~ 1*Inter_Posture
  LM ~~ 1*LM 
  #CC ~~ 1*CC 
  Prof ~~ 1*Prof
  
  
  # regressions
  #WTC ~ a*CC + c1*Inter_Posture
  WTC ~ a1*CA + a2*PCC + c1*Inter_Posture
  #CC ~ b*LM + Prof
  CA ~ b1*LM + Prof
  PCC ~ b2*LM + Prof
  LM ~ Inter_Posture
  
  # indirect Effect (a*b)
  c := c1
  a1b1 := a1*b1
  a2b2 := a2*b2

'
# HIERONDER ook aagepast nu naar a1b1 en a2b2!
# Hieronder ook 34:36 gedaan (er stond nog 34:35), je wilt immers alle 3 bekijken.
# Dat laatste lost ook de error bij de plot op!


fit <- sem(SEM.model, 
           sample.cov = covmx, 
           sample.nobs = 297) # sample size van de study!
summary(fit, standardized = TRUE)
lavInspect(fit, "vcov.def") # geeft cov. matrix van ab en c

fit.std <- standardizedSolution(fit)
which(fit.std[, 'label'] == "c")
which(fit.std[, 'label'] == "a1b1")
which(fit.std[, 'label'] == "a2b2")
indices <- 34:36 # Dit obv bovenstaande bepaald, dus per study ws aanpassen!
est <- fit.std[indices, 'est.std']
VCOV <- lavInspect(fit, "vcov.def.std.all") # geeft cov. matrix van ab en c



# Plot
lavaanPlot(model = fit, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), coefs = T, stand = T, covs = T, 
           stars = TRUE)




