
if (!require("lavaan")) install.packages("lavaan") # install this package first (once)
library(lavaan)

if (!require("lavaanPlot")) install.packages("lavaanPlot") # install this package first (once)
library(lavaanPlot)

# Als lower trinagular matrix, dan:
#https://lavaan.ugent.be/tutorial/cov.html

# Nu upper triangular
lower <- c(1,
           -.28, 1,
           .09, -.13, 1,
           .12, -.21, .31, 1,
           -.28, .33, -.07, -.19, 1,
           .57, -.29, .10, .10, -.46, 1,
           .34, -.24, .21, .24, -.20, .34, 1,
           .28, -.25, .19, .14, -.15, .25, .47, 1,
           .39, -.20, .19, .15, -.20, .36, .52, .71, 1)

# Zorg wel voor een 1 op de diagonaal


#k <- 9 # aantal variabelen
covmx <- 
  getCov(lower, names = c("NVTI", "VTI",
                          "CA", "SPCC",
                          "MI", "ALE",
                          "DTLE", "L2WTC", "Shyness"))
covmx # Check with corr matrix in article



SEM.model <- '
  # latent variables
  Immediacy =~ NA*NVTI + VTI
  #Confidence =~ NA*CA + SPCC
  Motivation =~ NA*MI + ALE + DTLE
  #Confidence ~~ 1*Confidence
  Immediacy ~~ 1*Immediacy 
  Motivation ~~ 1*Motivation
  
  # regressions
  #L2WTC ~ Confidence + Motivation
  L2WTC ~ CA + SPCC + Motivation
  #Confidence ~ Motivation + Shyness
  CA ~ Motivation + Shyness  
  SPCC ~ Motivation + Shyness
  Motivation ~ Shyness + Immediacy
  Shyness ~ Immediacy
'

fit <- sem(SEM.model, 
           sample.cov = covmx, 
           sample.nobs = 252) # sample size van de study!
summary(fit, standardized = TRUE)

# Nu wel warning, maar laten we ws dan ook gaan:
#Warning message:
#  In lavaan::lavaan(model = SEM.model, sample.cov = covmx, sample.nobs = 252,  :
#                      lavaan WARNING:
#                      the optimizer (NLMINB) claimed the model converged, but not all
#                    elements of the gradient are (near) zero; the optimizer may not
#                    have found a local solution use check.gradient = FALSE to skip
#                    this check.
                    

#Plot
lavaanPlot(model = fit, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), coefs = T, stand = T, covs = T, 
           stars = TRUE)



