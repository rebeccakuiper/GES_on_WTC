
if (!require("lavaan")) install.packages("lavaan") # install this package first (once)
library(lavaan)

if (!require("lavaanPlot")) install.packages("lavaanPlot") # install this package first (once)
library(lavaanPlot)

# Als lower trinagular matrix, dan:
#https://lavaan.ugent.be/tutorial/cov.html

  
# Nu lower triangular
lower <- c(1,
           0.42, 1,
           0.36, 0.35, 1,
           -0.20, -0.28, -0.05, 1,
           -0.23, -0.34, -0.48, 0.25, 1,
           0.14, 0.26, 0.35, -0.20, -0.31, 1,
           0.27, 0.28, 0.41, -0.26, -0.37, 0.41, 1,
           -0.24, -0.17, -0.29, 0.03, 0.11, -0.39, -0.35, 1,
           -0.29, -0.23, -0.31, 0.11, 0.17, -0.27, 0.29, 0.15, 1,
           0.25, 0.18, 0.36, -0.17, -0.32, 0.38, 0.41, -0.26, -0.29, 1,
           0.26, 0.26, 0.29, -0.26, -0.35, 0.43, 0.37, -0.31, -0.27, 0.43, 1,
           0.34, 0.35, 0.41, -0.26, -0.23, 0.25, 0.24, -0.38, -0.31, 0.51, 0.66, 1)

# Zorg wel voor een 1 op de diagonaal, als lower dan is dat anders dan bij upper!


#k <- 12 # aantal variabelen
covmx <- 
  getCov(lower, names = c("CEF", "CAF", "IMC", 
                          "JRC", "ERC", "SPC_W", 
                          "SPC_PG", "CA_W", "CA_PG", 
                          "WTC_W", "WTC_PG", "AC"))
covmx # Check with corr matrix in article


SEM.model <- '
  # latent variables
  MC =~ NA*IMC + JRC + ERC
  CSE =~ NA*CAF + CEF
  SPC =~ NA*SPC_W + SPC_PG 
  CA =~ NA*CA_W + CA_PG
  WTC =~ NA*WTC_W + WTC_PG
  MC ~~ 1*MC
  CSE ~~ 1*CSE 
  SPC ~~ 1*SPC 
  CA ~~ 1*CA
  WTC ~~ 1*WTC
  
  # regressions
  AC ~ WTC
  WTC ~ MC + CA + SPC
  MC ~ SPC 
  SPC ~ CSE 
'
#CA ~ SPC - eruit gehaald
#MC ~ SPC + CSE - laatste er uit gehaald

fit <- sem(SEM.model, 
           sample.cov = covmx, 
           sample.nobs = 107) # sample size van de study!
summary(fit, standardized = TRUE)


#Plot
lavaanPlot(model = fit, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), coefs = T, stand = T, covs = T, 
           stars = TRUE)



