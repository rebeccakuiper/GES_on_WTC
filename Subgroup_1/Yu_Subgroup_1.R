
if (!require("lavaan")) install.packages("lavaan") # install this package first (once)
library(lavaan)

if (!require("lavaanPlot")) install.packages("lavaanPlot") # install this package first (once)
library(lavaanPlot)


lower <- ' 
            1,
           -.367, 1,
           .504, -.491, 1,
           .182, -.285, .233, 1,
           .264, -.237, .351, .451, 1
           .246, -.358, .412, .464, .389, 1
           .140, -.202, .186, .339, .331, .445, 1
           .148, -.202, .202, .258, .293, .277, .137, 1
'

means <- c(60.85, 74.09, 68.57, 14.52, 9.47, 14.49, 11.32, 119.02)

Var <- c(546.811456, 162.588001, 251.127409, 9.333025, 6.007401, 12.1801, 5.396329, 229.8256)
G <- diag(Var)
invG <- solve(G)


#k <- 8 # aantal variabelen
corrmx <- 
  getCov(lower, names = c("WTC_E", "PRCA_E", 
                          "SPCC_E", "Integrativeness",
                          "Attitudes", "Motivation", "Instrumental", "TI"))
covmx <- invG %*% corrmx %*% invG
colnames(covmx) = rownames(covmx) = c("WTC_E", "PRCA_E", 
                                      "SPCC_E", "Integrativeness",
                                      "Attitudes", "Motivation", "Instrumental", "TI")
covmx # Check with corr matrix in article

# Nw was typefout: Intrumental 
SEM.model <- '
  # regressions
  WTC_E ~ Motivation + SPCC_E + TI + PRCA_E
  SPCC_E ~ PRCA_E + TI
  Motivation ~ TI + Attitudes + Integrativeness + Instrumental 
  PRCA_E ~ TI
'

fit <- sem(SEM.model, 
           sample.cov = covmx, 
           sample.nobs = 235) # sample size van de study!
summary(fit, standardized = TRUE)

# Nu wel warning, maar laten we ws dan ook gaan:
#Warning message:
#  In lav_model_vcov(lavmodel = lavmodel, lavsamplestats = lavsamplestats,  :
#                      lavaan WARNING:
#                      The variance-covariance matrix of the estimated parameters (vcov)
#                    does not appear to be positive definite! The smallest eigenvalue
#                    (= 5.206296e-14) is close to zero. This may be a symptom that the
#                    model is not identified.

#Plot
lavaanPlot(model = fit, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), coefs = T, stand = T, covs = T, 
           stars = TRUE)



