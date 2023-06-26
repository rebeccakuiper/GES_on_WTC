
if (!require("lavaan")) install.packages("lavaan") # install this package first (once)
library(lavaan)

if (!require("lavaanPlot")) install.packages("lavaanPlot") # install this package first (once)
library(lavaanPlot)

# Als lower trinagular matrix, dan:
#https://lavaan.ugent.be/tutorial/cov.html

# Nu upper triangular
lower <- c(1,
           .63, 1,
           .53, .40, 1,
           .81, .56, .38, 1,
           -.59, -.46, -.36, -.56, 1,
           .39, .33, .65, .35, -.33, 1,
           .49, .37, .84, .41, -.48, .64, 1,
           .46, .46, .15, .40, -.21, .20, .20, 1,
           .45, .25, .23, .45, -.26, .14, .19, .06, 1, 
           .48, .39, .33, .42, -.53, .43, .38, .34, .41, 1,
           .29, .32, .30, .25, -.08, .24, .12, .04, .33, .28, 1, 
           .36, .32, .45, .27, -.21, .37, .32, .09, .26, .35, .47, 1,
           .30, .24, .30, .23, -.11, .35, .20, .06, .43, .41, .52, .44, 1)

# Zorg wel voor een 1 op de diagonaal


covmx <- 
  getCov(lower, names = c("Frequency_of_communication", "WTC",
                          "Mot", "PCC",
                          "LA", "Attitudes",
                          "Integrativeness", "Social_context", 
                          "Intellect", "Extraversion", "Agreeableness", 
                          "Emotional_stability", "Conscientiousness"))


covmx # Check with corr matrix in article


SEM.model <- '
    # regressions
  Frequency_of_communication ~ WTC + Mot + Social_context
  WTC ~ PCC + LA + Social_context + Agreeableness + Mot #Mot nu zelf toegevoegd (Subgroep 3)
  Mot ~ Attitudes + Integrativeness
  PCC ~ Intellect + Social_context + LA
  LA ~ Extraversion
  Integrativeness ~ Emotional_stability + LA
  Attitudes ~ Conscientiousness + Integrativeness
  
'

fit <- sem(SEM.model, 
           sample.cov = covmx, 
           sample.nobs = 92) # sample size van de study!
summary(fit, standardized = TRUE)


# Plot
lavaanPlot(model = fit, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), coefs = T, stand = T, covs = T, 
           stars = TRUE)



