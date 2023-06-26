
if (!require("lavaan")) install.packages("lavaan") # install this package first (once)
library(lavaan)

if (!require("lavaanPlot")) install.packages("lavaanPlot") # install this package first (once)
library(lavaanPlot)

# Als lower trinagular matrix, dan:
#https://lavaan.ugent.be/tutorial/cov.html

# Nu upper triangular
lower <- c(1,
           -.29, 1,
           .04, .19, 1,
           -.44, .17, .45, 1,
           -.54, .28, .22, .03, 1)

# Zorg wel voor een 1 op de diagonaal


covmx <- 
  getCov(lower, names = c("Anx_Fr", "PC_Fr",
                          "Freq_Fr", "WTC_Fr", 
                          "AMI"))


covmx # Check with corr matrix in article


SEM.model <- '
    # regressions
  WTC_Fr ~ Anx_Fr + AMI + Freq_Fr + PC_Fr 
  
'

fit <- sem(SEM.model, 
           sample.cov = covmx, 
           sample.nobs = 71) # sample size van de study!
summary(fit, standardized = TRUE)


# Plot
lavaanPlot(model = fit, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), coefs = T, stand = T, covs = T, 
           stars = TRUE)



