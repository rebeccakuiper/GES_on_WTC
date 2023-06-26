
if (!require("lavaan")) install.packages("lavaan") # install this package first (once)
library(lavaan)

if (!require("lavaanPlot")) install.packages("lavaanPlot") # install this package first (once)
library(lavaanPlot)

# Als lower trinagular matrix, dan:
#https://lavaan.ugent.be/tutorial/cov.html

# Nu upper triangular
lower <- c(1, 
           .896, 1,
           .803, .600, 1,
           .850, .678, .607, 1,
           .864, .695, .610, .688, 1,
           .690, .667, .548, .545, .612, 1,
          -.596, -.609, -.509, -.405, -.454, -.658, 1,           
           .515, .489, .460, .452, .414, .559, -.553, 1)
           
           
           

# Zorg wel voor een 1 op de diagonaal


covmx <- 
  getCov(lower, names = c("WTCtot", "WTCS",
                          "WTCL", "WTCR",
                          "WTCW", "PCC", 
                          "LA", "Mot")) 


covmx # Check with corr matrix in article


SEM.model <- '
    # latent variables 
    WTCtot =~ NA*WTCL + WTCS + WTCR + WTCW
    WTCtot ~~ 1*WTCtot
    
    # regressions
    WTCtot ~ PCC + LA + Mot
  
'

fit <- sem(SEM.model, 
           sample.cov = covmx, 
           sample.nobs = 175) # sample size van de study!
summary(fit, standardized = TRUE)


# Plot
lavaanPlot(model = fit, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), coefs = T, stand = T, covs = T, 
           stars = TRUE)



