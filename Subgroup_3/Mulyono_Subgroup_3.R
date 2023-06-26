
if (!require("lavaan")) install.packages("lavaan") # install this package first (once)
library(lavaan)

if (!require("lavaanPlot")) install.packages("lavaanPlot") # install this package first (once)
library(lavaanPlot)

# Als lower trinagular matrix, dan:
#https://lavaan.ugent.be/tutorial/cov.html

# Nu upper triangular
upper <- c(1, .137, .221, -.05, .022, .092, .047, .082, .048,
           1, .865, .115, .008, .085, .206, .132, .064, 
           1, .083, .092, .072, .192, .134, .104,
           1, -.340, .412, .442, .369, .335, 
           1, -.373, -.152, -.060, -.040,
           1, .491, .407, .403, 
           1, .717, .503,
           1, .584,
           1)

# Zorg wel voor een 1 op de diagonaal


k <- 9 # aantal variabelen
corrmx <- matrix(NA, nrow=k, ncol = k)
corrmx[lower.tri(corrmx,diag=TRUE)] <- upper
corrmx <- t(corrmx)
corrmx[lower.tri(corrmx,diag=TRUE)] <- upper
#covmx <- invG %*% corrmx %*% invG # Als geen stand errors / varianties, dan deze niet!
covmx <- corrmx
colnames(covmx) = rownames(covmx) = c("Gender", "Age",
                          "Edu", "SC",
                          "SA", "Mot",
                          "WTCIC", "WTCOC", 
                          "WTCDE")


covmx # Check with corr matrix in article


SEM.model <- '
    # latent variables
    WTC =~ WTCIC + WTCOC + WTCDE
    
    # regressions
    WTC ~ SA + Mot + SC 
  
  
'

fit <- sem(SEM.model, 
           sample.cov = covmx, 
           sample.nobs = 92) # sample size van de study!
summary(fit, standardized = TRUE)


# Plot
lavaanPlot(model = fit, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), coefs = T, stand = T, covs = T, 
           stars = TRUE)



