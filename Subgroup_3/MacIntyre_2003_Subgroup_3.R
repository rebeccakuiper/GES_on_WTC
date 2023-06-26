if (!require("lavaan")) install.packages("lavaan") # install this package first (once)
library(lavaan)

if (!require("lavaanPlot")) install.packages("lavaanPlot") # install this package first (once)
library(lavaanPlot)

# Als lower trinagular matrix, dan:
#https://lavaan.ugent.be/tutorial/cov.html

# Nu upper triangular
lower <- c(1,
           -.307, 1,
           -.226, .624, 1,
           .220, -.291, -.309, 1,
           .213, -.221, -.351, .547, 1,
           .288, -.354, -.424, .411, .360, 1,
           .187, -.268, -.344, .200, .474, .442, 1,
           .293, -.205, -.350, .070, .232, .408, .498, 1)

# Zorg wel voor een 1 op de diagonaal


covmx <- 
  getCov(lower, names = c("AMI", "CAE",
                          "LAF", "PCE",
                          "PCF", "WTCE",
                          "WTCF", "FREQ"))


covmx # Check with corr matrix in article


SEM.model <- '
    # regressions
  WTCF ~ PCF + LAF + AMI 
'

fit <- sem(SEM.model, 
           sample.cov = covmx, 
           sample.nobs = 268) # sample size van de study!
summary(fit, standardized = TRUE)


# Plot
lavaanPlot(model = fit, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), coefs = T, stand = T, covs = T, 
           stars = TRUE)





