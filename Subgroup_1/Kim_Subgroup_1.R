
if (!require("lavaan")) install.packages("lavaan") # install this package first (once)
library(lavaan)

if (!require("lavaanPlot")) install.packages("lavaanPlot") # install this package first (once)
library(lavaanPlot)

# Als lower trinagular matrix, dan:
#https://lavaan.ugent.be/tutorial/cov.html

# Nu upper triangular
upper <- c(1, .28, .19, .31, .27, .29, .18, -.11, .21, .013,
1, .42, .57, .42, .52, .47, -.21, .19, .20, 
1, .49, .74, .46, .73, -.27, .34, .29, 
1, .50, .58, .55, -.21, .32, .22, 
1, .56, .74, -.21, .39, .29, 
1, .60, -.26, .25, .18, 
1, -.25, .28, .22, 
1, -.17, -.20, 
1, .44,
1)

# Zorg wel voor een 1 op de diagonaal

means <- c(4.17, 5.27, 3.90, 4.52, 3.80, 4.92, 5.14, 3.70, 2.56, 1.39)

Var <- c(1.87, 0.89, 1.20, 1.26, 1.32, 1.47, 1.20, 2.63, 2.19, 1.87)
G <- diag(Var)
invG <- solve(G)




k <- 10 # aantal variabelen
corrmx <- matrix(NA, nrow=k, ncol = k)
corrmx[lower.tri(corrmx,diag=TRUE)] <- upper
corrmx <- t(corrmx)
corrmx[lower.tri(corrmx,diag=TRUE)] <- upper
covmx <- invG %*% corrmx %*% invG
colnames(covmx) = rownames(covmx) = c("IFA", "ATT", 
                          "MI", "IVA",
                          "DLE", "IFO",
                          "ALE", "CA",
                          "PC", "WTC")
covmx # Check with corr matrix in article


# In plot AAT, is ws ATT

SEM.model <- '
  # latent variables
  A =~ NA*IFO + IVA + IFA + ATT
  ELM =~ NA*MI + DLE + ALE
  #CIEC =~ NA*CA + PC
  #WTC_in_L2 =~ NA*WTC
  WTC_in_L2 =~ WTC
  A ~~ 1*A
  ELM ~~ 1*ELM 
  #CIEC ~~ 1*CIEC 
  #WTC_in_L2 ~~ 1*WTC_in_L2 
  
  # regressions
  #WTC_in_L2 ~ A + ELM + CIEC
  WTC_in_L2 ~ A + ELM + CA + PC
  #CIEC ~ ELM
  CA ~ ELM 
  PC ~ ELM
  ELM ~ A
  
  # correlated residuals # dit moet je ook uit path model halen!
  #ELM ~~ CIEC # obv theorie er in
  ELM ~~ CA + PC # obv theorie er in
  IVA ~~ ATT # Deze niet standaard - alleen als fit slecht 
  DLE ~~ ALE # Deze niet standaard - alleen als fit slecht 
'

fit <- sem(SEM.model, 
           sample.cov = covmx, 
           sample.mean = means,
           sample.nobs = 191) # sample size van de study!
summary(fit, standardized = TRUE)


# Plot
lavaanPlot(model = fit, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), coefs = T, stand = T, covs = T, 
           stars = TRUE)



