
if (!require("lavaan")) install.packages("lavaan") # install this package first (once)
library(lavaan)

if (!require("lavaanPlot")) install.packages("lavaanPlot") # install this package first (once)
library(lavaanPlot)

# Als lower trinagular matrix, dan:
#https://lavaan.ugent.be/tutorial/cov.html

# Nu upper triangular
upper <- c(1, .59, .48, .26, .11, .11, .24, .36, .37, .01, .04, .16, .05,
           1, .58, .33, .17, .10, .28, .40, .31, .03, .09, .16, .06, 
           1, .40, .26, .12, .43, .41, .34, .00, .12, .20, .12, 
           1, .34, .36, .43, .38, .28, -.11, .26, .23, .29, 
           1, .33, .40, .32, .24, -.02, .16, .13, .22, 
           1, -.24, .20, .15, -.06, .09, .10, .12, 
           1, .43, .28, .01, .15, .17, .17,
           1, .57, .03, .16, .22, .17, 
           1, -.08, .12, .23, .17, 
           1, -.10, -.10, -.08,
           1, .74, .58,
           1, .60,
           1)

# Zorg wel voor een 1 op de diagonaal


k <- 13 # aantal variabelen
corrmx <- matrix(NA, nrow=k, ncol = k)
corrmx[lower.tri(corrmx,diag=TRUE)] <- upper
corrmx <- t(corrmx)
corrmx[lower.tri(corrmx,diag=TRUE)] <- upper
#covmx <- invG %*% corrmx %*% invG # Als geen stand errors / varianties, dan deze niet!
covmx <- corrmx
colnames(covmx) = rownames(covmx) = c("MI", "MD", 
                                      "MA", "AAT",
                                      "VA", "FA",
                                      "FO", "WTC",
                                      "PC", "CA", 
                                      "PE1", "PE2", 
                                      "PE3")
covmx # Check with corr matrix in article



SEM.model <- '
  # latent variables
  Mot =~ NA*MI + MA + MD
  Per =~ NA*PE1 + PE2 + PE3
  #Conf =~ NA*CA + PC
  Att =~ NA*VA + FA + AAT + FO
  #L2wtc =~ NA*WTC
  L2wtc =~ WTC
  Mot ~~ 1*Mot
  Per ~~ 1*Per 
  #Conf ~~ 1*Conf
  Att ~~ 1*Att
  #L2wtc ~~ 1*L2wtc 
  
  # regressions
  #L2wtc ~ Att + Mot + Conf
  L2wtc ~ Att + Mot + CA + PC
  #Conf ~ Per
  CA ~ Per
  PC ~ Per
  Mot ~ Att
'

fit <- sem(SEM.model, 
           sample.cov = covmx, 
           #sample.mean = means, # als er geen means zijn, dan deze niet...
           sample.nobs = 304) # sample size van de study!
summary(fit, standardized = TRUE)

# Nu krijg je een warning: 
#Warning message:
#  In lav_object_post_check(object) :
#  lavaan WARNING: some estimated ov variances are negative
# maar op zich runt het model. 
# Is alleen wat gek om negatieve varianties te hebben, maar gaan wij nu niet oplossen.

# Plot
lavaanPlot(model = fit, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), coefs = T, stand = T, covs = T, 
           stars = TRUE)



