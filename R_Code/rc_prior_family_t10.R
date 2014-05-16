# clear workspace
rm(list = ls())

# set working directory
setwd("~/Dropbox/Projects/Need")

# load packages
library(arm)
library(compactr)
library(R2jags)
library(devEMF)


d <- read.csv("Data/politics_and_need_rescale.csv")

################################################################################
## Estimate the Models
################################################################################

f <- oppose_expansion ~ gop_governor + percent_favorable_aca + gop_leg + percent_uninsured + 
  income + percent_nonwhite + percent_metro

# cauchy prior + mcmc
#y <- rep(NA, 50)
y <- d$oppose_expansion
X <- model.matrix(f, d)
n <- length(y)
K <- ncol(X)
scales <- c(.5, 1, 2.5, 5)
jags.data <- list("y", "X", "n", "K", "scale")
jags.params <- "beta"
jags.inits <- function() {
  list("beta" = rnorm(ncol(X), 0, 3))
}
m <- list()
mcmc.sims <- list()
for (i in 1:length(scales)) {
  scale <- scales[i]
  m[[i]] <- jags(data = jags.data,
                 param = jags.params,
                 inits = jags.inits,
                 DIC = FALSE,
                 model = "R_Code/t10_logit.bugs",
                 n.chains = 3,
                 n.iter = 10000)
  mcmc.sims[[i]] <- m[[i]]$BUGSoutput$sims.matrix
}

var.names <- c('GOP Governor', 'Percent Favorable to ACA', 'GOP Controlled Legislature', 'Percent Uninsured',
               'Income', 'Percent Nonwhite', 'Percent Metropolitan', 'Constant')
var.order <- c(2:8, 1)

tiff("Figures/rc_prior_family_t10.tiff", height = 4, width = 10, units = "in", res = 300, family = "serif")
par(mfrow = c(2,4), oma = c(3,1,2,1), mar = c(.75, .75, 1, .5), family = "serif")
eplot(xlim = c(-6, 12), ylim = c(length(scales), .5),
      anny = FALSE, xlab = "Logit Coefficient",
      main = var.names[1])
abline(v = 0, lty = 3, col = "grey50")
for (j in 1:length(scales)) {
  q <- quantile(mcmc.sims[[j]][, var.order[1]], c(0.05, .5, .95))
  #print(q)
  points(q[2], j, pch = 19)
  lines(c(q[1], q[3]), c(j, j))
  text(q[2], j, paste("scale = ", scales[j], sep = ""), pos = 3, cex = .9)
}

for (i in 2:8) {
  aplot(var.names[i])
  abline(v = 0, lty = 3, col = "grey50")
  for (j in 1:length(scales)) {
    q <- quantile(mcmc.sims[[j]][, var.order[i]], c(0.05, .5, .95))
    #print(q)
    points(q[2], j, pch = 19)
    lines(c(q[1], q[3]), c(j, j))
    multiple <- 1
    if (i == 8) multiple <- 10/scales[j]
    text(q[2], j, paste("scale = ", multiple*scales[j], sep = ""), pos = 3, cex = .9)
  }
}
dev.off()