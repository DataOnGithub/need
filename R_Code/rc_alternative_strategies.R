# clear workspace
rm(list = ls())
#dev.off()

# set seed
set.seed(87125081)

# set working directory
setwd("~/Dropbox/Projects/Need")

# load packages
library(arm)
library(compactr)
library(brglm)
library(logistf)
library(R2jags)
library(devEMF)

d <- read.csv("Data/politics_and_need_rescale.csv")

################################################################################
## Estimate the Models
################################################################################


f <- oppose_expansion ~ gop_governor + percent_favorable_aca + gop_leg + percent_uninsured + 
  bal2012 + multiplier + percent_nonwhite + percent_metro

# maximum likelihood
m1 <- glm(f, family = binomial, data = d)

# jeffreys prior
#m.jeffreys <- brglm(f, family = binomial, data = d)
m2 <- logistf(f, alpha = 0.1, data = d)
#plot(profile(m.jeffreys, variable = "gop_governor"))

# cauchy prior + asymptotic
m4 <- bayesglm(f, family = binomial, data = d, scale = 2.5)

# firth's penalty + bootstrap
n.bs <- 1000
m5 <- matrix(NA, nrow = n.bs, ncol = length(all.vars(f))) 
pb <- txtProgressBar(min = 0, max = n.bs, style = 3)
for (bs.iter in 1:n.bs) {
  bs.samp <- sample(1:nrow(d), nrow(d), replace = TRUE)
  bs.data <- d[bs.samp, ]
  m5[bs.iter, ] <- coef(logistf(f, alpha = 0.1, data = bs.data))
  setTxtProgressBar(pb, bs.iter)
} 
colnames(m5) <- names(coef(m2))
rm(n.bs, bs.samp, bs.data)

# cauchy prior + bootstrap
n.bs <- 1000
m6 <- matrix(NA, nrow = n.bs, ncol = length(all.vars(f))) 
pb <- txtProgressBar(min = 0, max = n.bs, style = 3)
for (bs.iter in 1:n.bs) {
  bs.samp <- sample(1:nrow(d), nrow(d), replace = TRUE)
  bs.data <- d[bs.samp, ]
  m6[bs.iter, ] <- coef(bayesglm(f, family = binomial, data = bs.data, control = glm.control(epsilon = 1e-8, maxit = 10000)))
  setTxtProgressBar(pb, bs.iter)
} 
colnames(m6) <- names(coef(m2))
rm(n.bs, bs.samp, bs.data)

# cauchy prior + mcmc
#y <- rep(NA, 50)
y <- d$oppose_expansion
X <- model.matrix(f, d)
n <- length(y)
K <- ncol(X)
jags.data <- list("y", "X", "n", "K", "scale")
jags.params <- "beta"
jags.inits <- function() {
  list("beta" = rnorm(ncol(X), 0, 3))
}

scale <- 1
m7 <- jags(data = jags.data,
           param = jags.params,
           inits = jags.inits,
           DIC = FALSE,
           model = "R_Code/normal_logit.bugs",
           n.chains = 3,
           n.iter = 100000)
plot(m7)

scale <- 2.5
m8 <- jags(data = jags.data,
           param = jags.params,
           inits = jags.inits,
           DIC = FALSE,
           model = "R_Code/t10_logit.bugs",
           n.chains = 3,
           n.iter = 100000)
plot(m8)

scale <- 2.5
m9 <- jags(data = jags.data,
           param = jags.params,
           inits = jags.inits,
           DIC = FALSE,
           model = "R_Code/cauchy_logit.bugs",
           n.chains = 3,
           n.iter = 100000)
plot(m9)



################################################################################
## Plot the Estimates
################################################################################

model.names <- c("Maximum\nLikelihood", 
                 "Firth's Penalty\n+ Asymptotics", 
                 "Firth's Penalty\n+ Likelihood Profiling",
                 "Cauchy Prior\n+ Asymptotics",
                 "Firth's Penalty\n+ Bootstrap", 
                 "Cauchy Prior\n+ Bootstrap",
                 "Normal Prior\n+ MCMC",
                 "t Prior\n+ MCMC",
                 "Cauchy Prior\n+ MCMC"
                 )
n.models <- length(model.names)

var.names <- c('gop_governor', 'percent_favorable_aca', 'gop_leg', 'percent_uninsured',
               'bal2012', 'multiplier', 'percent_nonwhite', 'percent_metro')
bugs.coef.names <- c("beta[2]", "beta[3]", "beta[4]", "beta[5]", "beta[6]", 
                     "beta[7]", "beta[8]", "beta[9]")
label.var.names <- c('GOP Governor', 'Percent Favorable to ACA', 'GOP Controlled Legislature', 'Percent Uninsured',
                     'Fiscal Health', 'Medicaid Multiplier', 'Percent Nonwhite', 'Percent Metropolitan')
state.names <- sort(unique(d$state_abbr))

n.vars <- length(all.vars(f)) - 1
n.states <- length(state.names)

emf("Figures/rc_alternative_strategies.emf", height = 8, width = 10, family = "serif")
par(mfrow = c(2,4), oma = c(3,1,1,1), mar = c(.75, .75, 1, .5), family = "serif")
for (var.index in 1:n.vars) {
  eplot(xlim = c(-12, 18), ylim = c(-1, -9.5), main = label.var.names[var.index], anny = FALSE, 
        xlab = "Logit Coefficient")
  abline(v = 0, lty = 3)
 
  ### MLE
  est <- coef(m1)[var.names[var.index]]
  se <- sqrt(diag(vcov(m1)))[var.names[var.index]]
  lines(c(est + 1.64*se, est - 1.64*se), c(-1, -1), lty = 1.5)
  points(est, -1, pch = 19)
  xpos <- est
  if (abs(est) > 4) xpos <- 0
  text(xpos, -1, model.names[1], pos = 3)
  
  ### Firth's + Asymptotics
  est <- coef(m2)[var.names[var.index]]
  se <- sqrt(diag(vcov(m2)))
  names(se) <- names(coef(m2))
  se <- se[var.names[var.index]]
  lines(c(est + 1.64*se, est - 1.64*se), c(-2, -2), lty = 1.5)
  points(est, -2, pch = 19)
  text(est, -2, model.names[2], pos = 3)
  
  ### Firth's + Likelihood Profiling
  est <- coef(m2)[var.names[var.index]]
  lwr <- m2$ci.lower[var.names[var.index]]
  upr <- m2$ci.upper[var.names[var.index]]
  lines(c(lwr, upr), c(-3, -3), lty = 1.5)
  points(est, -3, pch = 19)
  text(est, -3, model.names[3], pos = 3)
  
  ### Cauchy + Asymptotics
  est <- coef(m4)[var.names[var.index]]
  se <- sqrt(diag(vcov(m4)))
  names(se) <- names(coef(m4))
  se <- se[var.names[var.index]]
  lines(c(est + 1.64*se, est - 1.64*se), c(-4, -4), lty = 1.5)
  points(est, -4, pch = 19)
  text(est, -4, model.names[4], pos = 3)
  
  ### Firths + Bootstrap
  q <- quantile(m5[, var.names[var.index]], c(.5, .05, .95))
  lines(q[2:3], c(-5, -5), lty = 1.5)
  points(q[1], -5, pch = 19)
  text(q[1], -5, model.names[5], pos = 3)

  ### Cauchy + Bootstrap
  q <- quantile(m6[, var.names[var.index]], c(.5, .05, .95))
  lines(q[2:3], c(-6, -6), lty = 1.5)
  points(q[1], -6, pch = 19)
  text(q[1], -6, model.names[6], pos = 3)
  
  # a function to find the mode
  dmode <- function(x) {
    den <- density(x, kernel=c("biweight"), bw = .01)
    ( den$x[den$y==max(den$y)] )   
  }
  
  ### Normal + MCMC
  est <- mean(m7$BUGSoutput$sims.matrix[, bugs.coef.names[var.index]])
  est2 <- median(m7$BUGSoutput$sims.matrix[, bugs.coef.names[var.index]])
  est3 <- dmode(m7$BUGSoutput$sims.matrix[, bugs.coef.names[var.index]])
  lwr <- quantile(m7$BUGSoutput$sims.matrix[, bugs.coef.names[var.index]], .05)
  upr <- quantile(m7$BUGSoutput$sims.matrix[, bugs.coef.names[var.index]], .95)
  lines(c(lwr, upr), c(-7, -7), lty = 1.5)
  points(est3, -7, pch = 19)
  points(est2, -7, pch = 21, bg = "white")
  points(est, -7, pch = 4, bg = "white")
  text(est3, -7, model.names[7], pos = 3)
  
  ### t10 + MCMC
  est <- mean(m8$BUGSoutput$sims.matrix[, bugs.coef.names[var.index]])
  est2 <- median(m8$BUGSoutput$sims.matrix[, bugs.coef.names[var.index]])
  est3 <- dmode(m8$BUGSoutput$sims.matrix[, bugs.coef.names[var.index]])
  lwr <- quantile(m8$BUGSoutput$sims.matrix[, bugs.coef.names[var.index]], .05)
  upr <- quantile(m8$BUGSoutput$sims.matrix[, bugs.coef.names[var.index]], .95)
  lines(c(lwr, upr), c(-8, -8), lty = 1.5)
  points(est3, -8, pch = 19)
  points(est2, -8, pch = 21, bg = "white")
  points(est, -8, pch = 4)
  text(est3, -8, model.names[8], pos = 3)
  
  ### Cauchy + MCMC
  est <- mean(m9$BUGSoutput$sims.matrix[, bugs.coef.names[var.index]])
  est2 <- median(m9$BUGSoutput$sims.matrix[, bugs.coef.names[var.index]])
  est3 <- dmode(m9$BUGSoutput$sims.matrix[, bugs.coef.names[var.index]])
  lwr <- quantile(m9$BUGSoutput$sims.matrix[, bugs.coef.names[var.index]], .05)
  upr <- quantile(m9$BUGSoutput$sims.matrix[, bugs.coef.names[var.index]], .95)
  lines(c(lwr, upr), c(-9, -9), lty = 1.5)
  points(est3, -9, pch = 19)
  points(est2, -9, pch = 21, bg = "white")
  points(est, -9, pch = 4)
  text(est3, -9, model.names[9], pos = 3)
  #abline(v = est3)
}
dev.off()


