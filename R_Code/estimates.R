rm(list = ls())

library(arm)
library(compactr)
library(brglm)
library(texreg)

setwd("~/Dropbox/ACA")
d <- read.csv("Data/politics_and_need_rescale.csv")

bs.form <- oppose_expansion ~ gop_governor + obama_win + gop_leg + percent_uninsured + 
  income + percent_nonwhite + percent_metro

#bs.form <- oppose_expansion ~ gop_governor + obama_win + alec + percent_uninsured
  
m <- bayesglm(bs.form, 
               prior.df = 1, prior.scale = 2.5,
               family = binomial, data = d)

m.mle <- glm(bs.form, family = binomial, data = d)
m.mle2 <- glm(bs.form, family = binomial, data = d,
              control = list(epsilon = 10^-20, maxit = 100000))
m.firth <- brglm(bs.form, family = binomial, data = d)

screenreg(list(m, m.mle, m.mle2, m.firth),
          stars = 0.2,
          custom.model.names = c("Cauchy(2.5) Prior",
                                 "Default MLE",
                                 "Low Tolerance MLE",
                                 "Firth's Logit")#,
#           custom.coef.names = c("Intercept",
#                                 "GOP Governor",
#                                 "Obama Win in 2012",
#                                 "GOP House",
#                                 "% Uninsured",
#                                 "Income",
#                                 "% Non-White",
#                                 "%% Metropolitan")
)


bs.size <- 1000
bs.res <- NULL
for (i in 1:bs.size) {
  bs.samp <- sample(1:nrow(d), nrow(d), replace = TRUE)
  bs.data <- d[bs.samp, ]
  bs.est <- bayesglm(bs.form, 
                     prior.df = 1, prior.scale = 2.5,
                     family = binomial, data = bs.data)$coef
  bs.res <- rbind(bs.res, bs.est)
}

par(mfrow = c(4,3), mar = c(5,4,4,2), oma = c(0,0,0,0))
for (i in 1:ncol(bs.res)) {
  hist(bs.res[, i], freq = FALSE, breaks = 25,
       main = paste(colnames(bs.res)[i], 
       "\n(p = ", sum(bs.res[,i] > 0)/nrow(bs.res), ")", sep = ""),
       xlim = c(-5, 5))
  abline(v = 0, lty = 3)
  curve(dnorm(x, coef(m)[i], sqrt(diag(vcov(m)))[i]), add = TRUE)
}

apply(bs.res > 0, 2, sum)/nrow(bs.res)

par(mfrow = c(4,3))
x.lo <- c(1, 0, 0, median(d$alec), quantile(d$percent_uninsured, .25), median(d$income), median(d$percent_nonwhite), median(d$percent_metro))
x.hi <- c(1, 0, 0, median(d$alec), quantile(d$percent_uninsured, .75), median(d$income), median(d$percent_nonwhite), median(d$percent_metro))
pr.lo.sims <- plogis(bs.res%*%x.lo)
pr.hi.sims <- plogis(bs.res%*%x.hi)
fd.sims <- pr.hi.sims - pr.lo.sims
hist(pr.lo.sims)
hist(pr.hi.sims)
hist(fd.sims)
quantile(fd.sims, c(.05, .5, .95))






## Plotting the estimates from different models

model.names <- c("Cauchy Prior + Bootstrap", "Cauchy Prior", "Firth's", "MLE")
n.models <- length(model.names)

var.names <- c('gop_governor', 'obama_win', 'gop_leg', 'percent_uninsured',
               'income', 'percent_nonwhite', 'percent_metro', '(Intercept)')
label.var.names <- c('GOP Governor', 'Obama Win in 2012', 'GOP Controlled Legislature', 'Percent Uninsured',
                     'Income', 'Percent Nonwhite', 'Percent Metropolitan', 'Constant')
state.names <- sort(unique(d$state_abbr))

n.vars <- length(coef(m))
n.states <- length(state.names)

#svg("Figures/coefs.svg", height = 5, width = 10, family = "Georgia")
par(mfrow = c(2,4), oma = c(3,1,2,1), mar = c(.75, .75, 1, .5))
for (var.index in 1:n.vars) {
  eplot(xlim = c(-6, 6), ylim = c(-n.models - .2, -.2), main = label.var.names[var.index], anny = FALSE, 
        text.size = 1, ylab = "State Removed from the Analysis", ylabpos = 2.7,
        xlab = "Logistic Regression Coefficient\nand 90% Confidence Interval", xlabpos = 2.5)
  abline(v = 0, lty = 3)
  
  ### Cauchy + Bootstrap
  q <- quantile(bs.res[, var.names[var.index]], c(.5, .05, .95))
  lines(q[2:3], c(-1, -1), lty = 1.5)
  points(q[1], -1, pch = 19)
  text(q[1], -1, "Cauchy Prior\n+ Bootstrap", pos = 3)
  
  ### Cauchy
  m <- bayesglm(bs.form, 
                prior.df = 1, prior.scale = 2.5,
                family = binomial, data = d)
  est <- coef(m)[var.names[var.index]]
  se <- sqrt(diag(vcov(m)))[var.names[var.index]]
  lines(c(est + 1.64*se, est - 1.64*se), c(-2, -2), lty = 1.5)
  points(est, -2, pch = 19)
  text(est, -2, "Cauchy Prior", pos = 3)
  
  ### Firth's
  m <- brglm(bs.form, family = binomial, data = d)
  est <- coef(m)[var.names[var.index]]
  se <- sqrt(diag(vcov(m)))[var.names[var.index]]
  lines(c(est + 1.64*se, est - 1.64*se), c(-3, -3), lty = 1.5)
  points(est, -3, pch = 19)
  text(est, -3, "Firth's Logit", pos = 3)
  
  ### MLE
  m <- glm(bs.form, family = binomial, data = d)
  est <- coef(m)[var.names[var.index]]
  se <- sqrt(diag(vcov(m)))[var.names[var.index]]
  lines(c(est + 1.64*se, est - 1.64*se), c(-4, -4), lty = 1.5)
  points(est, -4, pch = 19)
  text(est, -4, "Maximum Likelihood", pos = 3)
}
#dev.off()

## Hypothesis Tests
sum(bs.res[, "gop_governor"] < 0)/bs.size
sum(bs.res[, "obama_win"] > 0)/bs.size
sum(bs.res[, "gop_leg"] < 0)/bs.size
sum(bs.res[, "percent_uninsured"] < 0)/bs.size
