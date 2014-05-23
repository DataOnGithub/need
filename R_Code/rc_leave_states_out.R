library(arm)
library(devEMF)
library(compactr)

setwd("~/Dropbox/Projects/Need")
d <- read.csv("Data/politics_and_need_rescale.csv")

bs.form <- oppose_expansion ~ gop_governor + percent_favorable_aca + gop_leg + percent_uninsured + 
  bal2012 + multiplier + percent_nonwhite + percent_metro


m <- bayesglm(bs.form, 
              prior.df = 1, prior.scale = 2.5,
              family = binomial, data = d)

var.names <- c('gop_governor', 'percent_favorable_aca', 'gop_leg', 'percent_uninsured',
                 'bal2012', 'multiplier', 'percent_nonwhite', 'percent_metro')
label.var.names <- c('GOP Governor', 'Percent Favorable to ACA', 'GOP Controlled Legislature', 'Percent Uninsured',
                     'Fiscal Health', 'Medicaid Multiplier', 'Percent Nonwhite', 'Percent Metropolitan')
state.names <- sort(unique(d$state_abbr))

n.vars <- length(coef(m)) - 1
n.states <- length(state.names)

emf("Figures/rc_leave_states_out.emf", height = 10, width = 8, family = "serif")
par(mfrow = c(2,4), oma = c(3,4,2,1), mar = c(.75, .75, 1, .5), family = "serif")
for (var.index in 1:n.vars) {
  eplot(xlim = c(-4, 4), ylim = c(-50, 0), main = label.var.names[var.index], yat = c(-50:0), 
        yticklab = rev(c("None", as.character(state.names))), text.size = 1, ylab = "State Removed from the Analysis", ylabpos = 2.7,
        xlab = "Logistic Regression Coefficient")
  abline(v = 0, lty = 3)
  m <- bayesglm(bs.form, 
                prior.df = 1, prior.scale = 2.5,
                family = binomial, data = d)
  est <- coef(m)[var.names[var.index]]
  se <- sqrt(diag(vcov(m)))[var.names[var.index]]
  lines(c(est + se, est - se), c(0, 0), lty = 1.5)
  points(est, 0, pch = 19)
  print(var.names[var.index])
  for (state.index in 1:n.states) {
    d0 <- d[d$state_abbr != state.names[state.index], ]
    m <- bayesglm(bs.form, 
                  prior.df = 1, prior.scale = 2.5,
                  family = binomial, data = d0)
    est <- coef(m)[var.names[var.index]]
    se <- sqrt(diag(vcov(m)))[var.names[var.index]]
    lines(c(est + se, est - se), c(-state.index, -state.index))
    points(est, -state.index, pch = 21, bg = "white")
  }
}
dev.off()
