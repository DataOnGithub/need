
library(metRology)

n.sims <- 100
x <- c(0, 1)
X <- cbind(1, x)
b0 <- -4
scales <- c(.5, 1, 1.5, 2, 2.5)

par(mfrow = c(3,5), oma = c(3,3,1,1), mar = c(.5, .5, 1, .5))

# Normal Prior
emf("Figures/rc_prior_selection.emf", height = 4, width = 10, family = "serif")
par(mfrow = c(3,5), oma = c(3,1,1,1), mar = c(.75, 1, 1, 1), family = "serif")
for (s in 1:length(scales)){
b1 <- abs(rnorm(n.sims, 0, scales[s]))
B <- cbind(b0, b1)
y.star <- X%*%t(B)
p <- plogis(y.star)
eplot(xlim = c(-.03, 1.03), ylim = c(0, 1),
      ylab = "Hypothetical\nPr(Oppose)",
      ylabpos = 2.1,
      xlab = "Partisanship",
      xat = c(0,1), xticklab = c("Democrat", "Republican"),
      main = paste("Normal(0, ", scales[s], ")", sep = ""))

for (i in 1:n.sims) {
    lines(c(0, 1), c(p[1, i], p[2, i]))
  }
}

# Cauchy Prior
for (s in 1:length(scales)){
  b1 <- abs(rt.scaled(n.sims, df = 10, mean = 0, sd = scales[s]))
  B <- cbind(b0, b1)
  y.star <- X%*%t(B)
  p <- plogis(y.star)
  eplot(xlim = c(-.03, 1.03), ylim = c(0, 1),
        ylab = "Hypothetical\nPr(Oppose)",
        ylabpos = 2.1,
        xlab = "Partisanship",
        xat = c(0,1), xticklab = c("Democrat", "Republican"),
        main = paste("t(10, 0, ", scales[s], ")", sep = ""))
  
  for (i in 1:n.sims) {
    lines(c(0, 1), c(p[1, i], p[2, i]))
  }
}

# Cauchy Prior
for (s in 1:length(scales)){
  b1 <- abs(rcauchy(n.sims, 0, scales[s]))
  B <- cbind(b0, b1)
  y.star <- X%*%t(B)
  p <- plogis(y.star)
  eplot(xlim = c(-.03, 1.03), ylim = c(0, 1),
        ylab = "Hypothetical\nPr(Oppose)",
        ylabpos = 2.1,
        xlab = "Partisanship",
        xat = c(0,1), xticklab = c("Democrat", "Republican"),
        main = paste("Cauchy(0, ", scales[s], ")", sep = ""))
  
  for (i in 1:n.sims) {
    lines(c(0, 1), c(p[1, i], p[2, i]))
  }
}
dev.off()