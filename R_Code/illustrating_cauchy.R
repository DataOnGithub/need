library(compactr)

setwd("~/Dropbox/ACA")



#svg(file = "Figures/cauchy.svg", family = "Georgia", height = 2, width = 3)
par(mfrow = c(1,1), mar = c(3,3,1,1), oma = c(0,0,0,0),
    xaxs = "i", yaxs = "i")
eplot(xlim = c(-7, 7), ylim = c(0, 1.1*dcauchy(0, 0, 2.5)))
curve(dcauchy(x, 0, 2.5), add = TRUE)
#curve(dcauchy(x, coef(m)[2], sqrt(diag(vcov(m)))[2]), add = TRUE)
#curve(dcauchy(x, coef(m.mle)[2], sqrt(diag(vcov(m.mle)))[2]), add = TRUE)
#dev.off()

