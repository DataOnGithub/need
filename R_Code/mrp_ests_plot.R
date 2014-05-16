# clear workspace 
rm(list = ls())

library(compactr)

setwd("~/Dropbox/Projects/Need")
d <- read.csv("~/Dropbox/Projects/ACA_Opinion/Data/mrp_est.csv", stringsAsFactors = FALSE)
tiff("Figures/mrp_ests.tiff", height = 7, width = 5, unit = "in", res = 300, family = "serif")
par(mfrow = c(1,1), oma = c(3,1,1,1), mar = c(0,0,0,0), family = "serif")
eplot(xlim = c(.2, 1), ylim = c(1, 51), anny = FALSE,
      xlab = "Proportion Favorable to ACA")
abline(v = .5, col = "grey50")
points(d$percent_favorable_aca[order(d$percent_favorable_aca)], 1:51, pch = 19)
text(d$percent_favorable_aca[order(d$percent_favorable_aca)], 1:51, d$state_abbr[order(d$percent_favorable_aca)], cex = .8, pos = 4)
dev.off()