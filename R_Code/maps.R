
setwd("~/Dropbox/Projects/Need")

library(maps)
library(mapdata)
library(devEMF)
#library(extrafont)

d <- read.csv("Data/politics_and_need.csv")
dem.support <- as.character(d$state[d$gop_governor == 0])
rep.support <- as.character(d$state[d$gop_governor == 1 & d$support_expansion == "Supports"])
rep.oppose <- as.character(d$state[d$gop_governor == 1 & d$support_expansion == "Opposes"])
rep.weighing <- as.character(d$state[d$gop_governor == 1 & d$support_expansion == "Weighing Options"])


tiff("Figures/maps.tiff", height = 3, width = 9, units = "in", res = 300, family = "serif")
par(mfrow = c(1, 2), oma = c(2.5,0,0,0), mar = c(0,0,0,0), family = "serif")
map("state", interior = FALSE, lty = 3)
map('state', 
    add = TRUE, lty = 1, fill = TRUE, col = "grey90", 
    region = dem.support)
mtext(side = 3, "States with Democratic Governors", cex = 1)
legend(par('usr')[1],par('usr')[3], xjust = 0, yjust = 1,
       legend = c("Support Expansion", "Weighing Options", "Oppose Expansion"), 
       pt.bg = c("grey90", "grey50", "grey10"), pt.lwd = 0, pch = 22, pt.cex = 2, xpd = NA)


map("state", interior = FALSE, lty = 3)
map('state', 
    add = TRUE, 
    lty = 1, fill = TRUE, col = "grey50", 
    region = rep.weighing)
map('state', 
    add = TRUE, lty = 1, fill = TRUE, col = "grey90", 
    region = rep.support)
map('state', 
    add = TRUE, lty = 1, fill = TRUE, col = "grey10", 
    region = rep.oppose)
mtext(side = 3, "States with Republican Governors", cex = 1)
dev.off()