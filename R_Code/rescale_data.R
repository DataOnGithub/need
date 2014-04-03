library(arm)

setwd("~/Dropbox/ACA")
d <- read.csv("Data/politics_and_need.csv")

# Variables for the analysis
state <- d$state
state_abbr <- d$state_abbr
oppose_expansion <- 1*(d$support_expansion_new == "Opposes")
gop_governor <- rescale(d$gop_governor)
obama_share <- rescale(d$obama_share)
obama_win <- rescale(d$obama_share > 0.5)
ideology <- rescale(d$ideology)
gop_house <- rescale(d$hscontrol)
dem_house <- rescale(1 - d$hscontrol)
gop_sen <- rescale(d$sencontrol)
gop_leg <- rescale(d$legGOP)
dem_leg <- rescale(d$legDem)
divided_leg <- rescale(d$legDem == 0 & d$legGOP == 0)
percent_uninsured <- rescale(d$percent_uninsured)
percent_uninsured_original_scale <- d$percent_uninsured
percent_nonwhite <- rescale(d$percent_hispanic + d$percent_black)
income <- rescale(d$income)
percent_metro <- rescale(d$percent_metro)

d.rescale <- data.frame(state, 
                        state_abbr,
                        oppose_expansion,
                        gop_governor,
                        obama_share,
                        obama_win,
                        ideology,
                        gop_house,
                        gop_sen,
                        gop_leg,
                        dem_leg,
                        divided_leg,
                        percent_uninsured,
                        percent_uninsured_original_scale,
                        percent_nonwhite,
                        income,
                        percent_metro)


write.csv(d.rescale, "Data/politics_and_need_rescale.csv", row.names = FALSE)
