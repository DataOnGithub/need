# clear workspace
rm(list = ls())

# set working directory
setwd("~/Dropbox/Projects/Need")

# load packages
library(arm)
library(plyr)
library(RCurl)

# load data
state.names <- read.csv("Data/state_names.csv")
gov.position <- read.csv("Data/gov_position.csv")
fmap <- read.csv("Data/fmap.csv")
metro <- read.csv("Data/metro.csv")
nasbo <- read.csv("Data/nasbo.csv")
parties <- read.csv("Data/parties.csv")
population <- read.csv("Data/population.csv")
race <- read.csv("Data/race.csv")
obama2012 <- read.csv("Data/obama2012.csv")
ideology <- read.csv("Data/ideology.csv")
insurance <- read.csv("Data/insurance.csv")
dsh <- read.csv("Data/dsh.csv")
poverty <- read.csv("Data/poverty.csv")
low.birthweight <- read.csv("Data/low_birthweight.csv")
infant.mortality.rate <- read.csv("Data/infant_mortality_rate.csv")
cancer.incidence <- read.csv("Data/cancer_incidence.csv")
heart.disease.death.rate <- read.csv("Data/heart_disease_death_rate.csv")
life.expectancy <- read.csv("Data/life_expectancy.csv")

# load MRP estimates from GitHub
url <- "https://raw.githubusercontent.com/carlislerainey/ACA_Opinion/master/Data/mrp_est.csv"
aca.opinion <- getURL(url)                
aca.opinion <- read.csv(textConnection(aca.opinion))

# join the data
d <- join(state.names, gov.position)
d <- join(d, fmap)
d <- join(d, nasbo)
d <- join(d, metro)
d <- join(d, parties)
d <- join(d, population)
d <- join(d, race)
d <- join(d, aca.opinion)
d <- join(d, obama2012)
d <- join(d, ideology)
d <- join(d, insurance)
d <- join(d, dsh)
d <- join(d, poverty)
d <- join(d, low.birthweight)
d <- join(d, infant.mortality.rate)
d <- join(d, cancer.incidence)
d <- join(d, heart.disease.death.rate)
d <- join(d, life.expectancy)

# drop DC
d <- d[d$state_abbr != "DC", ]

# save data
write.csv(d, "Data/politics_and_need.csv", row.names = FALSE)
