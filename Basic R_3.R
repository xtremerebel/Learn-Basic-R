#### read write #########

install.packages("xlsx")

#http://personal.colby.edu/personal/m/mgimond/RIntro/03_Reading_and_writing_files.html#reading-data-tables

require(dplyr)
dat <- read.csv("C:/Users/BIDHAN_MISHRA/Desktop/Learning/FAO_grains_NA.csv", header=TRUE)

summarise(dat, yr_min = min(Year), yr_max=max(Year))

dat.grp   <- group_by(dat, Country, Crop)
dat.sum   <- summarise(dat.grp, tot_yield = sum(Value))

library(dplyr)
dat2 <- dat %>%
  group_by(Crop, Year, Country) %>%
  summarise(median_yield = round(median(Value))) %>%
  arrange(Crop, desc(median_yield))

library(reshape2)
dcast(dat2[c(1:4)],Crop + Year ~ Country)
