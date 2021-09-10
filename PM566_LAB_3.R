library(tidyverse)
download.file("https://raw.githubusercontent.com/USCbiostats/data-science-data/master/02_met/met_all.gz", "met_all.gz", method="libcurl", timeout = 60)
met <- data.table::fread("met_all.gz")
dim(met)
head(met)
tail(met)
str(met)
table(met$year)
table(met$day)
table(met$hour)
summary(met$temp)
summary(met$elev)
met[met$elev==9999.0] <- NA
summary(met$elev)
met <- met[temp>-40]
met2 <- met[order(temp)]
head(met2)
met <- met[temp>-15]
met2 <- met[order(temp)]
head(met2)
elev <- met[elev==max(elev)]
summary(elev)
cor(elev$temp, elev$wind.sp, use="complete")
cor(elev$temp, elev$hour, use="complete")
cor(elev$wind.sp, elev$day, use="complete")
cor(elev$wind.sp, elev$hour, use="complete")
cor(elev$temp, elev$day, use="complete")
hist(met$elev, breaks=100)
hist(met$temp)
hist(met$wind.sp)
library(leaflet)
leaflet(elev) %>%
  addProviderTiles('OpenStreetMap') %>% 
  addCircles(lat=~lat,lng=~lon, opacity=1, fillOpacity=1, radius=100)
library(lubridate)
elev$date <- with(elev, ymd_h(paste(year, month, day, hour, sep= ' ')))
summary(elev$date)
elev <- elev[order(date)]
head(elev)
plot(elev$date, elev$temp, type='l')
plot(elev$date, elev$wind.sp, type='l')

