#make a clean start
rm(list=ls())

#load all the necessary libraries
library(plyr)
library(ggplot2)
library(ggmap)

#read the passenger data, fileEncoding is important
passengers <- read.table("odd_data/data/passenger_counts/REISENDE_2014.csv", header=T, sep=";", fileEncoding="utf-8")

#take only the values with Tagtyp_Id == 7 (otherwise the sumPax per day per station will be too big)
passengers_7 <- passengers[passengers$Tagtyp_Id==7,]

#read the station infos (link Haltestellen_Id to Haltestellenkurzname)
stations <- read.table("odd_data/data/passenger_counts/HALTESTELLEN_2014.csv", header=T, sep=";", fileEncoding="utf-8")

#read the station coordinate informations (link Haltestellenkurzname to coordinates)
statCoor <- read.table("odd_data/data/delay_data/stations_2017.csv", header=T, sep=",", fileEncoding="utf-8")
statCoor <- statCoor[,c(4,5,9)]

#drop the entries with no coordinates available
statCoor <- statCoor[!is.na(statCoor$GPS_Latitude),]

#aggregate the values per Haltestellenkurzname (and average the coodinates of the different Haltepunkte)
statCoor <- aggregate(x=statCoor, by=list(unique.values = statCoor$halt_kurz), FUN=mean)
statCoor <- statCoor[,c(1,2,3)]

#aggregate data per Haltestellen_Id and sum up the pax
sumPax <- ddply(.data=passengers_7, .variables=c("Haltestellen_Id"), .fun=summarise, sum_Einsteiger=sum(Einsteiger), sum_Aussteiger=sum(Aussteiger))

#difference between Ein- and Aussteiger
sumPax$diff <- sumPax$sum_Einsteiger - sumPax$sum_Aussteiger
sumPax$netto[sumPax$diff>=1] <- "more boarding passenger"
sumPax$netto[sumPax$diff<1] <- "more leaving passengers"
sumPax$ratio <- sumPax$sumPax_Einsteiger/sumPax$sumPax_Aussteiger
sumPax$interestingRatio[sumPax$ratio >= 2] <- 1
sumPax$interestingRatio[sumPax$ratio <= 0.5] <- 1

#merge the sumPax with the station infos (to get coordinates)
sumPax <- merge(sumPax, stations, by="Haltestellen_Id")
sumPax <- merge(sumPax, agg_coor, by.x="Haltestellenkurzname", by.y="unique.values")
sumPax <- sumPax[!is.na(sumPax$diff),]

#load all the base maps for different zoom levels
zurich10 <- get_map("Zurich, Switzerland", maptype = "roadmap", zoom = 10, color  ="bw")
zurich12 <- get_map("Zurich, Switzerland", maptype = "roadmap", zoom = 12, color  ="bw")
zurich15 <- get_map("Zurich, Switzerland", maptype = "roadmap", zoom = 15, color  ="bw")

#plot the maps
ggmap(zurich10) +
  geom_point(aes(x = GPS_Longitude, y = GPS_Latitude, size = abs(diff), color = netto), alpha = 0.75, data = sumPax)

ggmap(zurich12) +
  geom_point(aes(x = GPS_Longitude, y = GPS_Latitude, size = abs(diff), color = netto), alpha = 0.75, data = sumPax)

ggmap(zurich15) +
  geom_point(aes(x = GPS_Longitude, y = GPS_Latitude, size = abs(diff), color = netto), alpha = 0.75, data = sumPax)
