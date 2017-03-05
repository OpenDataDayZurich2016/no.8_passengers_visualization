#Load packages:
library(foreign)
library(Rcpp)
library(plyr)
library("ggplot2")
library("scales")
library(dtw)
library(proxy)
library(reshape)
library("xlsx")

REISENDE_2014 <- read.csv("REISENDE_2014.csv",sep =";") 
# choose the day type
REISENDE_2014_tagtyp6 <- REISENDE_2014[REISENDE_2014$Tagtyp_Id == 6, ] 
REISENDE_2014_tagtyp6$In_out <- REISENDE_2014_tagtyp6$Einsteiger - REISENDE_2014_tagtyp6$Aussteiger
REISENDE_2014_tagtyp6$hms <- as.POSIXct(as.character(REISENDE_2014_tagtyp6$FZ_AB),
                                       format="%H:%M:%S",
                                       tz="UTC")

# some time is like 24:10:00, read them as NA, have not deal with them 

minute_inter <- 60 # round time to every 60 minutes, in order to use cluster algorithm later 
REISENDE_2014_tagtyp6$hms_round <- format(strptime("1970-01-01", "%Y-%m-%d" ,tz="UTC") + round(as.numeric(REISENDE_2014_tagtyp6$hms)/(60*minute_inter))*(60*minute_inter),"%H:%M")
REISENDE_2014_tagtyp6$hms_round <- as.POSIXct(REISENDE_2014_tagtyp6$hms_round, format="%H:%M", tz="UTC")  

REISENDE_2014_tagtyp6_meanhour <- ddply(.data = REISENDE_2014_tagtyp6,
                                       .variables = c("hms_round", "Haltestellen_Id"),
                                       .fun=summarise,
                                       in_out_mean = mean(In_out)
                                      )
View(REISENDE_2014_tagtyp6_meanhour)
View(REISENDE_2014_tagtyp6_meanhour[REISENDE_2014_tagtyp6_meanhour$Haltestellen_Id==699,])

# test <- ddply(.data = REISENDE_2014_tagtyp6_meanhour,
#               .variables = c("Haltestellen_Id"),
#               .fun=summarise,
#               hour_length = length(hms_round)
#               )
# View(test)

# choose time from 9:00 -> 19:00
REISENDE_2014_tagtyp6_meanhour_hour9_19 <- REISENDE_2014_tagtyp6_meanhour[REISENDE_2014_tagtyp6_meanhour$hms_round > as.POSIXct('2017-03-04 08:00:00', format="%Y-%m-%d %H:%M:%S", tz="UTC")  & REISENDE_2014_tagtyp6_meanhour$hms_round < as.POSIXct('2017-03-04 20:00:00', format="%Y-%m-%d %H:%M:%S", tz="UTC"),]
REISENDE_2014_tagtyp6_meanhour_hour9_19 <- REISENDE_2014_tagtyp6_meanhour_hour9_19[is.na(REISENDE_2014_tagtyp6_meanhour_hour9_19$hms_round)==FALSE,]
REISENDE_2014_tagtyp6_meanhour_hour9_19 <- REISENDE_2014_tagtyp6_meanhour_hour9_19[REISENDE_2014_tagtyp6_meanhour_hour9_19$Haltestellen_Id != 209,]
View(REISENDE_2014_tagtyp6_meanhour_hour9_19)
# test2 <- ddply(.data = REISENDE_2014_tagtyp6_meanhour_hour9_19,
#               .variables = c("Haltestellen_Id"),
#               .fun=summarise,
#               hour_length = length(hms_round)
# )
# View(test2)
# exclude some special stations
REISENDE_2014_tagtyp6_meanhour_hour9_19 <- REISENDE_2014_tagtyp6_meanhour_hour9_19[!(REISENDE_2014_tagtyp6_meanhour_hour9_19$Haltestellen_Id %in% c(309,310,311,312,369,287)),]

## time series clustering ##############
# Dynamic Time Warping (DTW)
REISENDE_2014_tagtyp6_meanhour_hour9_19 <- REISENDE_2014_tagtyp6_meanhour_hour9_19[order(REISENDE_2014_tagtyp6_meanhour_hour9_19$hms_round, REISENDE_2014_tagtyp6_meanhour_hour9_19$Haltestellen_Id),]
REISENDE_2014_tagtyp6_meanhour_hour9_19_pivot <- cast(REISENDE_2014_tagtyp6_meanhour_hour9_19, Haltestellen_Id ~ hms_round, value = "in_out_mean")

# order by station_id
REISENDE_2014_tagtyp6_meanhour_hour9_19_pivot <- REISENDE_2014_tagtyp6_meanhour_hour9_19_pivot[order(REISENDE_2014_tagtyp6_meanhour_hour9_19_pivot$Haltestellen_Id),] 
REISENDE_2014_tagtyp6_meanhour_hour9_19_pivot$Haltestellen_Id <- NULL  # leave only time series data

# save project and data before this line, memory consumig, may crash...
distMatrix <- dist(REISENDE_2014_tagtyp6_meanhour_hour9_19_pivot, method="DTW")


hc <- hclust(distMatrix, method="average") # hc is a tree as produced by hclust
plot(hc, main="")
observedLabels <- cutree(hc, k = 100) # k: desired number of groups, should be changed accordinly ############
plot(hc, labels = observedLabels, main="")
#cophenetic distance to evalute the cluster, [-1,1],the bigger the better
cop <- cophenetic(hc)
cor(cop, distMatrix)

# assign the cluster_label_k, order by hms, then station_id, this is important
REISENDE_2014_tagtyp6_meanhour_hour9_19 <- REISENDE_2014_tagtyp6_meanhour_hour9_19[order(REISENDE_2014_tagtyp6_meanhour_hour9_19$hms_round, REISENDE_2014_tagtyp6_meanhour_hour9_19$Haltestellen_Id),]
REISENDE_2014_tagtyp6_meanhour_hour9_19$cluster_label_k <- rep(observedLabels, (11)) # 9-19:00, 11 hours 
REISENDE_2014_tagtyp6_meanhour_hour9_19$cluster_label_k <- as.character(REISENDE_2014_tagtyp6_meanhour_hour9_19$cluster_label_k)
REISENDE_2014_tagtyp6_meanhour_hour9_19$Haltestellen_Id <- as.character(REISENDE_2014_tagtyp6_meanhour_hour9_19$Haltestellen_Id)

test_cluster_num <- ddply(.data = REISENDE_2014_tagtyp6_meanhour_hour9_19,.variables = c("cluster_label_k"),
                          .fun=summarise, 
                          num_this_cluster = length(Haltestellen_Id))
View(test_cluster_num)

# visual certain labels 
p<-ggplot(REISENDE_2014_tagtyp6_meanhour_hour9_19[REISENDE_2014_tagtyp6_meanhour_hour9_19$cluster_label_k==6,]) + 
  aes(x=hms_round, y=in_out_mean, group=Haltestellen_Id,
  colour=Haltestellen_Id, na.rm=TRUE)+
  geom_line(size=1) +
  geom_point(size=0.2)+ theme_bw()+
  scale_x_datetime(labels=date_format("%H",tz="UTC"), breaks = date_breaks("1 hour"))

p<-p+xlab("Hour") + ylab("In - Out") + theme(axis.text=element_text(size=5),
                                           legend.title=element_blank(),
                                           legend.text = element_text(size=5),
                                           legend.position="right",
                                           axis.title=element_text(size=5))

p


# ddply to merge clusters, choose several clusters for examples 
REISENDE_2014_tagtyp6_meanhour_hour9_19_cluster <- ddply(.data = REISENDE_2014_tagtyp6_meanhour_hour9_19[
                                                          REISENDE_2014_tagtyp6_meanhour_hour9_19$cluster_label_k %in% c("3","4","20","33","6","14"),],
                                                         .variables = c("cluster_label_k", "hms_round"),
                                                         .fun = summarise,
                                                         mean_in_out = mean(in_out_mean),
                                                         num_station = length(in_out_mean),
                                                         stdv_in_out = sd(in_out_mean))
View(REISENDE_2014_tagtyp6_meanhour_hour9_19_cluster)

p_cluster <- ggplot(REISENDE_2014_tagtyp6_meanhour_hour9_19_cluster) + 
  aes(x = hms_round,y = mean_in_out,group=cluster_label_k,colour=cluster_label_k, na.rm=TRUE)+
  geom_line(size=1) + 
  geom_point(size=0.2)+ theme_bw() + 
  scale_x_datetime(labels=date_format("%H",tz="UTC"), breaks = date_breaks("1 hour"))
p_cluster <- p_cluster + xlab("Hour") + 
            ylab("mean_in_out") + 
            theme(axis.text=element_text(size=15), axis.title=element_text(size=15),legend.title=element_blank()) + 
            scale_color_manual(   # breaks = c("4","3","2","1"),
              breaks = c("3","4","20","33","6","14"),
              labels = c("cluster1±sd","cluster2±sd","cluster3±sd","cluster4±sd","cluster5±sd","cluster6±sd"),
              values = c("green","orange","blue","red","yellow","green3")
  ) #+
#ylim(-0.25,0.25)
p_cluster
#draw the standard deviation 
p_cluster <- p_cluster + 
  geom_ribbon(data=REISENDE_2014_tagtyp6_meanhour_hour9_19_cluster,aes(ymin=mean_in_out+stdv_in_out,ymax=mean_in_out-stdv_in_out),linetype=2,alpha=0.05) +
  theme(legend.title=element_blank(),legend.position="bottom",legend.text = element_text(size=15)) + 
  scale_color_manual(   # breaks = c("4","3","2","1"),
    breaks = c("3","4","20","33","6","14"),
    labels = c("cluster1±sd","cluster2±sd","cluster3±sd","cluster4±sd","cluster5±sd","cluster6±sd"),
    values = c("green","orange","blue","red","green4","blue4")
  ) #+
#ylim(-0.25,0.25)
p_cluster

# output station cluster to show them on map
station_num <- 630 # this number of stations have passengers data between 9:00-19:00 
write.xlsx(x = REISENDE_2014_tagtyp6_meanhour_hour9_19[order(REISENDE_2014_tagtyp6_meanhour_hour9_19$hms_round),][1:station_num,],
           file = "REISENDE_2014_tagtyp6_meanhour_hour9_19.xlsx",
           sheetName = "REISENDE_2014_tagtyp6_meanhour_hour9_19", row.names = FALSE)

REISENDE_2014_tagtyp6_meanhour$Haltestellen_Id <- as.character(REISENDE_2014_tagtyp6_meanhour$Haltestellen_Id)

# show time series pattern of id 199
p199 <- ggplot(REISENDE_2014_tagtyp6_meanhour[REISENDE_2014_tagtyp6_meanhour$Haltestellen_Id==199,]) + 
  aes(x=hms_round, y=in_out_mean, group=Haltestellen_Id,
      colour=Haltestellen_Id, na.rm=TRUE)+
  geom_point(size=3) + theme_bw() +
  scale_x_datetime(labels=date_format("%H",tz="UTC"), breaks = date_breaks("1 hour"))

p199 <- p199 + xlab("Hour") + ylab("station199: In - Out") + theme(axis.text=element_text(size=15),
                                             legend.title=element_blank(),
                                             legend.text = element_text(size=15),
                                             legend.position="right",
                                             axis.title=element_text(size=15))

p199
