
# If you do NOT want to start from scratch, skip down to #SKIPPED below
# Otherwise, this is working with the raw data

# first set working directory to Source File Location (directory of this script)
mdot_const <- read.csv("../raw_data/bluetoad.csv")

# except that part above-- that's a slice of a 352 mb csv of average travel time figures 
# at 5 minute increments between June 2012 and October 2013. The csv just loaded
# is just the subset of September 2013 data for eight pair_ids: 10356, 10357, 10360, 
# 10361, 10363, 10364, 10496, 10498, 10499. If you'd like the whole shebang, visit
# https://github.com/hackreduce/MassDOThack/tree/master/Road_RTTM_Volume
# for pair_id definitions referenced above and below, 
# visit https://github.com/hackreduce/MassDOThack/blob/master/Road_RTTM_Volume/pair_definitions.csv

# loading some libraries
library(lubridate)
library(dplyr)
library(ggplot2)

# converts the time column into data that can be recognized as such by R
mdot_const$time <- mdy_hm(mdot_const$insert_time)

# narrows down the September subset data to the two weeks of data we collected for 2014, but for 2013
# Sept 16 - 27, 2013 = Sept 15 - 26, 2014
sept2013 <- subset(mdot_const, time >= "2013-09-15 00:05:00" & time <= "2013-09-28 00:05:00")

# Also have to compensate for the historical data being in GMT/UTC time
sept2013$time_utc <- sept2013$time -hours(4)


# speaking of 2014, here's the data for scraped for September 2014
sept2014 <- read.csv("../raw_data/traffic_times_2014.csv")
sept2014$datetime <- mdy_hm(sept2014$datetime)

# adding a column to the 2014 data to give the equivalent 2013 dates so Mon = Mon, Tues = Tues, etc.
sept2014$time_utc <- sept2014$datetime - years(1)
sept2014$time_utc <- sept2014$time_utc + days(1)

# housekeeping/cleaning up some column names
sept2014$pair_id <- sept2014$pairid
sept2014$pairid <- NULL
sept2013$time_adjusted <- sept2013$time

# merging 2013 and 2014 data. 
# The 2014 data was only scraped every 15 minutes so any non-matching 2013 timestamp is discarded
sept13_14 <- merge(sept2013, sept2014, by=c("pair_id", "time_utc"))

traffic <- sept13_14

# some more housecleaning/eliminating unnecessary columns

traffic$time_14 <- traffic$datetime
traffic$datetime <- NULL
traffic$time_13 <- traffic$time_utc
traffic$time_utc <- NULL
traffic$insert_time <- NULL
traffic$time <- NULL
traffic$id <- NULL
traffic$X <- NULL
traffic$time_adjusted <- NULL
traffic$travel_13 <- traffic$travel_time
traffic$travel_time <- NULL
traffic$travel_14 <- traffic$traveltime
traffic$traveltime <- NULL
traffic$time_adjusted <- NULL

#SKIPPED -- Ok, working with the pre-subsetted and cleaned up data
traffic <- read.csv(../sept1314_merged.csv)

# converting time columns into something readable by R
traffic$time_13 <- ymd_hms(traffic$time_13)
traffic$time_14 <- ymd_hms(traffic$time_14)
traffic$day <- day(traffic$time_14)

traffic$travel_13 <- as.numeric(levels(traffic$travel_13))[traffic$travel_13]

# converting the time traveled into minutes
traffic$travel_13 <- traffic$travel_13/60
traffic$travel_14 <- traffic$travel_14/60

# subsetting the merged data to focus on specific roads.
# the following code focuses on just 4 stretches of road
# if you want to get the aggregate charts for the other 5 roads, 
# just modify the code below with a pair_id listed in the third graf

# skip down to #MOAR to get to the code that will generate charts for every road for every day
traffic_10361 <- subset(traffic, pair_id=="10361")
traffic_10356 <- subset(traffic, pair_id=="10356")
traffic_10360 <- subset(traffic, pair_id=="10360")
traffic_10357 <- subset(traffic, pair_id=="10357")


# 10356 aggregate. This filters the data for this road to just the workdays 
# and then to morning and afternoon rush hours. The average and median for the 15 minutes
# is calculated and charted out. 

traffic_10356_weekdays <- filter(traffic_10356, day==15 | day==16 | day==17 | day==18 | day==19 | day==22 | day==23 | day==24 | day==25 | day==26)

traffic_10356_weekdays$average_13 <- mean(traffic_10356_weekdays$travel_13, na.rm=TRUE)
traffic_10356_weekdays$average_14 <- mean(traffic_10356_weekdays$travel_14, na.rm=TRUE)
traffic_10356_weekdays$median_13 <- median(traffic_10356_weekdays$travel_13, na.rm=TRUE)
traffic_10356_weekdays$median_14 <- median(traffic_10356_weekdays$travel_14, na.rm=TRUE)

traffic_10356_weekdays$test <- format(traffic_10356_weekdays$time_14, format = "%H:%M:%S")
traffic_10356_weekdays$test <- as.POSIXct(traffic_10356_weekdays$test, format = "%H:%M:%S")

traffic_10356_weekdays$hours <- format(traffic_10356_weekdays$time_14, format = "%H:%M:%S")
traffic_10356_weekdays$hours <- as.POSIXct(traffic_10356_weekdays$hours, format = "%H:%M:%S")

average_10356_13 <- aggregate(traffic_10356_weekdays$travel_13 ~ traffic_10356_weekdays$hours, FUN=mean)
average_10356_13$variable = "Travel Time 2013"
average_10356_14 <- aggregate(traffic_10356_weekdays$travel_14 ~ traffic_10356_weekdays$hours, FUN=mean)
average_10356_14$variable= "Travel Time 2014"
colnames(average_10356_13) <- c("date", "minutes", "variable")
colnames(average_10356_14) <- c("date", "minutes", "variable")
average_10356 <- rbind(average_10356_13, average_10356_14)

png("../charts/average_10356.png", width=920, height=470, res=90)
a10356 <- ggplot(average_10356, aes(date, minutes, colour = variable))  + ylim(0, 20) + xlab("time") + ggtitle("10356 average travel time") 
a10356 + geom_line()
dev.off()

median_10356_13 <- aggregate(traffic_10356_weekdays$travel_13 ~ traffic_10356_weekdays$hours, FUN=median)
median_10356_13$variable = "Travel Time 2013"
median_10356_14 <- aggregate(traffic_10356_weekdays$travel_14 ~ traffic_10356_weekdays$hours, FUN=median)
median_10356_14$variable= "Travel Time 2014"
colnames(median_10356_13) <- c("date", "minutes", "variable")
colnames(median_10356_14) <- c("date", "minutes", "variable")
average_10356 <- rbind(median_10356_13, median_10356_14)

png("../charts/median_10356.png", width=840, height=350, res=80)
m10356 <- ggplot(average_10356, aes(date, minutes, colour = variable))  + ylim(0, 20) + xlab("time") + ggtitle("10356 median travel time")  + scale_color_manual(values=c("#E69F00","#009E73"))
m10356 + geom_line()
dev.off()


# 10357 aggregate. This filters the data for this road to just the workdays 
# and then to morning and afternoon rush hours. The average and median for the 15 minutes
# is calculated and charted out. 

traffic_10357_weekdays <- filter(traffic_10357, day==15 | day==16 | day==17 | day==18 | day==19 | day==22 | day==23 | day==24 | day==25 | day ==26)

traffic_10357_weekdays$average_13 <- mean(traffic_10357_weekdays$travel_13, na.rm=TRUE)
traffic_10357_weekdays$average_14 <- mean(traffic_10357_weekdays$travel_14, na.rm=TRUE)
traffic_10357_weekdays$median_13 <- median(traffic_10357_weekdays$travel_13, na.rm=TRUE)
traffic_10357_weekdays$median_14 <- median(traffic_10357_weekdays$travel_14, na.rm=TRUE)

traffic_10357_weekdays$test <- format(traffic_10357_weekdays$time_14, format = "%H:%M:%S")
traffic_10357_weekdays$test <- as.POSIXct(traffic_10357_weekdays$test, format = "%H:%M:%S")

traffic_10357_weekdays$hours <- format(traffic_10357_weekdays$time_14, format = "%H:%M:%S")
traffic_10357_weekdays$hours <- as.POSIXct(traffic_10357_weekdays$hours, format = "%H:%M:%S")

average_10357_13 <- aggregate(traffic_10357_weekdays$travel_13 ~ traffic_10357_weekdays$hours, FUN=mean)
average_10357_13$variable = "Travel Time 2013"
average_10357_14 <- aggregate(traffic_10357_weekdays$travel_14 ~ traffic_10357_weekdays$hours, FUN=mean)
average_10357_14$variable= "Travel Time 2014"
colnames(average_10357_13) <- c("date", "minutes", "variable")
colnames(average_10357_14) <- c("date", "minutes", "variable")
average_10357 <- rbind(average_10357_13, average_10357_14)

png("../charts/average_10357.png", width=920, height=470, res=90)
a10357 <- ggplot(average_10357, aes(date, minutes, colour = variable))
a10357 + geom_line()
dev.off()

median_10357_13 <- aggregate(traffic_10357_weekdays$travel_13 ~ traffic_10357_weekdays$hours, FUN=median)
median_10357_13$variable = "Travel Time 2013"
median_10357_14 <- aggregate(traffic_10357_weekdays$travel_14 ~ traffic_10357_weekdays$hours, FUN=median)
median_10357_14$variable= "Travel Time 2014"
colnames(median_10357_13) <- c("date", "minutes", "variable")
colnames(median_10357_14) <- c("date", "minutes", "variable")
average_10357 <- rbind(median_10357_13, median_10357_14)

png("../charts/median_10357.png", width=920, height=470, res=90)
m10357 <- ggplot(average_10357, aes(date, minutes, colour = variable))
m10357 + geom_line()
dev.off()

# 10360 aggregate. This filters the data for this road to just the workdays 
# and then to morning and afternoon rush hours. The average and median for the 15 minutes
# is calculated and charted out. 

traffic_10360_weekdays <- filter(traffic_10360, day==15 | day==16 | day==17 | day==18 | day==19 | day==22 | day==23 | day==24 | day == 25 | day == 26)

traffic_10360_weekdays$average_13 <- mean(traffic_10360_weekdays$travel_13, na.rm=TRUE)
traffic_10360_weekdays$average_14 <- mean(traffic_10360_weekdays$travel_14, na.rm=TRUE)
traffic_10360_weekdays$median_13 <- median(traffic_10360_weekdays$travel_13, na.rm=TRUE)
traffic_10360_weekdays$median_14 <- median(traffic_10360_weekdays$travel_14, na.rm=TRUE)

traffic_10360_weekdays$test <- format(traffic_10360_weekdays$time_14, format = "%H:%M:%S")
traffic_10360_weekdays$test <- as.POSIXct(traffic_10360_weekdays$test, format = "%H:%M:%S")

traffic_10360_weekdays$hours <- format(traffic_10360_weekdays$time_14, format = "%H:%M:%S")
traffic_10360_weekdays$hours <- as.POSIXct(traffic_10360_weekdays$hours, format = "%H:%M:%S")

average_10360_13 <- aggregate(traffic_10360_weekdays$travel_13 ~ traffic_10360_weekdays$hours, FUN=mean)
average_10360_13$variable = "Travel Time 2013"
average_10360_14 <- aggregate(traffic_10360_weekdays$travel_14 ~ traffic_10360_weekdays$hours, FUN=mean)
average_10360_14$variable= "Travel Time 2014"
colnames(average_10360_13) <- c("date", "minutes", "variable")
colnames(average_10360_14) <- c("date", "minutes", "variable")
average_10360 <- rbind(average_10360_13, average_10360_14)

png("../charts/average_10360.png", width=920, height=470, res=90)
a10360 <- ggplot(average_10360, aes(date, minutes, colour = variable)) + ylim(0, 20) + xlab("time") + ggtitle("10360 average travel time") + scale_color_manual(values=c("#E69F00","#009E73"))
a10360 + geom_line()
dev.off()

median_10360_13 <- aggregate(traffic_10360_weekdays$travel_13 ~ traffic_10360_weekdays$hours, FUN=median)
median_10360_13$variable = "Travel Time 2013"
median_10360_14 <- aggregate(traffic_10360_weekdays$travel_14 ~ traffic_10360_weekdays$hours, FUN=median)
median_10360_14$variable= "Travel Time 2014"
colnames(median_10360_13) <- c("date", "minutes", "variable")
colnames(median_10360_14) <- c("date", "minutes", "variable")
average_10360 <- rbind(median_10360_13, median_10360_14)

png("../charts/median_10360.png", width=750, height=390, res=90)
m10360 <- ggplot(average_10360, aes(date, minutes, colour = variable)) + ylim(0, 20) + xlab("time") + ggtitle("10360 median travel time") + scale_color_manual(values=c("#E69F00","#009E73"))
m10360 + geom_line()
dev.off()


# 10361 aggregate. This filters the data for this road to just the workdays 
# and then to morning and afternoon rush hours. The average and median for the 15 minutes
# is calculated and charted out. 

traffic_10361_weekdays <- filter(traffic_10361, day==15 | day==16 | day==17 | day==18 | day==19 | day==22 | day==23 | day==24 | day==25 | day ==26)

traffic_10361_weekdays$average_13 <- mean(traffic_10361_weekdays$travel_13, na.rm=TRUE)
traffic_10361_weekdays$average_14 <- mean(traffic_10361_weekdays$travel_14, na.rm=TRUE)
traffic_10361_weekdays$median_13 <- median(traffic_10361_weekdays$travel_13, na.rm=TRUE)
traffic_10361_weekdays$median_14 <- median(traffic_10361_weekdays$travel_14, na.rm=TRUE)

traffic_10361_weekdays$test <- format(traffic_10361_weekdays$time_14, format = "%H:%M:%S")
traffic_10361_weekdays$test <- as.POSIXct(traffic_10361_weekdays$test, format = "%H:%M:%S")

traffic_10361_weekdays$hours <- format(traffic_10361_weekdays$time_14, format = "%H:%M:%S")
traffic_10361_weekdays$hours <- as.POSIXct(traffic_10361_weekdays$hours, format = "%H:%M:%S")

average_10361_13 <- aggregate(traffic_10361_weekdays$travel_13 ~ traffic_10361_weekdays$hours, FUN=mean)
average_10361_13$variable = "Travel Time 2013"
average_10361_14 <- aggregate(traffic_10361_weekdays$travel_14 ~ traffic_10361_weekdays$hours, FUN=mean)
average_10361_14$variable= "Travel Time 2014"
colnames(average_10361_13) <- c("date", "minutes", "variable")
colnames(average_10361_14) <- c("date", "minutes", "variable")
average_10361 <- rbind(average_10361_13, average_10361_14)

png("../charts/average_10361.png", width=920, height=470, res=90)
a10361 <- ggplot(average_10361, aes(date, minutes, colour = variable)) + ylim(0, 20) + xlab("time") + ggtitle("10361 average travel time") 
a10361 + geom_line() 
dev.off()

median_10361_13 <- aggregate(traffic_10361_weekdays$travel_13 ~ traffic_10361_weekdays$hours, FUN=median)
median_10361_13$variable = "Travel Time 2013"
median_10361_14 <- aggregate(traffic_10361_weekdays$travel_14 ~ traffic_10361_weekdays$hours, FUN=median)
median_10361_14$variable= "Travel Time 2014"
colnames(median_10361_13) <- c("date", "minutes", "variable")
colnames(median_10361_14) <- c("date", "minutes", "variable")
average_10361 <- rbind(median_10361_13, median_10361_14)

png("../charts/median_10361.png", width=920, height=470, res=90)
m10361 <- ggplot(average_10361, aes(date, minutes, colour = variable)) + ylim(0, 20) + xlab("time") + ggtitle("10361 median travel time") + scale_color_manual(values=c("#E69F00","#009E73"))
m10361 + geom_line()
dev.off()

#MOAR

# Charts for every day of the week of data for pairids 10356 and 10361
# also average and median times for daily and rush hours. (Not in a dataframe, sorry)
# To get charts and calculations for the seven roads listed way up top,
# just replace the numbers below with whichever number you want

# 10356 specifics

traffic_stacked_10356 <- with(traffic_10356, data.frame(minutes = c(travel_13, travel_14), variable = factor(rep(c("Travel Time 2013","Travel Time 2014"),
                                                                                                                 each = NROW(traffic_10356))), date = rep(time_14, 2)))

traffic_stacked_10356$day <- day(traffic_stacked_10356$date)


traffic_stacked_10356_11 <- filter(traffic_stacked_10356, day==11)
traffic_stacked_10356_12 <- filter(traffic_stacked_10356, day==12)
traffic_stacked_10356_13 <- filter(traffic_stacked_10356, day==13)
traffic_stacked_10356_14 <- filter(traffic_stacked_10356, day==14)
traffic_stacked_10356_15 <- filter(traffic_stacked_10356, day==15)
traffic_stacked_10356_16 <- filter(traffic_stacked_10356, day==16)
traffic_stacked_10356_17 <- filter(traffic_stacked_10356, day==17)
traffic_stacked_10356_18 <- filter(traffic_stacked_10356, day==18)
traffic_stacked_10356_19 <- filter(traffic_stacked_10356, day==19)
traffic_stacked_10356_20 <- filter(traffic_stacked_10356, day==20)
traffic_stacked_10356_21 <- filter(traffic_stacked_10356, day==21)
traffic_stacked_10356_22 <- filter(traffic_stacked_10356, day==22)
traffic_stacked_10356_23 <- filter(traffic_stacked_10356, day==23)
traffic_stacked_10356_24 <- filter(traffic_stacked_10356, day==24)
traffic_stacked_10356_25 <- filter(traffic_stacked_10356, day==25)
traffic_stacked_10356_26 <- filter(traffic_stacked_10356, day==26)

png("../charts/10356_11.png", width=920, height=470, res=90)
p11 <- ggplot(traffic_stacked_10356_11, aes(date, minutes, colour = variable))
p11 + geom_line()
dev.off()

png("../charts/10356_12.png", width=920, height=470, res=90)
p12 <- ggplot(traffic_stacked_10356_12, aes(date, minutes, colour = variable))
p12 + geom_line()
dev.off()

png("../charts/10356_13.png", width=920, height=470, res=90)
p13 <- ggplot(traffic_stacked_10356_13, aes(date, minutes, colour = variable))
p13 + geom_line()
dev.off()

png("../charts/10356_14.png", width=920, height=470, res=90)
p14 <- ggplot(traffic_stacked_10356_14, aes(date, minutes, colour = variable))
p14 + geom_line()
dev.off()

png("../charts/10356_15.png", width=920, height=470, res=90)
p15 <- ggplot(traffic_stacked_10356_15, aes(date, minutes, colour = variable))
p15 + geom_line()
dev.off()

png("../charts/10356_16.png", width=920, height=470, res=90)
p16 <- ggplot(traffic_stacked_10356_16, aes(date, minutes, colour = variable))
p16 + geom_line()
dev.off()

png("../charts/10356_17.png", width=920, height=470, res=90)
p17 <- ggplot(traffic_stacked_10356_17, aes(date, minutes, colour = variable))
p17 + geom_line()
dev.off()

png("../charts/10356_18.png", width=920, height=470, res=90)
p18 <- ggplot(traffic_stacked_10356_18, aes(date, minutes, colour = variable))
p18 + geom_line()
dev.off()

png("../charts/10356_19.png", width=920, height=470, res=90)
p19 <- ggplot(traffic_stacked_10356_19, aes(date, minutes, colour = variable))
p19 + geom_line()
dev.off()

png("../charts/10356_20.png", width=920, height=470, res=90)
p20 <- ggplot(traffic_stacked_10356_20, aes(date, minutes, colour = variable))
p20 + geom_line()
dev.off()

png("../charts/10356_21.png", width=920, height=470, res=90)
p21 <- ggplot(traffic_stacked_10356_21, aes(date, minutes, colour = variable))
p21 + geom_line()
dev.off()

png("../charts/10356_22.png", width=920, height=470, res=90)
p22 <- ggplot(traffic_stacked_10356_22, aes(date, minutes, colour = variable))
p22 + geom_line()
dev.off()

png("../charts/10356_23.png", width=920, height=470, res=90)
p23 <- ggplot(traffic_stacked_10356_23, aes(date, minutes, colour = variable))
p23 + geom_line()
dev.off()

png("../charts/10356_24.png", width=920, height=470, res=90)
p24 <- ggplot(traffic_stacked_10356_24, aes(date, minutes, colour = variable))
p24 + geom_line()
dev.off()

png("../charts/10356_25.png", width=920, height=470, res=90)
p24 <- ggplot(traffic_stacked_10356_25, aes(date, minutes, colour = variable))
p24 + geom_line()
dev.off()

png("../charts/10356_26.png", width=920, height=470, res=90)
p24 <- ggplot(traffic_stacked_10356_26, aes(date, minutes, colour = variable))
p24 + geom_line()
dev.off()

mean(traffic_10356_weekdays$travel_13, na.rm=TRUE)
mean(traffic_10356_weekdays$travel_14, na.rm=TRUE)
median(traffic_10356_weekdays$travel_13, na.rm=TRUE)
median(traffic_10356_weekdays$travel_14, na.rm=TRUE)

traffic_10356_weekdays$hour <- hour(traffic_10356_weekdays$time_14)

traffic_10356_morning <- filter(traffic_10356_weekdays, hour==8 | hour==9 | hour==10)
traffic_10356_morning <- filter(traffic_10356_morning,  day==15 | day==16 | day==17 | day==18 | day==19 | day==22 | day==23 | day==24 | day==25 | day==26)
traffic_10356_afternoon <- filter(traffic_10356_weekdays, hour==16 | hour==17 | hour==18)
traffic_10356_afternoon <- filter(traffic_10356_afternoon,  day==15 | day==16 | day==17 | day==18 | day==19 | day==22 | day==23 | day==24 | day==25 | day==26)

mean(traffic_10356_morning$travel_13, na.rm=TRUE)
mean(traffic_10356_morning$travel_14, na.rm=TRUE)
median(traffic_10356_morning$travel_13, na.rm=TRUE)
median(traffic_10356_morning$travel_14, na.rm=TRUE)

mean(traffic_10356_afternoon$travel_13, na.rm=TRUE)
mean(traffic_10356_afternoon$travel_14, na.rm=TRUE)
median(traffic_10356_afternoon$travel_13, na.rm=TRUE)
median(traffic_10356_afternoon$travel_14, na.rm=TRUE)

#10361 specifics

traffic_stacked_10361 <- with(traffic_10361, data.frame(minutes = c(travel_13, travel_14), variable = factor(rep(c("Travel Time 2013","Travel Time 2014"),
                                                                                                                 each = NROW(traffic_10361))), date = rep(time_14, 2)))

traffic_stacked_10361$day <- day(traffic_stacked_10361$date)


traffic_stacked_10361_11 <- filter(traffic_stacked_10361, day==11)
traffic_stacked_10361_12 <- filter(traffic_stacked_10361, day==12)
traffic_stacked_10361_13 <- filter(traffic_stacked_10361, day==13)
traffic_stacked_10361_14 <- filter(traffic_stacked_10361, day==14)
traffic_stacked_10361_15 <- filter(traffic_stacked_10361, day==15)
traffic_stacked_10361_16 <- filter(traffic_stacked_10361, day==16)
traffic_stacked_10361_17 <- filter(traffic_stacked_10361, day==17)
traffic_stacked_10361_18 <- filter(traffic_stacked_10361, day==18)
traffic_stacked_10361_19 <- filter(traffic_stacked_10361, day==19)
traffic_stacked_10361_20 <- filter(traffic_stacked_10361, day==20)
traffic_stacked_10361_21 <- filter(traffic_stacked_10361, day==21)
traffic_stacked_10361_22 <- filter(traffic_stacked_10361, day==22)
traffic_stacked_10361_23 <- filter(traffic_stacked_10361, day==23)
traffic_stacked_10361_24 <- filter(traffic_stacked_10361, day==24)
traffic_stacked_10361_25 <- filter(traffic_stacked_10361, day==25)
traffic_stacked_10361_26 <- filter(traffic_stacked_10361, day==26)

png("../charts/10361_11.png", width=920, height=470, res=90)
p11 <- ggplot(traffic_stacked_10361_11, aes(date, minutes, colour = variable))
p11 + geom_line()
dev.off()

png("../charts/10361_12.png", width=920, height=470, res=90)
p12 <- ggplot(traffic_stacked_10361_12, aes(date, minutes, colour = variable))
p12 + geom_line()
dev.off()

png("../charts/10361_13.png", width=920, height=470, res=90)
p13 <- ggplot(traffic_stacked_10361_13, aes(date, minutes, colour = variable))
p13 + geom_line()
dev.off()

png("../charts/10361_14.png", width=920, height=470, res=90)
p14 <- ggplot(traffic_stacked_10361_14, aes(date, minutes, colour = variable))
p14 + geom_line()
dev.off()

png("../charts/10361_15.png", width=920, height=470, res=90)
p15 <- ggplot(traffic_stacked_10361_15, aes(date, minutes, colour = variable))
p15 + geom_line()
dev.off()

png("../charts/10361_16.png", width=920, height=470, res=90)
p16 <- ggplot(traffic_stacked_10361_16, aes(date, minutes, colour = variable))
p16 + geom_line()
dev.off()

png("../charts/10361_17.png", width=920, height=470, res=90)
p17 <- ggplot(traffic_stacked_10361_17, aes(date, minutes, colour = variable))
p17 + geom_line()
dev.off()

png("../charts/10361_18.png", width=920, height=470, res=90)
p18 <- ggplot(traffic_stacked_10361_18, aes(date, minutes, colour = variable))
p18 + geom_line()
dev.off()

png("../charts/10361_19.png", width=920, height=470, res=90)
p19 <- ggplot(traffic_stacked_10361_19, aes(date, minutes, colour = variable))
p19 + geom_line()
dev.off()

png("../charts/10361_20.png", width=920, height=470, res=90)
p20 <- ggplot(traffic_stacked_10361_20, aes(date, minutes, colour = variable))
p20 + geom_line()
dev.off()

png("../charts/10361_21.png", width=920, height=470, res=90)
p21 <- ggplot(traffic_stacked_10361_21, aes(date, minutes, colour = variable))
p21 + geom_line()
dev.off()

png("../charts/10361_22.png", width=920, height=470, res=90)
p22 <- ggplot(traffic_stacked_10361_22, aes(date, minutes, colour = variable))
p22 + geom_line()
dev.off()

png("../charts/10361_23.png", width=920, height=470, res=90)
p23 <- ggplot(traffic_stacked_10361_23, aes(date, minutes, colour = variable))
p23 + geom_line()
dev.off()

png("../charts/10361_24.png", width=920, height=470, res=90)
p24 <- ggplot(traffic_stacked_10361_24, aes(date, minutes, colour = variable))
p24 + geom_line()
dev.off()

png("../charts/10361_25.png", width=920, height=470, res=90)
p24 <- ggplot(traffic_stacked_10361_25, aes(date, minutes, colour = variable))
p24 + geom_line()
dev.off()

png("../charts/10361_26.png", width=920, height=470, res=90)
p24 <- ggplot(traffic_stacked_10361_26, aes(date, minutes, colour = variable))
p24 + geom_line()
dev.off()

mean(traffic_10361_weekdays$travel_13, na.rm=TRUE)
mean(traffic_10361_weekdays$travel_14, na.rm=TRUE)
median(traffic_10361_weekdays$travel_13, na.rm=TRUE)
median(traffic_10361_weekdays$travel_14, na.rm=TRUE)

traffic_10361_weekdays$hour <- hour(traffic_10361_weekdays$time_14)

traffic_10361_morning <- filter(traffic_10361_weekdays, hour==8 | hour==9 | hour==10)
traffic_10361_morning <- filter(traffic_10361_morning, day==15 | day==16 | day==17 | day==18 | day==19 | day==22 | day==23 | day==24 | day ==25 | day==26)
traffic_10361_afternoon <- filter(traffic_10361_weekdays, hour==16 | hour==17 | hour==18)
traffic_10361_afternoon <- filter(traffic_10361_afternoon, day==15 | day==16 | day==17 | day==18 | day==19 | day==22 | day==23 | day==24 | day==25 | day==26)

mean(traffic_10361_morning$travel_13, na.rm=TRUE)
mean(traffic_10361_morning$travel_14, na.rm=TRUE)
median(traffic_10361_morning$travel_13, na.rm=TRUE)
median(traffic_10361_morning$travel_14, na.rm=TRUE)

mean(traffic_10361_afternoon$travel_13, na.rm=TRUE)
mean(traffic_10361_afternoon$travel_14, na.rm=TRUE)
median(traffic_10361_afternoon$travel_13, na.rm=TRUE)
median(traffic_10361_afternoon$travel_14, na.rm=TRUE)

#10360 specifics

traffic_stacked_10360 <- with(traffic_10360, data.frame(minutes = c(travel_13, travel_14), variable = factor(rep(c("Travel Time 2013","Travel Time 2014"),
                                                                                                                 each = NROW(traffic_10360))), date = rep(time_14, 2)))

traffic_stacked_10360$day <- day(traffic_stacked_10360$date)


traffic_stacked_10360_11 <- filter(traffic_stacked_10360, day==11)
traffic_stacked_10360_12 <- filter(traffic_stacked_10360, day==12)
traffic_stacked_10360_13 <- filter(traffic_stacked_10360, day==13)
traffic_stacked_10360_14 <- filter(traffic_stacked_10360, day==14)
traffic_stacked_10360_15 <- filter(traffic_stacked_10360, day==15)
traffic_stacked_10360_16 <- filter(traffic_stacked_10360, day==16)
traffic_stacked_10360_17 <- filter(traffic_stacked_10360, day==17)
traffic_stacked_10360_18 <- filter(traffic_stacked_10360, day==18)
traffic_stacked_10360_19 <- filter(traffic_stacked_10360, day==19)
traffic_stacked_10360_20 <- filter(traffic_stacked_10360, day==20)
traffic_stacked_10360_21 <- filter(traffic_stacked_10360, day==21)
traffic_stacked_10360_22 <- filter(traffic_stacked_10360, day==22)
traffic_stacked_10360_23 <- filter(traffic_stacked_10360, day==23)
traffic_stacked_10360_24 <- filter(traffic_stacked_10360, day==24)
traffic_stacked_10360_25 <- filter(traffic_stacked_10360, day==25)
traffic_stacked_10360_26 <- filter(traffic_stacked_10360, day==26)

png("../charts/10360_11.png", width=920, height=470, res=90)
p11 <- ggplot(traffic_stacked_10360_11, aes(date, minutes, colour = variable))
p11 + geom_line()
dev.off()

png("../charts/10360_12.png", width=920, height=470, res=90)
p12 <- ggplot(traffic_stacked_10360_12, aes(date, minutes, colour = variable))
p12 + geom_line()
dev.off()

png("../charts/10360_13.png", width=920, height=470, res=90)
p13 <- ggplot(traffic_stacked_10360_13, aes(date, minutes, colour = variable))
p13 + geom_line()
dev.off()

png("../charts/10360_14.png", width=920, height=470, res=90)
p14 <- ggplot(traffic_stacked_10360_14, aes(date, minutes, colour = variable))
p14 + geom_line()
dev.off()

png("../charts/10360_15.png", width=920, height=470, res=90)
p15 <- ggplot(traffic_stacked_10360_15, aes(date, minutes, colour = variable))
p15 + geom_line()
dev.off()

png("../charts/10360_16.png", width=920, height=470, res=90)
p16 <- ggplot(traffic_stacked_10360_16, aes(date, minutes, colour = variable))
p16 + geom_line()
dev.off()

png("../charts/10360_17.png", width=920, height=470, res=90)
p17 <- ggplot(traffic_stacked_10360_17, aes(date, minutes, colour = variable))
p17 + geom_line()
dev.off()

png("../charts/10360_18.png", width=920, height=470, res=90)
p18 <- ggplot(traffic_stacked_10360_18, aes(date, minutes, colour = variable))
p18 + geom_line()
dev.off()

png("../charts/10360_19.png", width=920, height=470, res=90)
p19 <- ggplot(traffic_stacked_10360_19, aes(date, minutes, colour = variable))
p19 + geom_line()
dev.off()

png("../charts/10360_20.png", width=920, height=470, res=90)
p20 <- ggplot(traffic_stacked_10360_20, aes(date, minutes, colour = variable))
p20 + geom_line()
dev.off()

png("../charts/10360_21.png", width=920, height=470, res=90)
p21 <- ggplot(traffic_stacked_10360_21, aes(date, minutes, colour = variable))
p21 + geom_line()
dev.off()

png("../charts/10360_22.png", width=920, height=470, res=90)
p22 <- ggplot(traffic_stacked_10360_22, aes(date, minutes, colour = variable))
p22 + geom_line()
dev.off()

png("../charts/10360_23.png", width=920, height=470, res=90)
p23 <- ggplot(traffic_stacked_10360_23, aes(date, minutes, colour = variable))
p23 + geom_line()
dev.off()

png("../charts/10360_24.png", width=920, height=470, res=90)
p24 <- ggplot(traffic_stacked_10360_24, aes(date, minutes, colour = variable))
p24 + geom_line()
dev.off()

png("../charts/10360_25.png", width=920, height=470, res=90)
p24 <- ggplot(traffic_stacked_10360_25, aes(date, minutes, colour = variable))
p24 + geom_line()
dev.off()

png("../charts/10360_26.png", width=920, height=470, res=90)
p24 <- ggplot(traffic_stacked_10360_26, aes(date, minutes, colour = variable))
p24 + geom_line()
dev.off()

mean(traffic_10360_weekdays$travel_13, na.rm=TRUE)
mean(traffic_10360_weekdays$travel_14, na.rm=TRUE)
median(traffic_10360_weekdays$travel_13, na.rm=TRUE)
median(traffic_10360_weekdays$travel_14, na.rm=TRUE)

traffic_10360_weekdays$hour <- hour(traffic_10360_weekdays$time_14)

traffic_10360_morning <- filter(traffic_10360_weekdays, hour==8 | hour==9 | hour==10)
traffic_10360_morning <- filter(traffic_10360_morning, day==15 | day==16 | day==17 | day==18 | day==19 | day==22 | day==23 | day==24 | day ==25 | day==26)
traffic_10360_afternoon <- filter(traffic_10360_weekdays, hour==16 | hour==17 | hour==18)
traffic_10360_afternoon <- filter(traffic_10360_afternoon, day==15 | day==16 | day==17 | day==18 | day==19 | day==22 | day==23 | day==24 | day==25 | day==26)

mean(traffic_10360_morning$travel_13, na.rm=TRUE)
mean(traffic_10360_morning$travel_14, na.rm=TRUE)
median(traffic_10360_morning$travel_13, na.rm=TRUE)
median(traffic_10360_morning$travel_14, na.rm=TRUE)

mean(traffic_10360_afternoon$travel_13, na.rm=TRUE)
mean(traffic_10360_afternoon$travel_14, na.rm=TRUE)
median(traffic_10360_afternoon$travel_13, na.rm=TRUE)
median(traffic_10360_afternoon$travel_14, na.rm=TRUE)

