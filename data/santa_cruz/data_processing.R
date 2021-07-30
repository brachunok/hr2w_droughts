# EDA on the water rights data
library(stringr)
setwd("~/Documents/__college/reseach_stuff/hr2w_droughts/data/santa_cruz/")

# read in the North Coast Creeks

d1 <- read.csv("water_right_progress_reportS002043.csv")
d1 <- d1[,c(1:3)]
names(d1)<- c("year","month","volume2043")
#drop 088
d1 <- d1[which(d1$year!=2008),]

d2 <- read.csv("water_right_progress_reportS002042.csv") 
d2 <- d2[,c(1:3)]
names(d2)<- c("year","month","volume2042")
d2 <- d2[which(d2$year!=2008),]


d3 <- read.csv("water_right_progress_reportS008610.csv") 
d3 <- d3[,c(1:3)]
names(d3)<- c("year","month","volume8610")
d3 <- d3[complete.cases(d3),]
d3 <- d3[-133,]

d4 <- read.csv("water_right_progress_reportS002044.csv") # the AR one
d4 <- d4[,c(1:3)]
names(d4)<- c("year","month","volume2044")
d4 <- d4[which(d4$year!=2008),]


# Read in the Feltotn Diversions

d5 <- read.csv("water_right_progress_report16601.csv") # the AR one
d5 <- d5[,c(1:3)]
names(d5)<- c("year","month","volume16601")
d5 <- d5[complete.cases(d5),]

d6 <- read.csv("water_right_progress_report16123.csv")
d6 <- d6[,c(1:3)]
names(d6)<- c("year","month","volume16123")
d6 <- d6[which(d6$year!=2008),]
d6 <- d6[which(d6$year!=2020),]


# read in the Tait Street diversions
d8 <- read.csv("water_right_progress_report7200.csv")
d8 <- d8[c(1:3)]
d8 <- d8[which(d8$Year!=2008),]
d8 <- d8[complete.cases(d8),]
names(d8) <- c("year","month","volume7200")

d9 <- read.csv("water_right_progress_report1553.csv")
d9 <- d9[c(1:3)]
d9 <- d9[which(d9$Year!=2008),]
d9 <- d9[complete.cases(d9),]
names(d9) <- c("year","month","volume1553")

# newell creek
d7 <- read.csv("water_right_progress_report9847.csv")
# do some checks on d7
#plot(d7[,3],type="l")
#lines(d7[,4],type="l")
#lines(d7[,5],type="l",col="red")
# ASSUMPTION FOR NOW:
# add the first 2 columns to get total amount diverted 
# or collected to storage 
d7[is.na(d7[,4]),4] <- 0
d7[,3] <- d7[,3]+d7[,4]

d7 <-d7[,c(1:3,5)]
names(d7) <- c("year","month","volume9847","newellUsed")
d7 <- d7[which(d7$year!=2008),]
d7 <- d7[which(d7$year!=2020),]

# this one we know is in AF so divide by 3 to get the MG value
d7$volume9847 <- d7$volume9847/3.06888785
d7$newellUsed <- d7$newellUsed/3.06888785

# now combine them
df <- d1
df$month <- match(df$month,month.name)
df$datetime <- paste0(df$year,"-",str_pad(string = df$month,width = 2,pad = "0",side = "left"),"-","01")
df$datetime <- as.Date(df$datetime,format = "%Y-%m-%d")

# now add all the relavent columns 
df <- cbind(df,d2$volume2042,d3$volume8610,d4$volume2044,d5$volume16601,
            d6$volume16123,d7$volume9847,d8$volume7200,d9$volume1553,d7$newellUsed)

# clean up the GW column
# goal is to have it be one distribution of GW in Jnauary and the rest are 0s 


# now clean up DF to be in a nice order and remove the junk
#df <- df[,c(4,3,5:10)]
df$year <- as.numeric(format(df$datetime,'%Y'))

# north coast is 2042,2043,2044,8610
df_sources <- data.frame(date= df$datetime)
df_sources$northCoast <- (df$`d2$volume2042`+df$volume2043+df$`d4$volume2044`+df$`d3$volume8610`)/3.06888785
# make sure it's converted to MG

# Tait Street diersions 1553,7200
df_sources$taitStreet <- (df$`d8$volume7200`+df$`d9$volume1553`)/3.06888785

# Felton Diversions to Loch Lomond
df_sources$feltonDiversions <- (df$`d5$volume16601`+df$`d6$volume16123`)/3.06888785

# Newell Creek (Lomond Outflow)
df_sources$newellInflow <- df$`d7$volume9847`
df_sources$newellUsed   <- df$`d7$newellUsed`

#write.csv(df_sources,file="~/Documents/__college/reseach_stuff/hr2w_droughts/data/santa_cruz/sc_input_data_baseline.csv")

# break these down into time serieses ------------------------------------------
nc_ts <- ts(df_sources[,c(2)],frequency = 12)
nc_ts_components <- decompose(nc_ts)

ts_ts <- ts(df_sources$taitStreet,frequency = 12)
ts_ts_components <- decompose(ts_ts)

ll_ts <- ts(df_sources$newellInflow+df_sources$feltonDiversions,frequency = 12)
ll_ts_components <- decompose(ll_ts)

# "non drought" years: 2010
nd_rows <- which(format(df_sources$date,"%Y")%in%c("2010"))
#nd_rows_ts <-
# "Drought" years: 2015
d_rows <- which(format(df_sources$date,"%Y")%in%c("2013","2014","2015","2016"))

# seasonally adjust each -------------------------------------------------------
nc_ts_sa <- nc_ts - nc_ts_components$seasonal 
ts_ts_sa <- ts_ts - ts_ts_components$seasonal
ll_ts_sa <- ll_ts - ll_ts_components$seasonal

#  Calculate a 'baseline' mean 

nc_ts_baseline <- mean(nc_ts_sa[nd_rows])
ts_ts_baseline <- mean(ts_ts_sa[nd_rows])
ll_ts_baseline <- mean(ll_ts_sa[nd_rows])

nc_ts_sa <- nc_ts_sa - nc_ts_baseline
ts_ts_sa <- ts_ts_sa - ts_ts_baseline
ll_ts_sa <- ll_ts_sa - ll_ts_baseline

# now make a drought of greater intensity 
reduction_factor <- .5
nc_ts_sa_intense <- nc_ts_sa
nc_ts_sa_intense[d_rows] <- nc_ts_sa[d_rows] - abs(nc_ts_sa[d_rows])*reduction_factor

ts_ts_sa_intense <- ts_ts_sa
ts_ts_sa_intense[d_rows] <- ts_ts_sa[d_rows]- abs(ts_ts_sa[d_rows])*reduction_factor



ll_ts_sa_intense <- ll_ts_sa
ll_ts_sa_intense[d_rows] <- ll_ts_sa[d_rows]-abs(ll_ts_sa[d_rows])*reduction_factor


# re-introduce the seasonality
nc_ts_intense <- nc_ts_sa_intense+nc_ts_components$seasonal + nc_ts_baseline

ll_ts_intense <- ll_ts_sa_intense + ll_ts_components$seasonal + ll_ts_baseline
ll_ts_intense[which(ll_ts_intense<=0)] <- 0

ts_ts_intense <- ts_ts_sa_intense + ts_ts_components$seasonal + ts_ts_baseline

# now make plots 
plot(nc_ts_intense,col="red")
lines(nc_ts,col='black')

plot(ts_ts_intense,col="red")
lines(ts_ts,col="black")

plot(ll_ts_intense,col="red")
lines(ll_ts,col="black")

df_sources_intense <- df_sources
df_sources_intense$northCoast <- as.numeric(nc_ts_intense)
df_sources_intense$taitStreet <- as.numeric(ts_ts_intense)
df_sources_intense$newellInflow <- as.numeric(nc_ts_intense)


# baseline looks like this: 
# 2009,10,11,12 -- 14,15,16 --2009,10,11,12

pre_years  <- which(format(df_sources$date,"%Y")%in%c("2009","2010","2011","2012"))
mid_years  <- which(format(df_sources$date,"%Y")%in%c("2014","2015"))
post_years <- which(format(df_sources$date,"%Y")%in%c("2009","2010","2011","2012"))
post_years_long <- which(format(df_sources$date,"%Y")%in%c("2009","2010"))

df_sources_baseline <- df_sources[c(pre_years,mid_years,post_years),]
# intense looks like this
#  2009,10,11,12 --14a,15a,16a --2009,10,11,12

df_sources_intense<- rbind(df_sources[c(pre_years),],df_sources_intense[mid_years,],df_sources[post_years,])

# long looks like this
#  2009,10,11,12 --14,15,16,14,15,16 --2009
df_sources_long <- rbind(df_sources[c(pre_years),],df_sources[mid_years,],df_sources[mid_years,],df_sources[post_years_long,])

# long+ intense looks like this
# 2009,10,11,12 --14a,15a,16a,14a,15a,16a -- 2009
df_sources_long_intense <- rbind(df_sources[c(pre_years),],df_sources_intense[mid_years,],df_sources_intense[mid_years,],df_sources[post_years_long,])

# now write appropriate dates for each and write them to files.
df_sources_baseline$date <- df_sources$date[1:120]
df_sources_intense$date <- df_sources$date[1:120]
df_sources_long$date <- df_sources$date[1:120]
df_sources_long_intense$date <- df_sources$date[1:120]

# sanity check to change anything below 0 to 0 because
# soomehow it's possible for things to get <0 using the above code

df_sources_baseline[df_sources_baseline<0] <- 0
df_sources_long[df_sources_long<0] <- 0
df_sources_intense[df_sources_intense<0] <- 0
df_sources_long_intense[df_sources_long_intense<0] <-0

write.csv(df_sources_baseline,file="~/Documents/__college/reseach_stuff/hr2w_droughts/data/santa_cruz/generated_drought_scenarios/baseline.csv")
write.csv(df_sources_intense,file="~/Documents/__college/reseach_stuff/hr2w_droughts/data/santa_cruz/generated_drought_scenarios/intense.csv")
write.csv(df_sources_long,file="~/Documents/__college/reseach_stuff/hr2w_droughts/data/santa_cruz/generated_drought_scenarios/long.csv")
write.csv(df_sources_long_intense,file="~/Documents/__college/reseach_stuff/hr2w_droughts/data/santa_cruz/generated_drought_scenarios/long_intense.csv")


# make a few plots 
plot(df_sources_intense$taitStreet~df_sources_baseline$date,type="l",col="red")
lines(df_sources_long$taitStreet~df_sources_baseline$date,type="l",col="blue")
lines(df_sources_baseline$taitStreet~df_sources_baseline$date,type="l",col="black")
lines(df_sources_long_intense$taitStreet~df_sources_baseline$date,type="l",col="green")



