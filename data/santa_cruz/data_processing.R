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

write.csv(df_sources,file="~/Documents/__college/reseach_stuff/hr2w_droughts/data/santa_cruz/sc_input_data.csv")
