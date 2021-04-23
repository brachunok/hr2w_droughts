# EDA on the water rights data
library(stringr)
setwd("~/Documents/__college/reseach_stuff/hr2w_droughts/data/sacramento/water_rights_data/")

# read in teh AR data file 
d1 <- read.csv("water_right_progress_report_000992.csv") # the AR one
names(d1)<- c("year","month","SR","usage","groundwater","notes")

# remove the weird tab after september...
d1$month <- trimws(d1$month,"r")

# make 'df' as the primary dataframe 
df <- d1[,-c(4,6)]

# convert the date in DF 
df$month <- match(df$month,month.name)
df$datetime <- paste0(df$year,"-",str_pad(string = df$month,width = 2,pad = "0",side = "left"),"-","01")
df$datetime <- as.Date(df$datetime,format = "%Y-%m-%d")

# clean up the GW column
# goal is to have it be one distribution of GW in Jnauary and the rest are 0s 
df$groundwater[is.na(df$groundwater)] <- 0
# now read in the other datafiles and add them to DF                           
d2 <- read.csv("water_right_progress_report_11358.csv")
df$AR58 <- d2$Amt.directly.diverted.or.collected.to.sotrage..AF.                      

d3 <- read.csv("water_right_progress_report_11359.csv")
df$AR59 <- d3$Amt.directly.diverted.or.collected.to.sotrage..AF.

d4 <- read.csv("water_right_progress_report_11360.csv")
df$AR60 <- d4$Amt.directly.diverted.or.collected.to.sotrage..AF.

d5 <- read.csv("water_right_progress_report_11361.csv")
df$AR61 <- d5$Amt.directly.diverted.or.collected.to.sotrage..AF.


# now clean up DF to be in a nice order and remove the junk
df <- df[c(5,3,6:9,4)]
df$year <- as.numeric(format(df$datetime,'%Y'))
df$ARtotal <- df$AR58+df$AR59 + df$AR60 + df$AR61

df$surfaceTotal <- df$ARtotal+df$SR
# do some aggregation to see how the totals line up
yearly_totals <- aggregate(cbind(df$ARtotal,df$SR,df$groundwater),by=list(df$year),FUN="sum")
names(yearly_totals) <- c("year","americanRiver","sacramentoRiver","groundwater")
yearly_totals$totalSurface <- yearly_totals$americanRiver+yearly_totals$sacramentoRiver

# in 2015 the agency actually used
# 39,511 AF from SR
# 30,956 AF from AR
# 13,706 AF from GW 

# stacked bar of SR, AR and GW 
library(reshape2)

# make GW 1/12 of the annual values
df_gw12 <- df

for(i in 1:((nrow(df_gw12))/12)){
  gw <- df$groundwater[1+(i-1)*12]
  print(gw)
  val <- gw/12
  df_gw12$groundwater[c((1+(i-1)*12):(i*12))] <- val
}

df_melted <- melt(df_gw12,id.vars = "datetime",measure.vars = c("SR","ARtotal","groundwater"))
df_melted$variable <- factor(df_melted$variable,levels=c("ARtotal","SR","groundwater"))
library(ggplot2)
p1 <- ggplot(df_melted,aes(x=datetime,y=value,fill=variable))+geom_area()
library(viridis)
library(hrbrthemes)
p1 + geom_area(alpha=0.6 , size=.5, colour="white") +
  scale_fill_viridis(discrete = T) +
  theme_ipsum() 
