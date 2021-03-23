# validation for initial sacramento data against benecia, glendale, elsinore data

# they had fixed-threshold drought charges but 
# the thresholds were 0-1 CCF per month 

# so let's see if its similar

# read in my system overview file
bills <- read.csv("~/Documents/__college/reseach_stuff/water-equity/outputs/hh_bills.csv")
demands <- read.csv("~/Documents/__college/reseach_stuff/water-equity/outputs/hh_demand.csv")
outputs <- read.csv("~/Documents/__college/reseach_stuff/water-equity/outputs/outputs.csv")

library('ggplot2')
library('reshape2')
bills$date = outputs$Date
bills$type = "bill"
demands$date = outputs$Date
demands$type="demand"
hh = rbind(bills,demands)
# figure otu when we built the res and draw a vertical line
vl = outputs$Date[which(outputs$trigger=="YES")]

# melt for easier plotting
hh$date <- as.Date(as.character(hh$date))
hh_melted <- melt(hh[,-1],id.vars = c("date","type"))
hh_melted$date <- as.Date(as.character(hh_melted$date))
p <- ggplot(hh_melted,aes(x=date,y=value,color=variable))+ 
  geom_line()+
  facet_grid(rows=vars(type),scales = "free_y")+
  scale_color_brewer(palette="RdYlBu")+theme_bw()+
  geom_vline(xintercept = as.Date(vl),linetype="dashed",color = "red", size=1)
p

# look before/after the conservation
# in this stylized test case look at 2020 the year vs 2023 the year 
data <-  hh[which(hh$date<as.Date("2021-01-01")|hh$date>="2023-12-31"),]
data <- data[which(data$type=="bill"),]
data$sample <- "before"
data$sample[which(data$date>="2023-12-31")] <- "after"

# now divide by household incomes
incomes <- c(12500,17250,30000,42500,63500,87500,125000,175000)
incomes <- incomes/12
for (j in c(2:9)){
  data[,j] <- data[,j]/incomes[j]
  
}

# compare before vs after 

# line them up in an ad-hoc way

difference <- data[c(1:12),]
difference[c(1:12),c(2:8)] <- data[c(13:24),c(2:8)]-data[c(1:12),c(2:8)]

par(mfrow=c(3,3))
for(j in c(2:9)){
  
  plot(difference[,j],ylim = c(-.005,.005))
}

# make outputs for the overall model
plot(outputs$level~outputs$Date,type="l")
