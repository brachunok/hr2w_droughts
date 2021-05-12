#EDA on sc outputs

library(ggplot2)

bills <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/hh_bills.csv")
demands <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/hh_demand.csv")
outputs <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/outputs.csv")
outputs$Date <- as.Date(as.character(outputs$Date))

library('ggplot2')
library('reshape2')
bills$date = outputs$Date
bills$type = "bill"
demands$date = outputs$Date
demands$type="demand"
hh = rbind(bills,demands)
par(mfrow=c(2,1))
plot(outputs$level~outputs$Date,type="l")
plot(outputs$deficit~outputs$Date,type="l")



# figure otu when we built the res and draw a vertical line
#vl = outputs$Date[which(outputs$trigger=="YES")]

# melt for easier plotting
hh = melt(hh[,-1],id= c("date","type"))
hh$date <- as.Date(as.character(hh$date))
p = ggplot(hh,aes(x=date,y=value,color=variable))+ 
  geom_line()+
  facet_grid(rows=vars(type),scales = "free_y")+
  scale_color_brewer(palette="RdYlBu")+theme_bw()+
  geom_vline(xintercept = as.Date(vl),linetype="dashed",color = "red", size=1)
p
