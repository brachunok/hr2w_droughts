# plot SES results

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
hh = melt(hh[,-1])
hh$date <- as.Date(as.character(hh$date))
p = ggplot(hh,aes(x=date,y=value,color=variable))+ 
  geom_line()+
  facet_grid(rows=vars(type),scales = "free_y")+
  scale_color_brewer(palette="RdYlBu")+theme_bw()+
  geom_vline(xintercept = as.Date(vl),linetype="dashed",color = "red", size=1)
p
