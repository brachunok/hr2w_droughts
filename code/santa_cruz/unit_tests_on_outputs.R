# Unit Tests in R 
library(ggplot2)

# plot utility cost per month and compare against the shortfalls they predict (from rate study, in 
# weekly report)

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

# financial deficit plot
#plot(revenueLost~Date,data=outputs[which(outputs$revenueLost>0),],type="l")

#from UWMP
# 600k/yr for a stage 1 --> 50k/month
# 5.8millino/yr for a stage 5
# expecting 2.9mil/yr for STAGE 3

monthly_lost_revenues = table(outputs$revenueLost,outputs$conserveStatus)
monthly_lost_revenues

# drought cost recovery fee: 3/4" is 7.37/month
fixed_costs = outputs$fixedCharge
unique(fixed_costs)-10.99

