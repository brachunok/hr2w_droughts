# affordability-access plots 
library(reshape2)
library('ggplot2')

#|      /
#|    /   *   
#|  /  *         *    Vertical axis is CCFs/month
#|/  -  -  -  -  -
# ---------------- 
# MHI
# read and process data --------------------------------------------------------
# for now, just make an ad-hoc dataframe 
bl_bills <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/baseline/hh_bills.csv")
bl_demands <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/baseline/hh_demand.csv")
bl_outputs <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/baseline/outputs.csv")
bl_outputs$Date <- as.Date(as.character(bl_outputs$Date))

d1_bills <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/drought1/hh_bills.csv")
d1_demands <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/drought1/hh_demand.csv")
d1_outputs <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/drought1/outputs.csv")
d1_outputs$Date <- as.Date(as.character(d1_outputs$Date))

# now label 
bl_bills$scenario = "baseline"
bl_demands$scenario = "baseline"
bl_outputs$scenario = "baseline"

d1_bills$scenario = "drought1"
d1_demands$scenario = "drought1"
d1_outputs$scenario = "drought1"

# now combine them 
bills <- rbind(bl_bills,d1_bills)
demands <- rbind(bl_demands,d1_demands)
outputs <- rbind(bl_outputs,d1_outputs)

# change bills into % of income 
bills <- bills[,-1]
bills$date <- outputs$Date
incomes <- c(7500,12500,17500,22500,27500,32500,37500,42500,47500,55000,67500,87500,112500,137500,175000,250000)
hh_sizes <- c(1,3,3,3,1,1,1.67,1.4,1,1.57,1.86,3.17,3,3.57,4.58,5.38)
bills_fraction <- bills
# for each column, divide each value by 1/12th of the income 
for (j in 1:length(incomes)){
  bills_fraction[,j] <- bills[,j]/(incomes[j]/12)*100
}

# now combine bills and deamnds to make the affordability/access plot
bills_fraction$measurement <- "perc_income"
demands$measurement <- "household_demand"
demands <- demands[,-1]
demands$date <- bills_fraction$date
plot_data <- rbind(demands,bills_fraction)

# now do all the scenario ren-naming in plot data

# now define a before and after period
before_year <- 2010
after_year  <- 2015

# label everything with a before year and an after year 
plot_data$period <- NA
plot_data$period[format(plot_data$date,"%Y")%in%before_year] <- "before"
plot_data$period[format(plot_data$date,"%Y")%in%after_year] <- "after"

# calculate affordability for the before period and after period and for each scenario and income class
plot_data <- melt(plot_data,id.vars = c("period","date","scenario","measurement"))
plot_data <- plot_data[!is.na(plot_data$period),]

# before vs afterbaselien vs after scenario1 
# keep: baseline before, baseline after, drought 1 after 
plot_data$period <- paste0(plot_data$period,"_",plot_data$scenario)
keep <- c("before_baseline","after_baseline","after_drought1")
plot_data <- plot_data[which(plot_data$period%in%keep),]

# rename scenarios
plot_data$period[which(plot_data$period=="before_baseline")] ="before"
plot_data$period[which(plot_data$period=="after_baseline")] ="after_baseline"
plot_data$period[which(plot_data$period=="after_drought1")] ="after_drought1"

# now just pull out the month 
#bills_plotting$month <- format(bills_plotting$date,"%m")
#bills_plotting$year <- format(bills_plotting$date,"%Y")

plot_data$period <- factor(plot_data$period,levels=c("before","after_baseline","after_drought1"))
# now, recast measurement into a new column (this is absurd but i can't figure out how else to do it)--
plot_data <- dcast(plot_data,formula = period+date+scenario+variable ~measurement)

# now make the plot ------------------------------------------------------------
p1 <- ggplot(plot_data,aes(x=household_demand/748,y=perc_income,color=variable,group=variable))+geom_line()+facet_grid(cols=vars(period))+
  theme_bw();p1

