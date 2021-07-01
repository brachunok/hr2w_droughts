# scenario affordability calculations 

library(ggplot2)
library(ggExtra)
library(reshape2)

# for now, just make an ad-hoc dataframe 
bl_bills <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/baseline/hh_bills.csv")
bl_demands <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/baseline/hh_demand.csv")
bl_outputs <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/baseline/outputs.csv")
bl_outputs$Date <- as.Date(as.character(bl_outputs$Date))

# 15% the whole time
d1_bills <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/drought1/hh_bills.csv")
d1_demands <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/drought1/hh_demand.csv")
d1_outputs <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/drought1/outputs.csv")
d1_outputs$Date <- as.Date(as.character(d1_outputs$Date))

# 12% the whole time
d2_bills <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/drought_custom/hh_bills.csv")
d2_demands <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/drought_custom/hh_demand.csv")
d2_outputs <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/drought_custom/outputs.csv")
d2_outputs$Date <- as.Date(as.character(d2_outputs$Date))

# 12% the whole time, fixed household size 
d3_bills <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/drought_custom_hh_fixed/hh_bills.csv")
d3_demands <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/drought_custom_hh_fixed/hh_demand.csv")
d3_outputs <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/drought_custom_hh_fixed/outputs.csv")
d3_outputs$Date <- as.Date(as.character(d3_outputs$Date))

# baseline with alternative HH size 
bl_hh_bills <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/baseline_hh_fixed/hh_bills.csv")
bl_hh_demands <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/baseline_hh_fixed/hh_demand.csv")
bl_hh_outputs <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/baseline_hh_fixed/outputs.csv")
bl_hh_outputs$Date <- as.Date(as.character(bl_hh_outputs$Date))

# now label 
bl_bills$scenario = "baseline"
bl_demands$scenario = "baseline"
bl_outputs$scenario = "baseline"

# alternative basline
bl_hh_bills$scenario <- "baseline_fixed_hh"
bl_hh_demands$scenario <- "baseline_fixed_hh"
bl_hh_outputs$scenario <- "baseline_fixed_hh"

d1_bills$scenario = "drought15"
d1_demands$scenario = "drought15"
d1_outputs$scenario = "drought15"

d2_bills$scenario = "drought_12"
d2_demands$scenario = "drought_12"
d2_outputs$scenario = "drought_12"

d3_bills$scenario = "drought_12_fixed_hh"
d3_demands$scenario = "drought_12_fixed_hh"
d3_outputs$scenario = "drought_12_fixed_hh"


# now combine them 
bills <- rbind(bl_bills,bl_hh_bills,d1_bills,d2_bills,d3_bills)
demands <- rbind(bl_demands,bl_hh_outputs,d1_demands,d2_demands,d3_demands)
outputs <- rbind(bl_outputs,bl_hh_outputs,d1_outputs,d2_outputs,d3_outputs)

# change bills into % of income 
bills <- bills[,-1]
bills$date <- outputs$Date
incomes <- c(7500,12500,17500,22500,27500,32500,37500,42500,47500,55000,67500,87500,112500,137500,175000,250000)
hh_sizes2 <- c(2.72,2.72,2.72,2.72,2.72,2.72,2.72,2.72,2.72,2.72,2.72,2.72,2.72,2.72,2.72,2.72)
hh_sizes <- c(1,3,3,3,1,1,1.67,1.4,1,1.57,1.86,3.17,3,3.57,4.58,5.38)
hh_counts <- c(2365,1648,1456,1285,1424,1027,1018,1077,874,1931,2352,4064,3110,2091,3263,4895)
bills_fraction <- bills
# for each column, divide each value by 1/12th of the income 
for (j in 1:length(incomes)){
  bills_fraction[,j] <- bills[,j]/(incomes[j]/12)*100
}


# make figures of water bill for 47.5k and for 137.5k 
selected_incomes <- bills[,c(9,13,17,18)]
selected_incomes_plot <- melt(selected_incomes,id.vars = c("date","scenario"))

xmin_list_baseline <- as.Date(c("2009-05-01","2012-05-01","2013-05-01","2014-04-01"))
xmax_list_baseline <- as.Date(c("2009-10-01","2012-10-01","2013-12-01","2015-12-01"))

xmin_list_d1 <- as.Date(c("2009-05-01","2013-05-01"))
xmax_list_d1 <- as.Date(c("2009-10-01","2015-12-01"))


# just plot before/during drought
selected_incomes_plot <- selected_incomes_plot[which(selected_incomes_plot$date<=as.Date("2016-01-01")),]
income_plot_one <- ggplot()+geom_line(data= selected_incomes_plot[which(selected_incomes_plot$variable=="X47500"),],aes(x=date,y=value,color=scenario))+facet_grid(rows=vars(variable),scales = "free")+
  theme_bw()+ylab("Monthly Water Bill") + scale_color_manual(values = c("darkgray","darkred","darkblue","orange","purple"))+
  annotate("rect",xmin=xmin_list_baseline,xmax=xmax_list_baseline,ymin=20,ymax=22.5,alpha=0.2,fill=c("orange","green","green","red"))+
  annotate("rect",xmin=xmin_list_d1,xmax=xmax_list_d1,ymin=22.5,ymax=25,alpha=0.2,fill=c("orange","orange"));income_plot_one


income_plot_two <- ggplot()+geom_line(data= selected_incomes_plot[which(selected_incomes_plot$variable!="X47500"),],aes(x=date,y=value,color=scenario))+facet_grid(rows=vars(variable),scales = "free")+
  theme_bw()+ylab("Monthly Water Bill") + scale_color_manual(values = c("darkgray","darkred","darkblue","orange","purple"))+
  annotate("rect",xmin=xmin_list_baseline,xmax=xmax_list_baseline,ymin=55,ymax=57.5,alpha=0.2,fill=c("orange","green","green","red"))+
  annotate("rect",xmin=xmin_list_d1,xmax=xmax_list_d1,ymin=57.5,ymax=60,alpha=0.2,fill=c("orange","orange"));income_plot_two


# now define a before and after period
before_year <- 2010
after_year  <- 2015

# label everything with a before year and an after year 
bills_fraction$period <- NA
bills_fraction$period[format(bills_fraction$date,"%Y")%in%before_year] <- "before"
bills_fraction$period[format(bills_fraction$date,"%Y")%in%after_year] <- "after"

# calculate affordability for the before period and after period and for each scenario and income class
bills_plotting <- melt(bills_fraction,id.vars = c("period","date","scenario"))
bills_plotting <- bills_plotting[!is.na(bills_plotting$period),]

# before vs afterbaselien vs after scenario1 
# keep: baseline before, baseline after, drought 1 after 
bills_plotting$period <- paste0(bills_plotting$period,"_",bills_plotting$scenario)
keep <- c("after_drought_12_fixed_hh","before_baseline_fixed_hh","after_baseline_fixed_hh")
bills_plotting <- bills_plotting[which(bills_plotting$period%in%keep),]

# rename scenarios
bills_plotting$period[which(bills_plotting$period=="before_baseline")] ="before"
#bills_plotting$period[which(bills_plotting$period=="after_baseline")] ="after_baseline"
#bills_plotting$period[which(bills_plotting$period=="after_drought1")] ="after_drought1"
#bills_plotting$period[which(bills_plotting$period=="after_baseline")] ="after_baseline"
#bills_plotting$period[which(bills_plotting$period=="after_drought1")] ="after_drought1"

# now just pull out the month 
bills_plotting$month <- format(bills_plotting$date,"%m")
bills_plotting$year <- format(bills_plotting$date,"%Y")

#bills_plotting$period <- factor(bills_plotting$period,levels=c("before","after_baseline","after_drought1"))

MHI = 61000
CPM = 35923


# figure for sarah for SIL presentation
deep_poverty <- CPM/2

deep_poverty_bills <- bills[,c(1:3,17,18)]
deep_poverty_bills$average <- rowMeans(deep_poverty_bills[,c(1:3)])
deep_poverty_bills$average_perc <- deep_poverty_bills$average/(deep_poverty/12)*100

# make a before and after
deep_poverty_bills$period <- NA
deep_poverty_bills$period[format(deep_poverty_bills$date,"%Y")%in%before_year] <- "before"
deep_poverty_bills$period[format(deep_poverty_bills$date,"%Y")%in%after_year] <- "after"
deep_poverty_bills <- deep_poverty_bills[complete.cases(deep_poverty_bills),]

# merge terms 
deep_poverty_bills$scenario <- paste0(deep_poverty_bills$period,"_",deep_poverty_bills$scenario)
deep_poverty_bills <- deep_poverty_bills[which(deep_poverty_bills$scenario%in%keep),]

# rename the periods
deep_poverty_bills$period[which(deep_poverty_bills$scenario=="before_baseline_fixed_hh")]<- "Pre-Drought"
deep_poverty_bills$period[which(deep_poverty_bills$scenario=="after_baseline_fixed_hh")]<- "Drought, Historical"
deep_poverty_bills$period[which(deep_poverty_bills$scenario=="after_drought_12_fixed_hh")]<- "Drought, Improved"
#deep_poverty_bills <- deep_poverty_bills[!which(deep_poverty_bills$period%in%c("after_drought15","after_drought_12_fixed_hh")),]

deep_poverty_bills$period <- factor(deep_poverty_bills$period,levels=c("Pre-Drought","Drought, Historical","Drought, Improved"))


dpb_plot <- ggplot(deep_poverty_bills,aes(y=average_perc,x=period,fill=period))+geom_boxplot()+scale_fill_brewer()
dpb_plot+theme_bw()+ylab("Percent of Household Income Spent on Water")+xlab("")+ggtitle("Water Bill Affordability for Households in Deep Poverty")
