# scenario affordability calculations 

library(ggplot2)

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


# now define a before and after period
before_year <- 2010
after_year  <- 2015

# label everything with a before year and an after year 
bills_fraction$period <- NA
bills_fraction$period[format(bills_fraction$date,"%Y")%in%before_year] <- "before"
bills_fraction$period[format(bills_fraction$date,"%Y")%in%after_year] <- "after"

# calculate affordability for the before period and after period and for each scenario and income class

library(reshape2)
bills_plotting <- melt(bills_fraction,id.vars = c("period","date","scenario"))
bills_plotting <- bills_plotting[!is.na(bills_plotting$period),]

# before vs afterbaselien vs after scenario1 
# keep: baseline before, baseline after, drought 1 after 
bills_plotting$period <- paste0(bills_plotting$period,"_",bills_plotting$scenario)
keep <- c("before_baseline","after_baseline","after_drought1")
bills_plotting <- bills_plotting[which(bills_plotting$period%in%keep),]

# rename scenarios
bills_plotting$period[which(bills_plotting$period=="before_baseline")] ="before"
bills_plotting$period[which(bills_plotting$period=="after_baseline")] ="after_baseline"
bills_plotting$period[which(bills_plotting$period=="after_drought1")] ="after_drought1"

# now just pull out the month 
bills_plotting$month <- format(bills_plotting$date,"%m")
bills_plotting$year <- format(bills_plotting$date,"%Y")

bills_plotting$period <- factor(bills_plotting$period,levels=c("before","after_baseline","after_drought1"))

bill_plot <- ggplot(bills_plotting,aes(y=value,x=variable,fill=period))+geom_boxplot()+theme_bw()+ylab("% of household income on water");bill_plot


# now for water use 
demands$date <- bills$date
demands <- demands[,-1]
demands[,c(1:16)] <- demands[,c(1:16)]/748
demands$period[format(demands$date,"%Y")%in%before_year] <- "before"
demands$period[format(demands$date,"%Y")%in%after_year] <- "after"

# now also calculate gallons per person 
individual_demands <- demands

for(j in 1:16){
  individual_demands[,j] <- demands[,j]*748/(30.4*hh_sizes[j])
}

# this is in gallons per person 
demand_plotting <- melt(demands,id.vars = c("period","date","scenario"))
demand_plotting <- demand_plotting[complete.cases(demand_plotting),]

demand_plotting$period <- paste0(demand_plotting$period,"_",demand_plotting$scenario)
keep <- c("before_baseline","after_baseline","after_drought1")
demand_plotting <- demand_plotting[which(demand_plotting$period%in%keep),]

demand_plotting$period <- factor(demand_plotting$period,levels=c("before_baseline","after_baseline","after_drought1"))

demand_plot <- ggplot(demand_plotting,aes(y=value,x=variable,fill=period))+geom_boxplot()+theme_bw()+ylab("Monthly Household Water Use (CCFs)");demand_plot

# and again for individual demands -----------
individual_demand_plotting <- melt(individual_demands,id.vars = c("period","date","scenario"))
individual_demand_plotting <- individual_demand_plotting[complete.cases(individual_demand_plotting),]

individual_demand_plotting$period <- paste0(individual_demand_plotting$period,"_",individual_demand_plotting$scenario)
keep <- c("before_baseline","after_baseline","after_drought1")
individual_demand_plotting <- individual_demand_plotting[which(individual_demand_plotting$period%in%keep),]

individual_demand_plotting$period <- factor(individual_demand_plotting$period,levels=c("before_baseline","after_baseline","after_drought1"))

individual_demand_plot <- ggplot(individual_demand_plotting,aes(y=value,x=variable,fill=period))+geom_boxplot()+theme_bw()+ylab("Monthly Individual Water Use");individual_demand_plot
e

