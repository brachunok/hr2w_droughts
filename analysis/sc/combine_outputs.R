library(ggplot2)
library(ggExtra)
library(reshape2)


scenarios <- c("baseline","drought_custom")#,"drought1","drought_custom_hh_fixed","baseline_hh_fixed")
names <- c("Baseline","Drought12")#"Drought15","Drought12_fixedHH","Baseline_Fixed_HH")

before_year= 2010
after_year = 2015
CPM = 34000

bills = data.frame()
demands = data.frame()
outputs = data.frame()
for (j in 1:length(scenarios)){
 
  this_outputs <- read.csv(paste0("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/",scenarios[j],"/outputs.csv"))
  this_outputs$scenario <- names[j]
  outputs <- rbind(outputs,this_outputs)

  this_bills <- read.csv(paste0("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/",scenarios[j],"/hh_bills.csv"))
  this_bills$scenario <- names[j]
  this_bills$date <- as.Date(this_outputs$Date)
  bills <- rbind(bills,this_bills)
  
  this_demand <- read.csv(paste0("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/",scenarios[j],"/hh_demand.csv"))
  this_demand$scenario <- names[j]
  this_demand$date <- as.Date(this_outputs$Date)
  demands <- rbind(demands,this_demand)
  
}

# determine the before/after for each 
bills$period <- NA
demands$period <- NA
outputs$period <- NA

# label before/after
demands$period[format(demands$date,"%Y")%in%before_year] <- "before"
demands$period[format(demands$date,"%Y")%in%after_year] <- "after"
bills$period[format(bills$date,"%Y")%in%before_year] <- "before"
bills$period[format(bills$date,"%Y")%in%after_year] <- "after"
outputs$period[format(outputs$date,"%Y")%in%before_year] <- "before"
outputs$period[format(outputs$date,"%Y")%in%after_year] <- "after"

# make a column for the 'trial' --> before/after periods
demands$trial <- paste0(demands$period,"_",demands$scenario)
bills$trial <-  paste0(bills$period,"_",bills$scenario)
outputs$trial <- paste0(outputs$period,"_",outputs$scenario)

# ad-hoc changes 
bills <- bills[,-which(colnames(bills)=="X")]

# write the files here
save(bills,file="~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/processed_bills.Rdata")
save(demands,file="~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/processed_demands.Rdata")
save(outputs,file="~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/processed_outputs.Rdata")
rm(list=ls())
