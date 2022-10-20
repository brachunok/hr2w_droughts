
# for calculating the statistics which compare the 'best' option for utilities vs the 'best' option for low and high income households 


library("scales")
library("ggsci")
library(cowplot)
library('ggpattern')

options(scipen=999)
load("~/Documents/__college/reseach_stuff/hr2w_droughts/analysis/processed_and_binned_bill_data_plot3.Rdata")
og_outputs <- outputs
elasticities_values <- unique(outputs$price_elasticity)

#for (ela in elasticities_values){
  outputs <- og_outputs[which(og_outputs$price_elasticity==0.1),]
  
  #desal and GRRP-R are the non-dominated solutions based on capacity and LCW
  
  # Now make some figures
  library(reshape2)
  library(ggplot2)
  
  # going to melt the responses 
  outputs_totals <- outputs[,c(1:12,44:48,161:165)] #these are ad-hoc columns to just get the total bill changes by parameter
  outputs_perc_max <- outputs[,c(1:12,44:48,166:170)] 
  outputs_perc_avg <- outputs[,c(1:12,44:48,171:175)] 
  outputs_perc_total <- outputs[,c(1:12,44:48,181:185)] 
  
  outputs_p1<- melt(outputs_totals,measure.vars = c("deep_poverty_total_avg","poverty_total_avg","near_poverty_total_avg","middle_class_total_avg","upper_class_total_avg","total_utility_cost"),
                    variable.name = "income_group",value.name = "annual_bill_change")
  
  # now just look at the deep poverty and upper class groups 
  outputs_p1 <- outputs_p1[which(outputs_p1$income_group%in%c("deep_poverty_total_avg","upper_class_total_avg","total_utility_cost")),]
  
  #outputs_p1_tc <- outputs_p1[which(outputs_p1$income_group%in%c("total_utility_cost")),]
  # rename 
  outputs_p1$build_decision[which(outputs_p1$build_decision=="desal")] <- "High-Capacity"
  outputs_p1$build_decision[which(outputs_p1$build_decision=="grrp-r")] <- "Low-Capacity"
  outputs_p1$build_decision[which(outputs_p1$build_decision=="none")] <- "None"
  
  # just doing long/intense for now 
  #outputs_p1 <-outputs_p1[which(!outputs_p1$drought_characteristic%in%c("long_intense.csv","baseline.csv")),]
  
  #outputs_p1_tc <- outputs_p1[which(outputs_p1$income_group=="total_utility_cost"),]
  #outputs_p1 <- outputs_p1[which(outputs_p1$income_group!="total_utility_cost"),]
  
  # look at how reservoir size changes
  build_order <- c("None","Low-Capacity","High-Capacity")
  mitigate_order <- c("market","baseline")
  
  # figure ouf which of these is the minimum per facet 
  # (1) melt stats together so we can compare across deep poverty, high income, and total cost
  
  outputs_p1_agg <- aggregate(outputs_p1$annual_bill_change,by=list(outputs_p1$income_group,outputs_p1$drought_characteristic),FUN="min")
  #outputs_p1_agg_TC <- aggregate(outputs_p1_tc$annual_bill_change,by=list(outputs_p1_tc$income_group,outputs_p1_tc$drought_characteristic),FUN="min")
  
 # outputs_p1$label <- ""
  #outputs_p1$label[which(outputs_p1$annual_bill_change%in%outputs_p1_agg$x)] <- "X"
  
  #outputs_p1_tc$label <- ""
  #outputs_p1_tc$label[which(outputs_p1_tc$annual_bill_change%in%outputs_p1_agg_TC$x)] <- "X"
  
  # transform so it's droughts in Y, infrastructure options all LI, all HI, all Total Cost in X grouped together 
  library("tidyr")
  
  outputs_p1 <- outputs_p1[,c("drought_characteristic","mitigation_decision","income_group","build_decision","annual_bill_change")]
  outputs_p1_wide <- spread(outputs_p1,drought_characteristic,annual_bill_change)
  #write.csv(outputs_p1,file="~/Documents/__college/reseach_stuff/hr2w_droughts/analysis/sc/paper_one_analyses/outputs_for_utility_vs_ratepayer_calcs.csv")
  # there's an excel pivot table which does the rest of this of the same name in the above folder 
  
  
# NOW THE SAME BUT WITH MAXIMUMS ------------------------
  outputs_p1<- melt(outputs_perc_max,measure.vars = c("deep_poverty_perc_max_avg","poverty_perc_max_avg","near_poverty_perc_max_avg","middle_class_perc_max_avg","upper_class_perc_max_avg","total_utility_cost"),
                    variable.name = "income_group",value.name = "annual_bill_change")
  
  # now just look at the deep poverty and upper class groups 
  outputs_p1 <- outputs_p1[which(outputs_p1$income_group%in%c("deep_poverty_perc_max_avg","upper_class_perc_max_avg","total_utility_cost")),]
  
  #outputs_p1_tc <- outputs_p1[which(outputs_p1$income_group%in%c("total_utility_cost")),]
  # rename 
  outputs_p1$build_decision[which(outputs_p1$build_decision=="desal")] <- "High-Capacity"
  outputs_p1$build_decision[which(outputs_p1$build_decision=="grrp-r")] <- "Low-Capacity"
  outputs_p1$build_decision[which(outputs_p1$build_decision=="none")] <- "None"
  
  # just doing long/intense for now 
  #outputs_p1 <-outputs_p1[which(!outputs_p1$drought_characteristic%in%c("long_intense.csv","baseline.csv")),]
  
  #outputs_p1_tc <- outputs_p1[which(outputs_p1$income_group=="total_utility_cost"),]
  #outputs_p1 <- outputs_p1[which(outputs_p1$income_group!="total_utility_cost"),]
  
  # look at how reservoir size changes
  build_order <- c("None","Low-Capacity","High-Capacity")
  mitigate_order <- c("market","baseline")
  
  # figure ouf which of these is the minimum per facet 
  # (1) melt stats together so we can compare across deep poverty, high income, and total cost
  
  outputs_p1_agg <- aggregate(outputs_p1$annual_bill_change,by=list(outputs_p1$income_group,outputs_p1$drought_characteristic),FUN="min")
  #outputs_p1_agg_TC <- aggregate(outputs_p1_tc$annual_bill_change,by=list(outputs_p1_tc$income_group,outputs_p1_tc$drought_characteristic),FUN="min")
  
  # outputs_p1$label <- ""
  #outputs_p1$label[which(outputs_p1$annual_bill_change%in%outputs_p1_agg$x)] <- "X"
  
  #outputs_p1_tc$label <- ""
  #outputs_p1_tc$label[which(outputs_p1_tc$annual_bill_change%in%outputs_p1_agg_TC$x)] <- "X"
  
  # transform so it's droughts in Y, infrastructure options all LI, all HI, all Total Cost in X grouped together 
  library("tidyr")
  
  outputs_p1 <- outputs_p1[,c("drought_characteristic","mitigation_decision","income_group","build_decision","annual_bill_change")]
  outputs_p1_wide <- spread(outputs_p1,drought_characteristic,annual_bill_change)
  
#  write.csv(outputs_p1,file="~/Documents/__college/reseach_stuff/hr2w_droughts/analysis/sc/paper_one_analyses/outputs_for_utility_vs_ratepayer_calcs_MAX.csv")
  
  
# more calculations but for the purpose of getting section 2 numbers
outputs_scenario <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/plot3/34_outputs.csv")
outputs_scenario$Date <- as.Date(outputs_scenario$Date)

#
params <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/plot3/parameter_list.csv")


# get the params we want
#these_params <- params[which(params$price_elasticity==0.1),] this gets the first sets of numbers

historical_drought_params <- params[which(params$price_elasticity==0.1&params$drought_characteristic=="baseline.csv"&params$build_decision=="none"&params$mitigation_decision=="baseline"),] 
for(x in historical_drought_params$X){
#for(x in these_params$X){
  
  # the params we want arae 1 (baseline, curtail)
  bills <- read.csv(paste0("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/plot3/",x,"_hh_bills.csv"))
  bills$Date <- outputs_scenario$Date
  
  bills$dp <- rowMeans(cbind(bills$X7500, bills$X12500, bills$X17500))
  bills$hi <-
    rowMeans(cbind(bills$X137500, bills$X175000, bills$X250000))
  
  # now get the DP and HI bills for the other three
  bills$dp_demand_only <- rowMeans(cbind(bills$demandonly_7500,bills$demandonly_12500,bills$demandonly_17500))
  bills$hi_demand_only <- rowMeans(cbind(bills$demandonly_137500,bills$demandonly_175000,bills$demandonly_250000))
  # &^^ this one is the 'lower bound' 
  
  bills$dp_rate_only <- rowMeans(cbind(bills$rateonly_7500,bills$rateonly_12500,bills$rateonly_17500))
  bills$hi_rate_only <- rowMeans(cbind(bills$rateonly_137500,bills$rateonly_175000,bills$rateonly_250000))
  
  bills$dp_no_change <-rowMeans(cbind(bills$raw_7500,bills$raw_12500,bills$raw_17500))
  bills$hi_no_change <-rowMeans(cbind(bills$raw_137500,bills$raw_175000,bills$raw_250000))
  
  # subset by the drought dates
  drought_mask <- !bills$dp==bills$dp_no_change
  
  # this will only work if you run it once beforehand to initizliae bills masked
  #bills_masked <- rbind(bills_masked,bills[drought_mask,c(66:74)])
  bills_masked <-bills[drought_mask,c(66:74)]
  
}

# this fixes the garbo above
#bills_masked <-bills_masked[-c(1:12),]
# compute the columns for bills

# now subtract out the no changes
bills_masked$li_reduction <-bills_masked$dp_no_change - bills_masked$dp_demand_only
bills_masked$li_increase <- bills_masked$dp_rate_only - bills_masked$dp_no_change
bills_masked$li_result <- bills_masked$dp-bills_masked$dp_no_change

bills_masked$hi_reduction <- bills_masked$hi_no_change - bills_masked$hi_demand_only
bills_masked$hi_increase <- bills_masked$hi_rate_only - bills_masked$hi_no_change
bills_masked$hi_result <- bills_masked$hi-bills_masked$hi_no_change

# get my col averages 
bills_masked <- bills_masked[]
colMeans(bills_masked[,c(10:15)])
summary(bills_masked[,c(10:15)]) # this is for the numbers in the first results section
