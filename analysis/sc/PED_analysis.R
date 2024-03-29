# behavior change 

options(scipen = 999)
library(reshape2)
library(RColorBrewer)
library(data.table)
library(corrplot)
library(ggplot2)
load("../processed_and_binned_bill_data.Rdata")

res_size = 2800
income_elasticity = 0.1
water_cost <- 22326.39
#price_elasticity = 0.41
outputs <- outputs[which(outputs$income_elasticity==income_elasticity&outputs$reservoir_capacity==res_size&outputs$water_cost==water_cost),]
outputs$combined_decision <- paste0(outputs$build_decision,"_",outputs$mitigation_decision)
tc_PED_plot <- ggplot()+ geom_line(data=outputs,aes(x=price_elasticity,y=total_utility_cost,color=combined_decision))+theme_bw()+
  facet_grid(rows=vars(drought_characteristic))
tc_PED_plot

# 
tc_PED_plot <- ggplot()+ geom_line(data=outputs[which(outputs$total_utility_cost<=50000000),],aes(x=price_elasticity,y=total_utility_cost,color=combined_decision))+theme_bw()+
  facet_grid(rows=vars(drought_characteristic),scale="free")
tc_PED_plot

# same thing for other performance metrics 

# get the ending label point: e.g. whatever the performance metric is when PED = 1 
end_points <- outputs[which(outputs$price_elasticity==1),c("combined_decision","poverty_total_avg")]

li_total_PED_plot <- ggplot()+ geom_line(data=outputs,aes(x=price_elasticity,y=poverty_perc_total,color=combined_decision))+theme_bw()+
  facet_grid(rows=vars(drought_characteristic)) + geom_vline(xintercept=0.41) +xlim(-1,1)
li_total_PED_plot  

hi_total_PED_plot <- ggplot()+ geom_line(data=outputs,aes(x=price_elasticity,y=upper_class_perc_total,color=combined_decision))+theme_bw()+
  facet_grid(rows=vars(drought_characteristic)) + geom_vline(xintercept=0.41) +xlim(-1,1)
hi_total_PED_plot  


#FIGURE OUT WHY THE NPR BASELINE doesnt change in the baseline/intense cases. You've run the balance plot runner
#on the cluster, you just need to bring th plots back'
