#analyze_outputs
library(plyr)
load("~/Documents/__college/reseach_stuff/hr2w_droughts/analysis/sc/processed_bill_data_plot8.Rdata")

# figure out our 'baseline' values for each, and what row that will be
# take only the unique parameters (not DVs) and then find all the rows
# matching those parameters. 

# get rid of anyhing without 30 yr pbp and 3% interest
processed <- processed[which(processed$pay_back_period==30&processed$discount_rate==3),]
unique_params <- unique(processed[,c(2:8,11,12)])

incomes <- c(7500,12500,17500,22500,27500,32500,37500,42500,47500,55000,67500,87500,112500,137500,175000,250000)
deep_poverty <- paste0(incomes[1:3]) # 18000
poverty <- paste0(incomes[4:6])
near_poverty <- paste0(incomes[7:9])
middle_class <- paste0(incomes[10:13])
upper_class <-  paste0(incomes[14:16])

outputs <- processed
outputs[,paste0("total_difference",incomes)] <- NA
outputs[,paste0("D_max_perc",incomes)] <- NA
outputs[,paste0("D_avg_perc",incomes)] <- NA

outputs$deep_poverty_total_avg <- NA
outputs$poverty_total_avg <- NA
outputs$near_poverty_total_avg <- NA
outputs$middle_class_total_avg <- NA
outputs$upper_class_total_avg <- NA

outputs$deep_poverty_perc_max_avg <- NA
outputs$poverty_perc_max_avg <- NA
outputs$near_poverty_perc_max_avg <- NA
outputs$middle_class_perc_max_avg <- NA
outputs$upper_class_perc_max_avg <- NA

outputs$deep_poverty_perc_avg_avg <- NA
outputs$poverty_perc_avg_avg <- NA
outputs$near_poverty_perc_avg_avg <- NA
outputs$middle_class_perc_avg_avg <- NA
outputs$upper_class_perc_avg_avg <- NA

outputs$deep_poverty_perc_total <- NA
outputs$poverty_perc_total <- NA
outputs$near_poverty_perc_total <- NA
outputs$middle_class_perc_total<- NA
outputs$upper_class_perc_total <- NA

outputs$deep_poverty_max_avg <-NA
outputs$poverty_max_avg <- NA
outputs$near_poverty_max_avg <- NA
outputs$middle_class_max_avg <- NA
outputs$upper_class_max_avg <- NA

for(i in 1:nrow(unique_params)){
  
  this_param <- unique_params[i,]
  this_matching <- match_df(x=processed, y=this_param,on=colnames(processed)[c(2:8,11,12)])
  
  # get baseline row
  baseline_row <- which(this_matching$mitigation_decision=="baseline"&this_matching$build_decision=="none")
  baseline_row_in_outputs <- which(processed$X%in%this_matching$X[baseline_row])
  
  other_rows <-   which(this_matching$mitigation_decision!="baseline"|this_matching$build_decision!="none")
  other_rows_in_outputs <- which(processed$X%in%this_matching$X[other_rows])
  
  #(1) compute absolute bill differences. now called total_difference12000
  total_difference_after_cols <- grepl("A_total",colnames(this_matching))
  total_difference_before_cols <- grepl("B_total",colnames(this_matching))
  total_difference_write_cols <- grepl("total_difference",colnames(outputs))
  
  perc_max_after_cols <- grepl("A_max_perc",colnames(this_matching))
  perc_max_before_cols <- grepl("B_max_perc",colnames(this_matching))
  perc_max_write_cols <- grepl("D_max_perc",colnames(outputs))
  
  perc_avg_after_cols <- grepl("A_avg_perc",colnames(this_matching))
  perc_avg_before_cols <- grepl("B_avg_perc",colnames(this_matching))
  perc_avg_write_cols <- grepl("D_avg_perc",colnames(outputs))
  
  
  # compute the difference between the after and before columns in matching and 
  # write to the write cols of outputs
  baseline_total_difference <- this_matching[baseline_row,total_difference_after_cols]- this_matching[baseline_row,total_difference_before_cols]
  
  outputs[baseline_row_in_outputs,total_difference_write_cols] <- baseline_total_difference
  
  # now do it for the max percentage
  max_perc_baseline_difference <- this_matching[baseline_row,perc_max_after_cols]-this_matching[baseline_row,perc_max_before_cols]
  outputs[baseline_row_in_outputs,perc_max_write_cols] <-max_perc_baseline_difference 
  
  # and for the avg percentages
  avg_perc_baseline_difference <- this_matching[baseline_row,perc_avg_after_cols]-this_matching[baseline_row,perc_avg_before_cols]
  outputs[baseline_row_in_outputs,perc_avg_write_cols] <- avg_perc_baseline_difference 
  
  
  #(2) compute before/after difference between all other rows 'afters' and baseline 'before'
  for (r in 1:length(other_rows)){
    this_row <- other_rows[r]
    outputs[other_rows_in_outputs[r],total_difference_write_cols] <- this_matching[this_row,total_difference_after_cols]-this_matching[baseline_row,total_difference_before_cols]
    outputs[other_rows_in_outputs[r],perc_max_write_cols] <- this_matching[this_row,perc_max_after_cols]-this_matching[baseline_row,perc_max_before_cols]
    outputs[other_rows_in_outputs[r],perc_avg_write_cols] <- this_matching[this_row,perc_avg_after_cols]-this_matching[baseline_row,perc_avg_before_cols]
    
  }
  
  
  if (floor(i/10)==i/10){
    print(i/nrow(unique_params)*100)
  }
  
}


#make the 'max dollar value increase' columns. Loop through all the 

max_dp <- outputs[,colnames(outputs)%in%paste0("D_max_perc",deep_poverty)]
for(j in 1:ncol(max_dp)){
max_dp[,j] <-max_dp[,j]*as.numeric(deep_poverty[j])/12
  
}
outputs$deep_poverty_max_avg <- rowMeans(max_dp)

max_p <- outputs[,colnames(outputs)%in%paste0("D_max_perc",poverty)]
for(j in 1:ncol(max_p)){
  max_p[,j] <-max_p[,j]*as.numeric(poverty[j])/12
  
}
outputs$poverty_max_avg <- rowMeans(max_p)

max_np <- outputs[,colnames(outputs)%in%paste0("D_max_perc",near_poverty)]
for(j in 1:ncol(max_np)){
  max_np[,j] <-max_np[,j]*as.numeric(near_poverty[j])/12
  
}

outputs$near_poverty_max_avg <- rowMeans(max_np)

max_mc <- outputs[,colnames(outputs)%in%paste0("D_max_perc",middle_class)]
for(j in 1:ncol(max_mc)){
  max_mc[,j] <-max_mc[,j]*as.numeric(middle_class[j])/12
  
}
outputs$middle_class_max_avg <- rowMeans(max_mc)

max_uc <- outputs[,colnames(outputs)%in%paste0("D_max_perc",upper_class)]
for(j in 1:ncol(max_uc)){
  max_uc[,j] <-max_uc[,j]*as.numeric(upper_class[j])/12
  
}
outputs$upper_class_max_avg<- rowMeans(max_uc)


# now that we've computed all the differences, do the aggregation by bin


outputs$deep_poverty_total_avg <- rowMeans(outputs[,colnames(outputs)%in%paste0("total_difference",deep_poverty)])
outputs$poverty_total_avg<- rowMeans(outputs[,colnames(outputs)%in%paste0("total_difference",poverty)])
outputs$near_poverty_total_avg <- rowMeans(outputs[,colnames(outputs)%in%paste0("total_difference",near_poverty)])
outputs$middle_class_total_avg<- rowMeans(outputs[,colnames(outputs)%in%paste0("total_difference",middle_class)])
outputs$upper_class_total_avg<- rowMeans(outputs[,colnames(outputs)%in%paste0("total_difference",upper_class)])

# now for max percentage
outputs$deep_poverty_perc_max_avg <- rowMeans(outputs[,colnames(outputs)%in%paste0("D_max_perc",deep_poverty)])
outputs$poverty_perc_max_avg<- rowMeans(outputs[,colnames(outputs)%in%paste0("D_max_perc",poverty)])
outputs$near_poverty_perc_max_avg <- rowMeans(outputs[,colnames(outputs)%in%paste0("D_max_perc",near_poverty)])
outputs$middle_class_perc_max_avg<- rowMeans(outputs[,colnames(outputs)%in%paste0("D_max_perc",middle_class)])
outputs$upper_class_perc_max_avg<- rowMeans(outputs[,colnames(outputs)%in%paste0("D_max_perc",upper_class)])

# now for avg percentage
outputs$deep_poverty_perc_avg_avg <- rowMeans(outputs[,colnames(outputs)%in%paste0("D_avg_perc",deep_poverty)])
outputs$poverty_perc_avg_avg<- rowMeans(outputs[,colnames(outputs)%in%paste0("D_avg_perc",poverty)])
outputs$near_poverty_perc_avg_avg <- rowMeans(outputs[,colnames(outputs)%in%paste0("D_avg_perc",near_poverty)])
outputs$middle_class_perc_avg_avg<- rowMeans(outputs[,colnames(outputs)%in%paste0("D_avg_perc",middle_class)])
outputs$upper_class_perc_avg_avg<- rowMeans(outputs[,colnames(outputs)%in%paste0("D_avg_perc",upper_class)])

# compute avg %ages of income
outputs$deep_poverty_perc_total <- rowMeans(outputs[,colnames(outputs)%in%paste0("A_max_perc_",deep_poverty)])
outputs$poverty_perc_total <- rowMeans(outputs[,colnames(outputs)%in%paste0("A_max_perc_",poverty)])
outputs$near_poverty_perc_total <- rowMeans(outputs[,colnames(outputs)%in%paste0("A_max_perc_",near_poverty)])
outputs$middle_class_perc_total <- rowMeans(outputs[,colnames(outputs)%in%paste0("A_max_perc_",middle_class)])
outputs$upper_class_perc_total <- rowMeans(outputs[,colnames(outputs)%in%paste0("A_max_perc_",upper_class)])

save(outputs,file = "~/Documents/__college/reseach_stuff/hr2w_droughts/analysis/processed_and_binned_bill_data_plot8.Rdata")

# # Now make some figures
# library(reshape2)
# library(ggplot2)
# 
# # going to melt the responses 
# outputs_totals <- outputs[,c(1:12,44:47,161:165)] #these are ad-hoc columns to just get the total bill changes by parameter
# outputs_perc_max <- outputs[,c(1:12,44:47,166:170)] 
# outputs_perc_avg <- outputs[,c(1:12,44:47,171:175)] 
# 
# outputs_p1<- melt(outputs_totals,measure.vars = c("deep_poverty_total_avg","poverty_total_avg","near_poverty_total_avg","middle_class_total_avg","upper_class_total_avg"),
#                      variable.name = "income_group",value.name = "annual_bill_change")
# 
# # change reservoir size
# outputs_p1 <- outputs_p1[which(outputs_p1$income_distribution=="state"& outputs_p1$fee_passthrough=="zero_threshold" & outputs_p1$income_elasticity==0.1&
#                                  outputs_p1$pay_back_period==30&outputs_p1$reservoir_capacity==2800&outputs_p1$water_cost==22326.39 ),]
# 
# # for now just baseline conservation
# #outputs_p1 <-outputs_p1[which(outputs_p1$mitigation_decision=="baseline"),]
# 
# # look at how reservoir size changes
# p1 <- ggplot(outputs_p1,aes(x=income_group,y=annual_bill_change,fill=build_decision))+
#   geom_bar(stat='identity',position='dodge')+ 
#   facet_grid(cols=vars(price_elasticity),rows=vars(mitigation_decision)) + theme_bw() +
#   theme(axis.text.x = element_text(angle = 90)) + ggtitle("Annual Bill Change by  income group, mitigation decision, and price elasticity")
# 
# p1
# 
# p1_utility_cost <- ggplot(outputs_p1,aes(x=build_decision,y=total_utility_cost,fill=build_decision))+
#   geom_bar(stat='identity',position='dodge')+ 
#   facet_grid(cols=vars(drought_characteristic),rows=vars(mitigation_decision)) + theme_bw() +
#   theme(axis.text.x = element_text(angle = 90)) + ggtitle("Total Utility Cost by mitigation decision, build options, and drought characteristics")
# p1_utility_cost
# 
# # utility cost and cost per income group
# p1_utility_individual_cost <- ggplot(outputs_p1,aes(x=total_utility_cost,y=annual_bill_change,color=income_group))+geom_jitter()+
#   scale_color_brewer() + facet_grid(rows=vars(mitigation_decision),cols=vars(drought_characteristic)) +theme_bw()
# p1_utility_individual_cost
# 
# # now make the same thing but do it for the income-fractional data
# 
# outputs_p1_perc_max <- melt(outputs_perc_max,measure.vars = c("deep_poverty_perc_max_avg","poverty_perc_max_avg","near_poverty_perc_max_avg","middle_class_perc_max_avg","upper_class_perc_max_avg"),
#                             variable.name = "income_group",value.name = "max_percentage_bill_change")
# 
# outputs_p1_perc_max <- outputs_p1_perc_max[which(outputs_p1_perc_max$reservoir_capacity==2800&outputs_p1_perc_max$income_elasticity==0.1),]
# 
# outputs_p1_perc_avg <- melt(outputs_perc_max,measure.vars = c("deep_poverty_perc_max_avg","poverty_perc_max_avg","near_poverty_perc_max_avg","middle_class_perc_max_avg","upper_class_perc_max_avg"),
#                                variable.name = "income_group",value.name = "avg_percentage_bill_change")
# 
# outputs_p1_perc_avg  <-outputs_p1_perc_avg[which(outputs_p1_perc_avg$reservoir_capacity==2800&outputs_p1_perc_avg$income_elasticity==0.1),]
# 
# p1_another_way <- ggplot(outputs_p1_perc_max,aes(x=log1p(total_utility_cost),y=max_percentage_bill_change,color=income_group,group=income_group))+geom_line()+
#   facet_grid(rows=vars(mitigation_decision),cols=vars(drought_characteristic)) + theme_bw()
# 
# p1_another_way
# 
# p1_another_way_market_buy <- ggplot(outputs_p1_perc_max,aes(x=log1p(total_utility_cost),y=market_buy,color=income_group,group=income_group))+geom_line()+
#   facet_grid(rows=vars(mitigation_decision),cols=vars(drought_characteristic)) + theme_bw()
# 
# # how does market water change things?
# outputs_perc_avg$equity_diff <- abs(outputs_perc_avg$upper_class_perc_avg_avg - outputs_perc_avg$deep_poverty_perc_avg_avg)
# plot(equity_diff~water_cost,data=outputs_perc_avg)                                      
# 
# outputs_perc_avg_melted <- melt(outputs_perc_avg,measure.vars = c("deep_poverty_perc_avg_avg","poverty_perc_avg_avg","near_poverty_perc_avg_avg","middle_class_perc_avg_avg","upper_class_perc_avg_avg"),
#                                 variable.name = "income_group",value.name = "avg_percentage_bill_change")
# 
# # now plot the perc changes against the cost of water                                      
# 
# outputs_perc_avg_melted <- outputs_perc_avg_melted[which(outputs_perc_avg_melted$mitigation_decision=="market"),]
# p1_water_cost <- ggplot(outputs_perc_avg_melted,aes(x=water_cost,y=avg_percentage_bill_change,color=income_group,group=income_group))+geom_line()+
#   facet_grid(rows=vars(build_decision),cols=vars(drought_characteristic))+ theme_bw()
# p1_water_cost
