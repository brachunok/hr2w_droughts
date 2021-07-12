#analyze_outputs
library(plyr)
load("~/Documents/__college/reseach_stuff/hr2w_droughts/analysis/sc/processed_bill_data.Rdata")
head(processed)

# figure out our 'baseline' values for each, and what row that will be
# take only the unique parameters (not DVs) and then find all the rows
# matching those parameters. 

# get rid of anyhing without 30 yr pbp and 3% interest
processed <- processed[which(processed$pay_back_period==30&processed$discount_rate==3),]
unique_params <- unique(processed[,c(2:8)])

incomes <- c(7500,12500,17500,22500,27500,32500,37500,42500,47500,55000,67500,87500,112500,137500,175000,250000)
deep_poverty <- paste0("D",incomes[1:3]) # 18000
poverty <- paste0("D",incomes[4:6])
near_poverty <- paste0("D",incomes[7:9])
middle_class <- paste0("D",incomes[10:13])
upper_class <-  paste0("D",incomes[14:16])

outputs <- processed
outputs[,paste0("D",incomes)] <- NA

outputs$deep_poverty_avg <- NA
outputs$poverty_avg <- NA
outputs$near_poverty_avg <- NA
outputs$middle_class_avg <- NA
outputs$upper_class_avg <- NA

# get some column indices beforhand
before_cols <- which(grepl("B",colnames(outputs)))
after_cols <- which(grepl("A",colnames(outputs)))
difference_cols <- which(grepl("D",colnames(outputs)))

for(i in 1:nrow(unique_params)){
  
  this_param <- unique_params[i,]
  this_matching <- match_df(x=processed, y=this_param,on=colnames(processed)[c(2:8)])
  
  # get baseline row
  baseline_row <- which(this_matching$mitigation_decision=="baseline")
  baseline_row_in_outputs <- which(processed$X%in%this_matching$X[baseline_row])
  
  other_rows <-   which(this_matching$mitigation_decision!="baseline")
  other_rows_in_outputs <- which(processed$X%in%this_matching$X[other_rows])
  
  #(1) compute difference between before and after for 'baseline' and write that to baseline
  baseline_difference <- this_matching[baseline_row,after_cols]- this_matching[baseline_row,before_cols]
  
  outputs[baseline_row_in_outputs,difference_cols] <- baseline_difference
  
  #(2) compute before/after difference between all other rows 'afters' and baseline 'before'
  for (r in 1:length(other_rows)){
    this_row <- other_rows[r]
    outputs[other_rows_in_outputs[r],difference_cols] <- this_matching[this_row,after_cols]-this_matching[baseline_row,before_cols]
    
  }
  if (floor(i/10)==i/10){
    print(i/nrow(unique_params)*100)
  }
  
}

# now that we've computed all the differences, do the aggregation by bin

outputs$deep_poverty_avg <- rowMeans(outputs[,colnames(outputs)%in%deep_poverty])
outputs$poverty_avg<- rowMeans(outputs[,colnames(outputs)%in%poverty])
outputs$near_poverty_avg<- rowMeans(outputs[,colnames(outputs)%in%near_poverty])
outputs$middle_class_avg <- rowMeans(outputs[,colnames(outputs)%in%middle_class])
outputs$upper_class_avg <- rowMeans(outputs[,colnames(outputs)%in%upper_class])

#save(outputs,file = "~/Documents/__college/reseach_stuff/hr2w_droughts/analysis/processed_and_binned_bill_data.Rdata")

# Now make some figures
library(reshape2)
library(ggplot2)

# going to melt the responses 
outputs_melt <- melt(outputs,measure.vars = c("deep_poverty_avg","poverty_avg","near_poverty_avg","middle_class_avg","upper_class_avg"),
                     variable.name = "income_group",value.name = "annual_bill_change")

# plot of passthrough type and drought duration
# hold the rest constant

outputs_p1 <- outputs_melt
outputs_p1 <- outputs_p1[which(outputs_p1$income_distribution=="state"& outputs_p1$fee_passthrough=="zero_threshold" & outputs_p1$reservoir_capacity==2800),]

p1 <- ggplot(outputs_p1,aes(x=income_group,y=annual_bill_change,fill=mitigation_decision))+
  geom_bar(stat='identity',position='dodge')+ 
  facet_grid(cols=vars(drought_characteristic),rows=vars(income_elasticity))

p1
