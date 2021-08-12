# Calculate RDM formulations 
# aim is this:
#          | npr   | conserve | market |     ...
#      s1     P1        
#      s2     P2
#      s3
#      s4
# 
#           expectation of columns
#           regret of columns 
#           variacne of columns 
#
# do one of these for p = utilty cost, low income bill, high income bill ...
# 
# 
options(scipen = 999)
library(reshape2)
library(RColorBrewer)
library(data.table)
library(corrplot)
load("../processed_and_binned_bill_data.Rdata")

res_size = 2800
income_elasticity = 0.1
water_cost <- 22326.39
outputs <- outputs[which(outputs$income_elasticity==income_elasticity&outputs$reservoir_capacity==res_size&outputs$water_cost==water_cost),]

# this is the name of the col(s) we want to make a performance measure of 
performance_measures <- c("total_utility_cost","poverty_total_avg","upper_class_total_avg","poverty_perc_max_avg","upper_class_perc_max_avg")
performance_measures_var_names <- c("tc","pov_total","uc_total","pov_perc","uc_perc")
p_m_cols <- which(colnames(outputs)%in%performance_measures)

# now shrink it to teh scenario descriptors and p_m_cols. define the columns for scenario descriptors manually
outputs <- outputs[,c(1:11,p_m_cols)]

# do it once to initialize everything
this_outputs_wide <- t(dcast(setDT(outputs),drought_characteristic~build_decision+mitigation_decision,value.var=performance_measures[1]))
this_outputs_wide <- data.frame(this_outputs_wide)
colnames(this_outputs_wide) <- paste0(this_outputs_wide[1,],"_",performance_measures_var_names[1])
outputs_wide <-this_outputs_wide

for (i in 2:length(performance_measures)){
  this_outputs_wide <- t(dcast(setDT(outputs),drought_characteristic~build_decision+mitigation_decision,value.var=performance_measures[i]))
  this_outputs_wide <- data.frame(this_outputs_wide)
  colnames(this_outputs_wide) <- paste0(this_outputs_wide[1,],"_",performance_measures_var_names[i])
  outputs_wide <- cbind(outputs_wide,this_outputs_wide)
}

outputs_wide <- outputs_wide[-1,]
# get rid of the improved 
outputs_wide <- outputs_wide[!grepl("improved",rownames(outputs_wide)),]
outputs_wide <- as.data.frame(outputs_wide)

for( j in 1:ncol(outputs_wide)){
  outputs_wide[,j] <- as.numeric(outputs_wide[,j])
}

# now calculate some things from outputw_wide and put them into 'results'
results <- data.frame(row.names = rownames(outputs_wide))

tc_cols <- which(grepl("_tc",colnames(outputs_wide)))
pov_total_cols <- which(grepl("pov_total",colnames(outputs_wide)))
uc_total_cols <-  which(grepl("uc_total",colnames(outputs_wide)))
pov_perc_cols <-  which(grepl("pov_perc",colnames(outputs_wide)))
uc_perc_cols <-   which(grepl("uc_perc",colnames(outputs_wide)))

results$tc_exp <- rowMeans(outputs_wide[,tc_cols])
results$tc_var <- NA
results$tc_reg <- NA

results$pov_total_exp <- rowMeans(outputs_wide[,pov_total_cols])
results$pov_total_var <- NA
results$pov_total_reg <- NA

results$uc_total_exp <- rowMeans(outputs_wide[,uc_total_cols])
results$uc_total_var <- NA
results$uc_total_reg <- NA

results$pov_perc_exp <- rowMeans(outputs_wide[,pov_perc_cols])
results$pov_perc_var <- NA
results$pov_perc_reg <- NA

results$uc_perc_exp <- rowMeans(outputs_wide[,uc_perc_cols])
results$uc_perc_var <- NA
results$uc_perc_reg <- NA

# loop through rows and calculate the variance and regret 
for( i in 1:nrow(outputs_wide)){
  
  results$tc_var[i] <- var(unlist(outputs_wide[i,tc_cols]))
  results$pov_total_var[i] <- var(unlist(outputs_wide[i,pov_total_cols]))
  results$uc_total_var[i] <- var(unlist(outputs_wide[i,uc_total_cols]))
  results$pov_perc_var[i] <- var(unlist(outputs_wide[i,pov_perc_cols]))
  results$uc_perc_var[i] <- var(unlist(outputs_wide[i,uc_perc_cols]))
  
  
  results$tc_reg[i] <- min(outputs_wide[i,tc_cols])-max(outputs_wide[i,tc_cols])
  results$pov_total_reg[i] <- min(outputs_wide[i,pov_total_cols])-max(outputs_wide[i,pov_total_cols])
  results$uc_total_reg[i] <- min(outputs_wide[i,uc_total_cols])-max(outputs_wide[i,uc_total_cols])
  results$pov_perc_reg[i] <- min(outputs_wide[i,pov_perc_cols])-max(outputs_wide[i,pov_perc_cols])
  results$uc_perc_reg[i] <- min(outputs_wide[i,uc_perc_cols])-max(outputs_wide[i,uc_perc_cols])
  
}

# now that we have results, let's fire up some figures
# first let's try out a couple of correlation plots 

M <- cor(results)
corrplot(M,method="circle",col=brewer.pal(n=6, name="PuOr"))

# make a parallel axis plot 
library(GGally)
library(dplyr)
library(viridis)
library(hrbrthemes)

# make a results dataset for which everything is "close to 0 is bad, high positive number is good" 
results_pap <- results

# almost everything we want to multiply by -1 except for regret which is already 'psotive good' 
reg_cols <- grepl("reg",colnames(results))
results_pap[,!reg_cols] <- -1*results[,!reg_cols]

pap <- ggparcoord(results_pap,scale="uniminmax",alphaLines = 0.3,columns=1:15) + theme_ipsum();pap

results_pap$market <- NA
results_pap$market <- grepl("market",rownames(results))
results_pap$name <- rownames(results_pap)
pap_market <- ggparcoord(results_pap,scale="uniminmax",columns=1:15,groupColumn = 16)+theme_ipsum()+ scale_color_viridis(discrete=TRUE);pap_market

# now make the same thing but for just exp
exp_cols <- which(grepl("exp",colnames(results)))
pap_market_exp <- ggparcoord(results_pap,scale="uniminmax",columns=exp_cols,groupColumn = 17)+theme_ipsum()+ scale_color_viridis(discrete=TRUE);pap_market_exp
pap_market_exp_reordered <- ggparcoord(results_pap,scale="uniminmax",columns=c(1,10,13,4,7),groupColumn = 17)+theme_ipsum()+ scale_color_viridis(discrete=TRUE);pap_market_exp_reordered


pap_market_reg <- ggparcoord(results_pap,scale="uniminmax",columns=c(1,which(reg_cols)),groupColumn = 16)+theme_ipsum()+ scale_color_viridis(discrete=TRUE);pap_market_reg
pap_reg <- ggparcoord(results_pap,scale="uniminmax",columns=c(1,which(reg_cols)),groupColumn = 17)+theme_ipsum()+ scale_color_viridis(discrete=TRUE);pap_reg

# leaderboard for each,--> being cheap and doing this in excel
write.csv("~/Downloads/results.csv",x = results)

# color leaderboard by market/baseline

# 








