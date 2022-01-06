# paper one figure 5: parallel axis plot 

# notes from RDM analysisi

options(scipen = 999)
library(reshape2)
library(RColorBrewer)
library(data.table)
library(corrplot)
load("../processed_and_binned_bill_data_plot5.Rdata")


# this is the name of the col(s) we want to make a performance measure of 
performance_measures <- c("total_utility_cost","poverty_perc_total","upper_class_perc_total","poverty_perc_max_avg","upper_class_perc_max_avg")
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
results$tc_range <- NA
results$tc_reg <- NA

results$pov_total_exp <- rowMeans(outputs_wide[,pov_total_cols])
results$pov_total_range <- NA
results$pov_total_reg <- NA

results$uc_total_exp <- rowMeans(outputs_wide[,uc_total_cols])
results$uc_total_range <- NA
results$uc_total_reg <- NA

results$pov_perc_exp <- rowMeans(outputs_wide[,pov_perc_cols])
results$pov_perc_range <- NA
results$pov_perc_reg <- NA

results$uc_perc_exp <- rowMeans(outputs_wide[,uc_perc_cols])
results$uc_perc_range <- NA
results$uc_perc_reg <- NA

# loop through rows and calculate the variance and regret 
for( i in 1:nrow(outputs_wide)){
  
  results$tc_range[i] <- max(unlist(outputs_wide[i,tc_cols]))-min(unlist(outputs_wide[i,tc_cols]))
  results$pov_total_range[i] <- max(unlist(outputs_wide[i,pov_total_cols]))-min(unlist(outputs_wide[i,pov_total_cols]))
  results$uc_total_range[i] <- max(unlist(outputs_wide[i,uc_total_cols]))-min(unlist(outputs_wide[i,uc_total_cols]))
  results$pov_perc_range[i] <- max(unlist(outputs_wide[i,pov_perc_cols]))-min(unlist(outputs_wide[i,pov_perc_cols]))
  results$uc_perc_range[i] <- max(unlist(outputs_wide[i,uc_perc_cols])) - min(unlist(outputs_wide[i,uc_perc_cols]))
  
  
  results$tc_reg[i] <- min(outputs_wide[i,tc_cols])-max(outputs_wide[i,tc_cols])
  results$pov_total_reg[i] <- min(outputs_wide[i,pov_total_cols])-max(outputs_wide[i,pov_total_cols])
  results$uc_total_reg[i] <- min(outputs_wide[i,uc_total_cols])-max(outputs_wide[i,uc_total_cols])
  results$pov_perc_reg[i] <- min(outputs_wide[i,pov_perc_cols])-max(outputs_wide[i,pov_perc_cols])
  results$uc_perc_reg[i] <- min(outputs_wide[i,uc_perc_cols])-max(outputs_wide[i,uc_perc_cols])
  
}

# now that we have results, let's fire up some figures
# first let's try out a couple of correlation plots 

M_ev <- cor(results[,c(1,4,7,10,13)])
corrplot(M_ev,method="circle",col=brewer.pal(n=6, name="PuOr"))

M_var <- cor(results[,c(2,5,8,11,14)])
corrplot(M_var,method="circle",col=brewer.pal(n=6, name="PuOr"))

M_reg <- cor(results[,c(3,6,9,12,15)])
corrplot(M_reg,method="circle",col=brewer.pal(n=6, name="PuOr"))
# make one 


# make a parallel axis plot 
library(GGally)
library(dplyr)
library(ggrepel)
library(viridis)
library(hrbrthemes)
library(reshape2)
library("scales")
library("ggsci")
library(cowplot)
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
range_cols <- which(grepl("range",colnames(results)))

# make a parallel axis plot on my own
#(1) scale each column so that minimum is 0 and max is 1
results_pap_scaled <- results_pap

for(j in 1:16){
  this_col <- results_pap[,j]
  range <- max(this_col)-min(this_col)

  new_this_col <- (this_col-min(this_col))/range  
  results_pap_scaled[,j] <- new_this_col
}

# now make my own parallel axis plot 
results_pap_scaled <- results_pap_scaled[,c(exp_cols,which(reg_cols))]
results_pap <- results_pap[,c(exp_cols,which(reg_cols))]

results_pap_scaled$name <- rownames(results_pap_scaled)
results_pap_scaled$mitigation <-c("conserve","market")
results_pap_scaled$build <- c("high","high","low","low","none","none")
results_pap$name <- rownames(results_pap_scaled)
results_pap$mitigation <-c("conserve","market")
results_pap$build <- c("high","high","low","low","none","none")

results_pap_scaled_melt <- melt(results_pap_scaled,id.vars = c("name","mitigation","build"))
results_pap_melt  <- melt(results_pap,id.vars = c("name","mitigation","build")) 

results_pap_scaled_melt$label <- results_pap_melt$value*-1
results_pap_scaled_melt$label[which(!results_pap_scaled_melt$value%in%c(0,1))] <- NA


pap_market_exp <- ggplot()+geom_line(data=results_pap_scaled_melt,aes(x=variable,y=value,group=name,color=mitigation))+
  geom_point(data=results_pap_scaled_melt,aes(x=variable,y=value,color=mitigation,shape=build))+theme_bw()+scale_color_npg()+
  labs(y="Scenario Performance \n (Higher is Better)",x="")+geom_text_repel(data=results_pap_scaled_melt,aes(x=variable,y=value,label=round(label,digits=3)))
pap_market_exp


# want to re-make a figure which has expected total cost and regret costs side-by-side



ggsave(pap_market_exp,filename="~/Documents/__college/reseach_stuff/hr2w_droughts/analysis/sc/draft_figure_5.pdf",width=6,height=4,units="in",scale=1.3)
