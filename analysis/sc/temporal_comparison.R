# comparing different performance metrics


# baseline 'do-nothing'
load("../processed_and_binned_bill_data.Rdata")

# what do these mean:
#poverty_perc_total: maximum %age of income spent on water after droughts for poor populations
# poverty_total_avg: total amount the bill went up for the last year
# poverty_perc_max_avg:  maximum bill increase as a %age of income
# poverty_perc_avg_avg: average bill increase over a year as a %age of income
deep_poverty_metrics  <- c("deep_poverty_perc_total","deep_poverty_total_avg","deep_poverty_perc_max_avg","deep_poverty_perc_avg_avg")
poverty_metrics <- c("poverty_perc_total","poverty_total_avg","poverty_perc_max_avg","poverty_perc_avg_avg")
high_income_metrics <- c("upper_class_perc_total","upper_class_total_avg","upper_class_perc_max_avg","upper_class_perc_avg_avg")

# make the original plot of bill increase by population group for each drought and option
# this is to see why the poverty and deep poverty figures are so different

outputs_deep_poverty <- outputs[,c(2:12,which(colnames(outputs)%in%deep_poverty_metrics)),]
outputs_poverty <- outputs[,c(2:12,which(colnames(outputs)%in%poverty_metrics)),]
outputs_hi <- outputs[,c(2:12,which(colnames(outputs)%in%high_income_metrics)),]

# now trim down the dataframe to only include the normal PED values
outputs_deep_poverty <- outputs_deep_poverty[which(outputs_deep_poverty$price_elasticity==0.5),]
outputs_poverty <- outputs_poverty[which(outputs_poverty$price_elasticity==0.5),]
outputs_hi <- outputs_hi[which(outputs_hi$price_elasticity==0.5),]

# TODO: make sure we run this with an actual value of 0.41


pairs(outputs_poverty[,c(12:15)])


library('ggplot2')
library('ggrepel')
library('gghighlight')

# now only do it for the ones we see differences in
# this means: poverty_total_avg vs poverty_perc_max_avg
outputs_deep_poverty$label <- paste0(outputs_deep_poverty$mitigation_decision,"_",outputs_deep_poverty$build_decision)
outputs_poverty$label <- paste0(outputs_poverty$mitigation_decision,"_",outputs_poverty$build_decision)
outputs_hi$label <- paste0(outputs_hi$mitigation_decision,"_",outputs_hi$build_decision)

p0 <- ggplot(data=outputs_deep_poverty,aes(x=deep_poverty_total_avg,y=deep_poverty_perc_max_avg,label=label,fill=drought_characteristic))+geom_jitter()+
  geom_label_repel(max.overlaps = 20)+geom_hline(yintercept=0)+geom_vline(xintercept = 0) +theme_bw()+gghighlight(mitigation_decision=="baseline");p0 

p0.1 <- ggplot(data=outputs_deep_poverty,aes(x=deep_poverty_total_avg,y=deep_poverty_perc_max_avg,fill=drought_characteristic))+geom_jitter()+geom_hline(yintercept=0)+
  geom_vline(xintercept = 0) +theme_bw()+gghighlight(mitigation_decision=="baseline")+geom_label_repel(aes(label=label),max.overlaps = 20) ;p0.1

# same thing but highlight and facet based on drought 

outputs_deep_poverty$drought_characteristic <- factor(outputs_deep_poverty$drought_characteristic,levels=c("baseline.csv","intense.csv","long.csv","long_intense.csv",""))
p0.2 <- ggplot(data=outputs_deep_poverty,aes(x=deep_poverty_total_avg,y=deep_poverty_perc_max_avg,fill=drought_characteristic))+geom_jitter()+geom_hline(yintercept=0)+
  geom_vline(xintercept = 0) +
  theme_bw()+
  gghighlight()+
  geom_label_repel(aes(label=label),max.overlaps = 20) + facet_wrap(~drought_characteristic);p0.2



# make 2: one with the market grayed out and one with the consevatin grayed out

p1 <- ggplot(data=outputs_poverty,aes(x=poverty_total_avg,y=poverty_perc_max_avg,label=label,fill=drought_characteristic))+geom_jitter()+
  geom_label_repel(max.overlaps = 20)+geom_hline(yintercept=0)+geom_vline(xintercept = 0) +theme_bw();p1 

p2 <- ggplot(data=outputs_hi,aes(x=upper_class_total_avg,y=upper_class_perc_max_avg,label=label,fill=drought_characteristic))+geom_jitter()+
  geom_label_repel(max.overlaps = 20) +geom_hline(yintercept=0)+geom_vline(xintercept = 0)+ theme_bw();p2


p2.1 <- ggplot(data=outputs_hi,aes(x=upper_class_total_avg,y=upper_class_perc_max_avg,fill=drought_characteristic))+geom_jitter()+geom_hline(yintercept=0)+
  geom_vline(xintercept = 0) +theme_bw()+gghighlight(mitigation_decision!="baseline")+geom_label_repel(aes(label=label),max.overlaps = 20) ;p2.1

