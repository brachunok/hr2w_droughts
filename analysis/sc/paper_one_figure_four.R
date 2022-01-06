# paper one figure 4: temporal changes in metrics

# comparing different performance metrics


# baseline 'do-nothing'
load("../processed_and_binned_bill_data_plot3.Rdata")

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
outputs_deep_poverty <- outputs_deep_poverty[which(outputs_deep_poverty$price_elasticity==0.1),]
outputs_poverty <- outputs_poverty[which(outputs_poverty$price_elasticity==0.1),]
outputs_hi <- outputs_hi[which(outputs_hi$price_elasticity==0.1),]

library(ggsci)
library('ggplot2')
library('ggrepel')
library('gghighlight')
library("ggpattern")
library("ggConvexHull")

# now only do it for the ones we see differences in
# this means: poverty_total_avg vs poverty_perc_max_avg
outputs_deep_poverty$label <- paste0(outputs_deep_poverty$mitigation_decision,"_",outputs_deep_poverty$build_decision)
outputs_poverty$label <- paste0(outputs_poverty$mitigation_decision,"_",outputs_poverty$build_decision)
outputs_hi$label <- paste0(outputs_hi$mitigation_decision,"_",outputs_hi$build_decision)

# rename low and high-capacity
outputs_deep_poverty$build_decision[which(outputs_deep_poverty$build_decision=="desal")] <- "high_capacity"
outputs_deep_poverty$build_decision[which(outputs_deep_poverty$build_decision=="grrp-r")] <- "low_capacity"

# calculate our one-to-one lines 
# best case line is x/12
# worst case line is y=x

p0 <- ggplot()+
  geom_segment(aes(x = 0, xend = 80, y = 0, yend = 80),color="gray",linetype="dashed",size=1.5)+
  geom_segment(aes(x=0,xend=300,y=0,yend=300/12),color="black",linetype="dashed",size=1.5)+
  geom_vline(xintercept = 0) +geom_hline(yintercept=0)+theme_bw()+
  #geom_convexhull(data=outputs_deep_poverty,aes(x=deep_poverty_total_avg,y=deep_poverty_perc_max_avg*13500/12,fill=mitigation_decision),alpha=0.5)
  scale_color_npg()+
  geom_point(data=outputs_deep_poverty,aes(x=deep_poverty_total_avg,y=deep_poverty_perc_max_avg*13500/12,shape=build_decision,color=mitigation_decision),size=4)+
  labs(x="Total Drought Bill Increase (USD)",y="Maximum Monthly Bill Increase (USD)",color="Mitigation Decision")+
  ggtitle("Total Bill Increase vs. Maximum Bill Increase ")

#p0_1


ggsave(p0,filename = "~/Documents/__college/reseach_stuff/hr2w_droughts/analysis/sc/draft_figure_4_v2.pdf",width=7,height=4,unit="in")
