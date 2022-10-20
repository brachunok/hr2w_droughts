# analysis of 'what if we had already had high water rates' 

#
library("scales")
library("ggsci")
library(cowplot)
library(ggrepel)
library('ggpattern')

options(scipen=999)
load("~/Documents/__college/reseach_stuff/hr2w_droughts/analysis/processed_and_binned_bill_data_plot10_TEST.Rdata")
og_outputs <- outputs

ela = 0.1
#desal and GRRP-R are the non-dominated solutions based on capacity and LCW

# Now make some figures
library(reshape2)
library(ggplot2)

# going to melt the responses 
outputs_totals <- outputs[,c(1:13,46:49,162:166)] #these are ad-hoc columns to just get the total bill changes by parameter
outputs_perc_max <- outputs[,c(1:13,46:49,167:171)] 
outputs_perc_avg <- outputs[,c(1:13,46:49,172:176)] 
outputs_perc_total <- outputs[,c(1:13,46:49,182:186)] 

outputs_p1<- melt(outputs_totals,measure.vars = c("deep_poverty_total_avg","poverty_total_avg","near_poverty_total_avg","middle_class_total_avg","upper_class_total_avg","total_utility_cost"),
                  variable.name = "income_group",value.name = "annual_bill_change")

# now just look at the deep poverty and upper class groups 
outputs_p1 <- outputs_p1[which(outputs_p1$income_group%in%c("deep_poverty_total_avg","upper_class_total_avg","total_utility_cost")),]

#outputs_p1_tc <- outputs_p1[which(outputs_p1$income_group%in%c("total_utility_cost")),]
# rename 
#outputs_p1$build_decision[which(outputs_p1$build_decision=="desal")] <- "High-Capacity"
#outputs_p1$build_decision[which(outputs_p1$build_decision=="desal_old")] <- "old-high-cap"
#outputs_p1$build_decision[which(outputs_p1$build_decision=="grrp-r")] <- "Low-Capacity"
outputs_p1$build_decision[which(outputs_p1$build_decision=="none")] <- "None"

# just doing long/intense for now 
#outputs_p1 <-outputs_p1[which(!outputs_p1$drought_characteristic%in%c("long_intense.csv","baseline.csv")),]

outputs_p1_tc <- outputs_p1[which(outputs_p1$income_group=="total_utility_cost"),]
outputs_p1 <- outputs_p1[which(outputs_p1$income_group!="total_utility_cost"),]

# look at how reservoir size changes
#build_order <- c("None","old-high-cap","High-Capacity")
mitigate_order <- c("market","baseline")
lock_in_order <- c("old","lockin")
# figure ouf which of these is the minimum per facet 
# (1) melt stats together so we can compare across deep poverty, high income, and total cost

outputs_p1_agg <- aggregate(outputs_p1$annual_bill_change,by=list(outputs_p1$income_group,outputs_p1$drought_characteristic),FUN="min")
outputs_p1_agg_TC <- aggregate(outputs_p1_tc$annual_bill_change,by=list(outputs_p1_tc$income_group,outputs_p1_tc$drought_characteristic),FUN="min")


p1 <- ggplot()+
  geom_bar_pattern(data=outputs_p1,aes(x=rate_structure,y=annual_bill_change,fill=rate_structure,pattern=mitigation_decision),stat='identity',position=position_dodge(),color="black") +
  facet_grid(rows=vars(drought_characteristic),cols=vars(income_group),scales="free_x") + theme_bw()  + scale_pattern_manual(values=c(market="none",baseline='circle'))+
  scale_fill_npg() +geom_hline(yintercept = 0)+labs(x="",y="Total Bill Change", pattern="Mitigation decision",fill="Infrastructure")+scale_x_discrete(limits=lock_in_order)
p1


ggsave(p1,filename = paste0("~/Documents/__college/reseach_stuff/hr2w_droughts/analysis/sc/lock_in_figure_final.pdf"),width=5,height=4,unit="in",scale=1.25)




