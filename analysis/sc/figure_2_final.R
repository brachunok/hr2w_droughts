#FINAL FIGURE 2
library("scales")
library("ggsci")
library(cowplot)
library('ggpattern')
library('ggrepel')

options(scipen=999)
load("~/Documents/__college/reseach_stuff/hr2w_droughts/analysis/processed_and_binned_bill_data_plot_responses.Rdata")
og_outputs <- outputs
elasticities_values <- unique(outputs$price_elasticity)
ela <- elasticities_values[2]


# cross price of 0.013, PED of 0, YED of 0.15 is the "low income doesn't change, high income does"
# PED = 0.325, CPE= -0.013, is the "low income does change, high income doesn't"



 outputs <- og_outputs[which(og_outputs$price_elasticity==0.35),]
  
  # also go through and make sure we have the cross prices as 0 and the income elasticity set 
  
 outputs <- outputs[which(outputs$cpe_squared==0.0),]
 outputs <- outputs[which(outputs$income_elasticity==0.15),]
  
  #desal and GRRP-R are the non-dominated solutions based on capacity and LCW
  
  # Now make some figures
  library(reshape2)
  library(ggplot2)
  
  # going to melt the responses 
  outputs_totals <- outputs[,c(1:15,48:51,164:168)] #these are ad-hoc columns to just get the total bill changes by parameter
  outputs_perc_max <- outputs[,c(1:15,48:51,169:173)] 
  outputs_perc_avg <- outputs[,c(1:15,48:51,174:178)] 
  outputs_perc_total <- outputs[,c(1:15,48:51,179:184)] 
  
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
  
  outputs_p1_tc <- outputs_p1[which(outputs_p1$income_group=="total_utility_cost"),]
  outputs_p1 <- outputs_p1[which(outputs_p1$income_group!="total_utility_cost"),]
  
  # look at how reservoir size changes
  build_order <- c("None","Low-Capacity","High-Capacity")
  mitigate_order <- c("market","baseline")
  
  # figure ouf which of these is the minimum per facet 
  # (1) melt stats together so we can compare across deep poverty, high income, and total cost
  
  outputs_p1_agg <- aggregate(outputs_p1$annual_bill_change,by=list(outputs_p1$income_group,outputs_p1$drought_characteristic),FUN="min")
  outputs_p1_agg_TC <- aggregate(outputs_p1_tc$annual_bill_change,by=list(outputs_p1_tc$income_group,outputs_p1_tc$drought_characteristic),FUN="min")
  
  outputs_p1$label <- ""
  outputs_p1$label[which(outputs_p1$annual_bill_change%in%outputs_p1_agg$x)] <- "" # this is an X to add the X same with below
  
  outputs_p1_tc$label <- ""
  outputs_p1_tc$label[which(outputs_p1_tc$annual_bill_change%in%outputs_p1_agg_TC$x)] <- ""
  
  p1 <- ggplot()+
    geom_bar_pattern(data=outputs_p1,aes(x=build_decision,y=annual_bill_change,fill=build_decision,pattern=mitigation_decision),stat='identity',position=position_dodge(),color="black") +
    facet_grid(rows=vars(drought_characteristic),cols=vars(income_group),scales="free_x") + theme_bw()  + scale_pattern_manual(values=c(market="none",baseline='circle'))+
    scale_fill_npg() +geom_hline(yintercept = 0)+labs(x="",y="Total Bill Change (USD/yr)", pattern="Mitigation decision",fill="Infrastructure")+
    scale_x_discrete(limits=build_order)+ geom_text(data=outputs_p1,aes(label=label,x=build_decision,y=annual_bill_change,group=mitigation_decision),position=position_dodge(width=0.8))+
    geom_label_repel(data=outputs_p1,aes(x=build_decision,y=annual_bill_change,label=round(annual_bill_change,digits=2)))
  p1
  
  p1_tc <- ggplot()+ geom_bar_pattern(data=outputs_p1_tc,aes(x=build_decision,y=annual_bill_change/1000000,fill=build_decision,pattern=mitigation_decision),stat='identity',position='dodge',color="black") +
    facet_grid(rows=vars(drought_characteristic),cols=vars(income_group),scales="free_x") + theme_bw()  + scale_pattern_manual(values=c(market="none",baseline='circle'))+
    scale_fill_npg() +geom_hline(yintercept = 0)+labs(x="",y="Total Utility Cost (million USD/yr)", pattern="Mitigation decision",fill="Infrastructure")+
    scale_x_discrete(limits=build_order) + scale_y_continuous(position = "right",limits=c(-66.6,100))+
    geom_text(data=outputs_p1_tc,aes(label=label,x=build_decision,y=annual_bill_change,group=mitigation_decision),vjust=-.6,position=position_dodge(width=0.8))+
    geom_label_repel(data=outputs_p1_tc,aes(x=build_decision,y=annual_bill_change/1000000,label=round(annual_bill_change/1000000,digits=2)))
  #p1_tc
  
  p1_tc_no_leg <- p1_tc + theme(legend.position="none")
  p1_no_leg <- p1 + theme(legend.position = "none")+ theme(strip.text.y = element_blank())
  p1_leg <- get_legend(p1)
  title <- ggdraw() 
  
  p1_DMDU <- plot_grid(p1_leg,p1_no_leg,p1_tc_no_leg,nrow=1,rel_widths = c(.5,2,1.3),align = "v",axis = 'tb')

  p1_DMDU
  print(ela)
  #ggsave(p1_DMDU,filename = paste0("~/Documents/__college/reseach_stuff/hr2w_droughts/analysis/sc/figure_2_plots/final_figure_2_revisions.pdf"),width=6,height=4,unit="in",scale=1.25)
  
  
