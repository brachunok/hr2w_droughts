# draft figure 2
#
library("scales")
library("ggsci")
library(cowplot)
library(ggrepel)
library('ggpattern')

options(scipen=999)
load("~/Documents/__college/reseach_stuff/hr2w_droughts/analysis/processed_and_binned_bill_data_plot8.Rdata")
og_outputs <- outputs
elasticities_values <- unique(outputs$price_elasticity)

for (ela in elasticities_values){
  outputs <- og_outputs[which(og_outputs$price_elasticity==ela),]
  
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
  outputs_p1$build_decision[which(outputs_p1$build_decision=="desal_old")] <- "old-high-cap"
  #outputs_p1$build_decision[which(outputs_p1$build_decision=="grrp-r")] <- "Low-Capacity"
  outputs_p1$build_decision[which(outputs_p1$build_decision=="none")] <- "None"
  
  # just doing long/intense for now 
  #outputs_p1 <-outputs_p1[which(!outputs_p1$drought_characteristic%in%c("long_intense.csv","baseline.csv")),]
  
  outputs_p1_tc <- outputs_p1[which(outputs_p1$income_group=="total_utility_cost"),]
  outputs_p1 <- outputs_p1[which(outputs_p1$income_group!="total_utility_cost"),]
  
  # look at how reservoir size changes
  build_order <- c("None","old-high-cap","High-Capacity")
  mitigate_order <- c("market","baseline")
  
  # figure ouf which of these is the minimum per facet 
  # (1) melt stats together so we can compare across deep poverty, high income, and total cost
  
  outputs_p1_agg <- aggregate(outputs_p1$annual_bill_change,by=list(outputs_p1$income_group,outputs_p1$drought_characteristic),FUN="min")
  outputs_p1_agg_TC <- aggregate(outputs_p1_tc$annual_bill_change,by=list(outputs_p1_tc$income_group,outputs_p1_tc$drought_characteristic),FUN="min")
  
  outputs_p1$label <- ""
  outputs_p1$label[which(outputs_p1$annual_bill_change%in%outputs_p1_agg$x)] <- "X"
  
  outputs_p1_tc$label <- ""
  outputs_p1_tc$label[which(outputs_p1_tc$annual_bill_change%in%outputs_p1_agg_TC$x)] <- "X"
  
  p1 <- ggplot()+
    geom_bar_pattern(data=outputs_p1,aes(x=build_decision,y=annual_bill_change,fill=build_decision,pattern=mitigation_decision),stat='identity',position=position_dodge(),color="black") +
    facet_grid(rows=vars(drought_characteristic),cols=vars(income_group),scales="free_x") + theme_bw()  + scale_pattern_manual(values=c(market="none",baseline='circle'))+
    scale_fill_npg() +geom_hline(yintercept = 0)+labs(x="",y="Total Bill Change", pattern="Mitigation decision",fill="Infrastructure")+
    scale_x_discrete(limits=build_order)+ geom_text(data=outputs_p1,aes(label=label,x=build_decision,y=annual_bill_change,group=mitigation_decision),position=position_dodge(width=0.8))+
    geom_label_repel(data=outputs_p1,aes(x=build_decision,y=signif(annual_bill_change,digits=4),label = signif(annual_bill_change,digits=4)), colour = "black")
  #p1
  
  p1_tc <- ggplot()+ geom_bar_pattern(data=outputs_p1_tc,aes(x=build_decision,y=annual_bill_change,fill=build_decision,pattern=mitigation_decision),stat='identity',position='dodge',color="black") +
    facet_grid(rows=vars(drought_characteristic),cols=vars(income_group),scales="free_x") + theme_bw()  + scale_pattern_manual(values=c(market="none",baseline='circle'))+
    scale_fill_npg() +geom_hline(yintercept = 0)+labs(x="",y="Total Utility Cost (USD)", pattern="Mitigation decision",fill="Infrastructure")+
    scale_x_discrete(limits=build_order) + scale_y_continuous(position = "right")+
    geom_text(data=outputs_p1_tc,aes(label=label,x=build_decision,y=annual_bill_change,group=mitigation_decision),vjust=-.6,position=position_dodge(width=0.8))+
    geom_label_repel(data=outputs_p1_tc,aes(x=build_decision,y=signif(annual_bill_change,digits=4),label = signif(annual_bill_change,digits=4)), colour = "black")
  
  p1_tc_no_leg <- p1_tc + theme(legend.position="none")
  p1_no_leg <- p1 + theme(legend.position = "none")+ theme(strip.text.y = element_blank())
  p1_leg <- get_legend(p1)
  title <- ggdraw() + draw_label(paste0("Total bill change, Elasticity: ",ela), fontface="bold")
  
  p1_DMDU <- plot_grid(p1_leg,p1_no_leg,p1_tc_no_leg,nrow=1,rel_widths = c(.5,2,1.3),align = "v",axis = 'tb')
  p1_DMDU <- plot_grid(title,p1_DMDU,nrow=2,align="h",rel_heights = c(.05,1))
  print(ela)
  ggsave(p1_DMDU,filename = paste0("~/Documents/__college/reseach_stuff/hr2w_droughts/analysis/sc/figure_2_plots/draft_figure_2_high_rates",ela,".png"),width=8,height=4,unit="in",scale=1.25)
  
  p1_nl <- ggplot()+
    geom_bar_pattern(data=outputs_p1,aes(x=build_decision,y=annual_bill_change,fill=build_decision,pattern=mitigation_decision),stat='identity',position=position_dodge(),color="black") +
    facet_grid(rows=vars(drought_characteristic),cols=vars(income_group),scales="free_x") + theme_bw()  + scale_pattern_manual(values=c(market="none",baseline='circle'))+
    scale_fill_npg() +geom_hline(yintercept = 0)+labs(x="",y="Total Bill Change", pattern="Mitigation decision",fill="Infrastructure")+
    scale_x_discrete(limits=build_order)+ geom_text(data=outputs_p1,aes(label=label,x=build_decision,y=annual_bill_change,group=mitigation_decision),position=position_dodge(width=0.8))
  #p1
  
  p1_tc_nl <- ggplot()+ geom_bar_pattern(data=outputs_p1_tc,aes(x=build_decision,y=annual_bill_change,fill=build_decision,pattern=mitigation_decision),stat='identity',position='dodge',color="black") +
    facet_grid(rows=vars(drought_characteristic),cols=vars(income_group),scales="free_x") + theme_bw()  + scale_pattern_manual(values=c(market="none",baseline='circle'))+
    scale_fill_npg() +geom_hline(yintercept = 0)+labs(x="",y="Total Utility Cost (USD)", pattern="Mitigation decision",fill="Infrastructure")+
    scale_x_discrete(limits=build_order) + scale_y_continuous(position = "right")+
    geom_text(data=outputs_p1_tc,aes(label=label,x=build_decision,y=annual_bill_change,group=mitigation_decision),vjust=-.6,position=position_dodge(width=0.8))
  
  p1_tc_no_leg <- p1_tc_nl + theme(legend.position="none")
  p1_no_leg <- p1_nl + theme(legend.position = "none")+ theme(strip.text.y = element_blank())
  p1_leg <- get_legend(p1)
  title <- ggdraw() + draw_label(paste0("Total bill change, Elasticity: ",ela), fontface="bold")
  
  p1_DMDU <- plot_grid(p1_leg,p1_no_leg,p1_tc_no_leg,nrow=1,rel_widths = c(.5,2,1.3),align = "v",axis = 'tb')
  p1_DMDU <- plot_grid(title,p1_DMDU,nrow=2,align="h",rel_heights = c(.05,1))
  print(ela)
  ggsave(p1_DMDU,filename = paste0("~/Documents/__college/reseach_stuff/hr2w_droughts/analysis/sc/figure_2_plots/draft_figure_2_high_rates_no_lab",ela,".png"),width=8,height=4,unit="in",scale=1.25)
  
  
  # i want insets on all sides showing the differences
  
  # FIGURING OUT WHAT/WHY
  #slide 8 of weekly report 11/08/21
  # x = 0,22,1
  # baseline, build low, curtailment x = 22
  # intense, build none, curtailment x = 132
  
  # left is going to be giving examples as to why lock-in is bad 
  # lock-in is bad because if we build then have to conserve it's not enough so the bill inrease is more
  
  # so let's show 3 overlapping bills (1) is build nothing, conserve, historical drought. (scenario 0) (2) bill if we build low-capacity, baseline drought(scenario 2), 
  #(3) bill if we build low-capacity in intense drought (scenario 14)
  # 
  # outputfordate <-read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/plot2/0_outputs.csv")
  # bill_bl <-read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/plot2/0_hh_bills.csv")
  # bill_lc <-read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/plot2/2_hh_bills.csv")
  # bill_li <-read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/plot2/14_hh_bills.csv")
  # 
  # bills_p2 <- data.frame(baseline=cbind(rowMeans(cbind(bill_bl$X7500,bill_bl$X12500,bill_bl$X17500))))
  # bills_p2$low_good <- cbind(rowMeans(cbind(bill_lc$X7500,bill_lc$X12500,bill_lc$X17500)))
  # bills_p2$low_bad <- cbind(rowMeans(cbind(bill_li$X7500,bill_li$X12500,bill_li$X17500)))
  # bills_p2$date <- as.Date(outputfordate$Date)
  # 
  # bills_p2_melt <- melt(bills_p2,id.vars = "date")
  # 
  # ll_date = as.Date("2012-01-01")
  # ul_date = as.Date("2014-01-01")
  # 
  # p2 <- ggplot() + geom_line(data=bills_p2_melt,aes(x=date,y=value,color=variable))+theme_bw()+scale_color_npg()+xlim(ll_date,ul_date)+
  #   geom_vline(xintercept = as.Date("2013-03-01"),linetype="dashed")+ xlab("Date")+ylab("Low Income Household \n Water Bill ")+labs(color="Scenario")
  # 
  # p2 <- p2 + geom_ribbon(data=bills_p2,aes(x=date,ymin=low_good,ymax=low_bad),fill="grey",alpha=0.3)+scale_y_continuous(limits = c(45,70),position="right")
  # p2_no_leg <- p2 + theme(legend.position="none")
  # p2_leg <- get_legend(p2)
  # # idea: fill the baselien with a shaded gray area, then color the increases 
  # # make the date earlier. just 2012,2013
  # 
  # # third plot is going to show the difference between low and high-income bills similar to figure 3
  # 
  # 
  # bills <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/plot2/0_hh_bills.csv")
  # bills$Date <- outputfordate$Date
  # 
  # bills$dp <- rowMeans(cbind(bills$X7500, bills$X12500, bills$X17500))
  # bills$hi <-
  #   rowMeans(cbind(bills$X137500, bills$X175000, bills$X250000))
  # 
  # # now get the DP and HI bills for the other three
  # bills$dp_demand_only <- rowMeans(cbind(bills$demandonly_7500,bills$demandonly_12500,bills$demandonly_17500))
  # bills$hi_demand_only <- rowMeans(cbind(bills$demandonly_137500,bills$demandonly_175000,bills$demandonly_250000))
  # # &^^ this one is the 'lower bound' 
  # 
  # bills$dp_rate_only <- rowMeans(cbind(bills$rateonly_7500,bills$rateonly_12500,bills$rateonly_17500))
  # bills$hi_rate_only <- rowMeans(cbind(bills$rateonly_137500,bills$rateonly_175000,bills$rateonly_250000))
  # 
  # bills$dp_no_change <-rowMeans(cbind(bills$raw_7500,bills$raw_12500,bills$raw_17500))
  # bills$hi_no_change <-rowMeans(cbind(bills$raw_137500,bills$raw_175000,bills$raw_250000))
  # 
  # bills_bar <- data.frame(class=character(2),increase=numeric(2),decrease=numeric(2),change=numeric(2))
  # bills_bar$class <- c("Low_Income","High_Income")
  # bills_bar$increase[1] <- sum(bills$dp_rate_only)-sum(bills$dp_no_change)
  # bills_bar$decrease[1] <- -1*(sum(bills$dp_no_change)-sum(bills$dp_demand_only))
  # bills_bar$change[1]   <- sum(bills$dp)-sum(bills$dp_no_change)
  # 
  # bills_bar$increase[2] <- sum(bills$hi_rate_only)-sum(bills$hi_no_change)
  # bills_bar$decrease[2] <- -1*(sum(bills$hi_no_change)-sum(bills$hi_demand_only))
  # bills_bar$change[2]   <- sum(bills$hi)-sum(bills$hi_no_change)
  # 
  # bills_bar_melt <- melt(bills_bar,id.vars = c("class"))
  # 
  # bills_bar_melt_1 <- bills_bar_melt[which(bills_bar_melt$variable!="change"),]
  # bills_bar_melt_2 <- bills_bar_melt[which(bills_bar_melt$variable=="change"),]
  # 
  # #bills_bar_melt$class <- as.factor(bills_bar_melt)
  # width <- 0.7
  # income_order <- c("Low_Income","High_Income")
  # 
  # p3<- ggplot()+geom_bar(data= bills_bar_melt_1,aes(fill=variable,y=value,x=class),position="stack",stat="identity",alpha=0.45,width=width)+theme_bw()+ylab("Yearly Change in \n Water Bills (USD) ")+
  #   xlab("")+geom_bar_pattern(data=bills_bar_melt_2,aes(fill=variable,y=value,x=class),position="stack",stat="identity",width=width,pattern='stripe',fill=NA,color='black',alpha=0.7)+scale_fill_npg()+
  #   theme(
  #     strip.background = element_blank(),
  #     strip.text.x = element_blank()
  #   )+scale_y_continuous(limits = c(-500,500),position="right")+geom_hline(yintercept = 0,alpha=0.7)+
  #   scale_x_discrete(limits=income_order)
  # #p3
  # p3_leg <- get_legend(p3)
  # p3_no_leg <- p3 + theme(legend.position = "none")
  # 
  # #p2 gets some sort of title saying "example of lockin"
  # #p2 goes on the bottom of the right side
  # 
  # #p3 is going to be an example showing low and high-income bills and is going to go in the top right. 
  # #legend goes on the left of annual bill change 
  # # 
  # #ggsave(p3_no_leg,filename="~/Documents/__college/reseach_stuff/hr2w_droughts/analysis/sc/draft_figure_2b_DMDU.pdf",width=4,height=4,units = "in")
  # 
  # p_left <- plot_grid(p1_leg,p1_no_leg,nrow=1,rel_widths = c(.25,1))
  # p_leg <- plot_grid(p2_leg,p3_leg,nrow=1)
  # p_right <- plot_grid(p3_no_leg,p_leg,p2_no_leg,ncol=1,rel_heights = c(1,.35,1))
  # p_final <- plot_grid(p_left,p_right,ncol=2,rel_widths=c(.6,.4))
}

#ggsave(p_final,filename = "~/Documents/__college/reseach_stuff/hr2w_droughts/analysis/sc/draft_figure_2.pdf",width=8,height=4,unit="in",scale = 1.25)
