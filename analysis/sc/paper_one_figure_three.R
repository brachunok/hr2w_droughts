# figure 3, some sort of behavioral response

# 
load("~/Documents/__college/reseach_stuff/hr2w_droughts/analysis/processed_and_binned_bill_data_plot3.Rdata")

# now make plots which show how income elasticiy changes 

# on the left let's do what we had (total bill increase and total utility cost ), and on the right let's use one of the balance plot examples which shows all the different lines 

library('ggplot2')
library(cowplot)
library('reshape2')
library('ggpattern')

outputs_melt <- melt(outputs,measure.vars = c("deep_poverty_total_avg","total_utility_cost"),id.vars=c("drought_characteristic","price_elasticity","mitigation_decision","build_decision"),
                     value.name = "USD")

# change the names of build decisions
outputs_melt$build_decision[which(outputs_melt$build_decision=="desal")] <- "high_capacity"
outputs_melt$build_decision[which(outputs_melt$build_decision=="grrp-r")] <- "low_capacity"

# just baseline drought geom
outputs_melt <- outputs_melt[which(outputs_melt$drought_characteristic=="baseline.csv"),]

outputs_melt$combined <- paste0(outputs_melt$mitigation_decision,"_",outputs_melt$build_decision)
p1 <- ggplot()+geom_line(data=outputs_melt[which(outputs_melt$variable!="total_utility_cost"),],aes(x=price_elasticity,y=USD,color=build_decision,linetype=mitigation_decision))+theme_bw()
p1 <- p1+xlim(0,0.8) + ylab("Annual Bill Increase for Low Income Populations (USD)")+xlab("Price Elasticity")+scale_color_npg()

# put dots on the ones we want 
point_df <- data.frame(x=numeric(2),y=numeric(2))
point_df$x <-c(0.1,0.8)
point_df$y <- outputs_melt$USD[c(2,9)]
  
  
p1 <- p1 + geom_point(data=point_df,aes(x=x,y=y),shape=21,colour="blue",size=3,stroke=1,aes=0.2);p1

p1_no_leg <- p1 + theme(legend.position = "none")

p1_leg <- get_legend(p1)



# now pull out the billing plots for low-income populations for two of these scenarios 
# they are "market mitigaiton", build nothing, baseline drought (x=34,x=41)

outputs_scenario <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/plot3/34_outputs.csv")
outputs_scenario$Date <- as.Date(outputs_scenario$Date)

# get the bills
bills <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/plot3/1_hh_bills.csv")
bills$Date <- outputs_scenario$Date

bills$dp <- rowMeans(cbind(bills$X7500, bills$X12500, bills$X17500))
bills$hi <-
  rowMeans(cbind(bills$X137500, bills$X175000, bills$X250000))

# now get the DP and HI bills for the other three
bills$dp_demand_only <- rowMeans(cbind(bills$demandonly_7500,bills$demandonly_12500,bills$demandonly_17500))
bills$hi_demand_only <- rowMeans(cbind(bills$demandonly_137500,bills$demandonly_175000,bills$demandonly_250000))
# &^^ this one is the 'lower bound' 

bills$dp_rate_only <- rowMeans(cbind(bills$rateonly_7500,bills$rateonly_12500,bills$rateonly_17500))
bills$hi_rate_only <- rowMeans(cbind(bills$rateonly_137500,bills$rateonly_175000,bills$rateonly_250000))

bills$dp_no_change <-rowMeans(cbind(bills$raw_7500,bills$raw_12500,bills$raw_17500))
bills$hi_no_change <-rowMeans(cbind(bills$raw_137500,bills$raw_175000,bills$raw_250000))

bills_melted <-
  melt(
    bills,
    id.vars = "Date",
    measure.vars = c("dp", "hi"),
    variable.name = "Income_Group"
  )


# now add the 'what it should have been
bills_plot <- ggplot() + geom_line(data=bills,aes(x=Date,y=dp_no_change),linetype="dashed",color="gray") + theme_bw()

# now add the lower bound which is hi/li _demand_only
bills_plot <- bills_plot + geom_ribbon(data=bills,aes(x=Date,ymin=dp_demand_only,ymax=dp_no_change),alpha=0.5,fill="green")

# now the upper bound 
bills_plot <- bills_plot + geom_ribbon(data=bills,aes(x=Date,ymax=dp_rate_only,ymin=dp_no_change),alpha=0.5,fill="red")
bills_plot <- bills_plot + geom_line(data=bills,aes(x=Date,y=dp))
bills_plot+ xlim(c(as.Date("2012-10-01"),as.Date("2015-12-31")))+ylab("Low Income Bill Increase ($)")

bills_bar <- data.frame(class=character(4),increase=numeric(4),decrease=numeric(4),change=numeric(4))
bills_bar$class <- c("Low_Income","High_Income")
bills_bar$scenario <- c(1,1,2,2)
bills_bar$increase[1] <- sum(bills$dp_rate_only)-sum(bills$dp_no_change)
bills_bar$decrease[1] <- -1*(sum(bills$dp_no_change)-sum(bills$dp_demand_only))
bills_bar$change[1]   <- sum(bills$dp)-sum(bills$dp_no_change)

bills_bar$increase[2] <- sum(bills$hi_rate_only)-sum(bills$hi_no_change)
bills_bar$decrease[2] <- -1*(sum(bills$hi_no_change)-sum(bills$hi_demand_only))
bills_bar$change[2]   <- sum(bills$hi)-sum(bills$hi_no_change)

#bills_bar_melt <- melt(bills_bar)




# And now for the other one too ------------------------------------------------
bills <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/plot3/8_hh_bills.csv")
bills$Date <- outputs_scenario$Date

bills$dp <- rowMeans(cbind(bills$X7500, bills$X12500, bills$X17500))
bills$hi <-
  rowMeans(cbind(bills$X137500, bills$X175000, bills$X250000))

# now get the DP and HI bills for the other three
bills$dp_demand_only <- rowMeans(cbind(bills$demandonly_7500,bills$demandonly_12500,bills$demandonly_17500))
bills$hi_demand_only <- rowMeans(cbind(bills$demandonly_137500,bills$demandonly_175000,bills$demandonly_250000))
# &^^ this one is the 'lower bound' 

bills$dp_rate_only <- rowMeans(cbind(bills$rateonly_7500,bills$rateonly_12500,bills$rateonly_17500))
bills$hi_rate_only <- rowMeans(cbind(bills$rateonly_137500,bills$rateonly_175000,bills$rateonly_250000))

bills$dp_no_change <-rowMeans(cbind(bills$raw_7500,bills$raw_12500,bills$raw_17500))
bills$hi_no_change <-rowMeans(cbind(bills$raw_137500,bills$raw_175000,bills$raw_250000))

bills_melted <-
  melt(
    bills,
    id.vars = "Date",
    measure.vars = c("dp", "hi"),
    variable.name = "Income_Group"
  )


# now add the 'what it should have been
bills_plot <- ggplot() + geom_line(data=bills,aes(x=Date,y=dp_no_change),linetype="dashed",color="gray") + theme_bw()

# now add the lower bound which is hi/li _demand_only
bills_plot <- bills_plot + geom_ribbon(data=bills,aes(x=Date,ymin=dp_demand_only,ymax=dp_no_change),alpha=0.5,fill="green")

# now the upper bound 
bills_plot <- bills_plot + geom_ribbon(data=bills,aes(x=Date,ymax=dp_rate_only,ymin=dp_no_change),alpha=0.5,fill="red")
bills_plot <- bills_plot + geom_line(data=bills,aes(x=Date,y=dp))
bills_plot+ xlim(c(as.Date("2012-10-01"),as.Date("2015-12-31")))+ylab("Low Income Bill Increase ($)")


# stacked bar chart. 

#bills_bar$class <- c("Low_Income","High_Income")
bills_bar$increase[3] <- sum(bills$dp_rate_only)-sum(bills$dp_no_change)
bills_bar$decrease[3] <- -1*(sum(bills$dp_no_change)-sum(bills$dp_demand_only))
bills_bar$change[3]   <- sum(bills$dp)-sum(bills$dp_no_change)

bills_bar$increase[4] <- sum(bills$hi_rate_only)-sum(bills$hi_no_change)
bills_bar$decrease[4] <- -1*(sum(bills$hi_no_change)-sum(bills$hi_demand_only))
bills_bar$change[4]   <- sum(bills$hi)-sum(bills$hi_no_change)

bills_bar_melt <- melt(bills_bar,id.vars = c("class","scenario"))

bills_bar_melt_1 <- bills_bar_melt[which(bills_bar_melt$variable!="change"),]
bills_bar_melt_2 <- bills_bar_melt[which(bills_bar_melt$variable=="change"),]

#bills_bar_melt$class <- as.factor(bills_bar_melt)
width <- 0.7
p2_2 <- ggplot()+geom_bar(data= bills_bar_melt_1,aes(fill=variable,y=value,x=class),position="stack",stat="identity",alpha=0.45,width=width)+facet_grid(cols=vars(scenario))+theme_bw()+ylab("Yearly Change in \n Water Bills (USD)")+
  xlab("")+geom_bar_pattern(data=bills_bar_melt_2,aes(fill=variable,y=value,x=class),position="stack",stat="identity",width=width,pattern='stripe',fill=NA,color='black',alpha=0.7)+scale_fill_npg()+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )+ylim(-600,300)+scale_y_continuous(limits = c(-600,300),position="right")+geom_hline(yintercept = 0,alpha=0.7)

p2_2_leg <- get_legend(p2_2)
p2_2_no_leg <- p2_2 + theme(legend.position = "none")

# now combine all them
library("scales")
library("ggsci")
library(cowplot)
legends <- plot_grid(p1_leg,p2_2_leg,nrow=1,rel_heights = c(1,2))
p_right <- plot_grid(p2_2_no_leg,legends,nrow=2,rel_heights=c(2,1))
p_final <- plot_grid(p1_no_leg,p_right,rel_widths = c(1,1.5),rel_heights = c(1.5,1))
#ggsave(p_final,filename = "~/Documents/__college/reseach_stuff/hr2w_droughts/analysis/sc/draft_figure_3.pdf",width=5,height=4,unit="in")

# figure for presentation
p2_2 <- p2_2+ggtitle("Total Bill Increase for High and Low-income Households, Price Elasticity 0.1 vs 0.8")
#ggsave(p2_2,filename = "~/Documents/__college/reseach_stuff/hr2w_droughts/analysis/sc/draft_figure_3_DMDU_pres.pdf",width=7,height=4,unit="in")

