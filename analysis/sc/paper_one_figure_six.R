# contributions of poor and rich to changes in cost 
load("~/Documents/__college/reseach_stuff/hr2w_droughts/analysis/processed_and_binned_bill_data_plot2.Rdata")

#outputs <- outputs[which(outputs$price_elasticity==0.5),]

#desal and GRRP-R are the non-dominated solutions based on capacity and LCW

# Now make some figures
library(reshape2)
library(ggplot2)

# going to melt the responses 
outputs_totals <- outputs[,c(1:12,44:47,161:165)] #these are ad-hoc columns to just get the total bill changes by parameter
outputs_perc_max <- outputs[,c(1:12,44:47,166:170)] 
outputs_perc_avg <- outputs[,c(1:12,44:47,171:175)] 

outputs_p1<- melt(outputs_totals,measure.vars = c("deep_poverty_total_avg","poverty_total_avg","near_poverty_total_avg","middle_class_total_avg","upper_class_total_avg"),
                  variable.name = "income_group",value.name = "annual_bill_change")

# now just look at the deep poverty and upper class groups 
outputs_p1 <- outputs_p1[which(outputs_p1$income_group%in%c("deep_poverty_total_avg","upper_class_total_avg")),]

# rename 
outputs_p1$build_decision[which(outputs_p1$build_decision=="desal")] <- "high_capacity"
outputs_p1$build_decision[which(outputs_p1$build_decision=="grrp-r")] <- "low_capacity"

#
library("scales")
library("ggsci")
library(cowplot)
library('ggpattern')



bills <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/plot2/0_hh_bills.csv")
percs <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/plot6/0_rev_contribs.csv")
percs <- percs[,-1]
# the (a)s are adjusted, the (b)s are baseline (non-adjusted). So the revenue loss is the 
# pairwise difference between each 
revenue_contributions <- percs[,c(17:32)]-percs[,c(1:16)]
revenue_contributions <- revenue_contributions[which(revenue_contributions[,1]!=0),]
revenue_contributions <- revenue_contributions/sum(revenue_contributions)

# now bin into the LI/HI groups
revenue_contributions$LI <- sum(revenue_contributions$b7500,revenue_contributions$b12500,revenue_contributions$b17500)
revenue_contributions$HI <- sum(revenue_contributions$b137500,revenue_contributions$b175000,revenue_contributions$b250000)
revenue_contributions$MC <- 1-revenue_contributions$LI - revenue_contributions$HI

bills$Date <- outputfordate$Date

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

bills_bar <- data.frame(class=character(2),increase=numeric(2),decrease=numeric(2),change=numeric(2))
bills_bar$class <- c("Low_Income","High_Income")
bills_bar$increase[1] <- sum(bills$dp_rate_only)-sum(bills$dp_no_change)
bills_bar$decrease[1] <- -1*(sum(bills$dp_no_change)-sum(bills$dp_demand_only))
bills_bar$change[1]   <- sum(bills$dp)-sum(bills$dp_no_change)

bills_bar$increase[2] <- sum(bills$hi_rate_only)-sum(bills$hi_no_change)
bills_bar$decrease[2] <- -1*(sum(bills$hi_no_change)-sum(bills$hi_demand_only))
bills_bar$change[2]   <- sum(bills$hi)-sum(bills$hi_no_change)

bills_bar_melt <- melt(bills_bar,id.vars = c("class"))

bills_bar_melt_1 <- bills_bar_melt[which(bills_bar_melt$variable!="change"),]
bills_bar_melt_2 <- bills_bar_melt[which(bills_bar_melt$variable=="change"),]

#bills_bar_melt$class <- as.factor(bills_bar_melt)
width <- 0.7
income_order <- c("Low_Income","High_Income")

# doing some ad-hoc bullshit
plot_data <- data.frame(class=character(8),variable=character(8),value=numeric(8))
plot_data$class <- c("Low_Income","Low_Income","Low_Income","Low_Income","High_Income","High_Income","High_Income","High_Income")
plot_data$variable <- c("Increase_from_LI","Increase_from_MC","Increase_from_HI","Decrease")

plot_data$value <- c(bills_bar_melt_1$value[1]*revenue_contributions[,c('LI',"MC","HI")],bills_bar_melt_1$value[3],bills_bar_melt_1$value[1]*revenue_contributions[,c('LI',"MC","HI")],bills_bar_melt_1$value[4])
plot_data$value <- as.numeric(plot_data$value)
# PLOT DATA USED TO BE bills_bar_melt_1
p3<- ggplot()+geom_bar(data= plot_data,aes(fill=variable,y=value,x=class),position="stack",stat="identity",alpha=0.45,width=width)+theme_bw()+ylab("Yearly Change in \n Water Bills (USD) ")+
  xlab("")+geom_bar_pattern(data=bills_bar_melt_2,aes(fill=variable,y=value,x=class),position="stack",stat="identity",width=width,pattern='stripe',fill=NA,color='black',alpha=0.7)+scale_fill_npg()+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )+scale_y_continuous(limits = c(-500,500),position="right")+geom_hline(yintercept = 0,alpha=0.7)+
  scale_x_discrete(limits=income_order)
p3
p3_leg <- get_legend(p3)
p3_no_leg <- p3 + theme(legend.position = "none")
