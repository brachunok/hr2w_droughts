# scenario affordability calculations 

library(ggplot2)
library(ggExtra)
library(reshape2)

load("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/processed_bills.Rdata")
load("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/processed_demands.Rdata")
load("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/processed_outputs.Rdata")

# change bills into % of income 
#bills <- bills[,-1]
#bills$date <- outputs$Date
incomes <- c(7500,12500,17500,22500,27500,32500,37500,42500,47500,55000,67500,87500,112500,137500,175000,250000)
hh_sizes <- c(1.870420, 1.875249, 2.201320, 2.373557 ,2.483636 ,2.583789 ,2.678085, 2.736607, 2.687910 ,2.792010, 2.883071, 2.947434, 3.085348, 3.139869 ,3.206045, 3.180690)
#hh_sizes2 <- c(2.72,2.72,2.72,2.72,2.72,2.72,2.72,2.72,2.72,2.72,2.72,2.72,2.72,2.72,2.72,2.72)
#hh_sizes <- c(1,3,3,3,1,1,1.67,1.4,1,1.57,1.86,3.17,3,3.57,4.58,5.38)
hh_counts <- c(2365,1648,1456,1285,1424,1027,1018,1077,874,1931,2352,4064,3110,2091,3263,4895)
bills_fraction <- bills
# for each column, divide each value by 1/12th of the income 
for (j in 1:length(incomes)){
  bills_fraction[,j] <- bills[,j]/(incomes[j]/12)*100
}

outputs$Date <- as.Date(outputs$Date)
# make figures of water bill for 47.5k and for 137.5k 
# selected_incomes <- bills[,c(9,13,17,18)]
# selected_incomes_plot <- melt(selected_incomes,id.vars = c("date","scenario"))
# 
# xmin_list_baseline <- as.Date(c("2009-05-01","2012-05-01","2013-05-01","2014-04-01"))
# xmax_list_baseline <- as.Date(c("2009-10-01","2012-10-01","2013-12-01","2015-12-01"))
# 
# xmin_list_d1 <- as.Date(c("2009-05-01","2013-05-01"))
# xmax_list_d1 <- as.Date(c("2009-10-01","2015-12-01"))
# 
# 
# # just plot before/during drought
# selected_incomes_plot <- selected_incomes_plot[which(selected_incomes_plot$date<=as.Date("2016-01-01")),]
# income_plot_one <- ggplot()+geom_line(data= selected_incomes_plot[which(selected_incomes_plot$variable=="X47500"),],aes(x=date,y=value,color=scenario))+facet_grid(rows=vars(variable),scales = "free")+
#   theme_bw()+ylab("Monthly Water Bill") + scale_color_manual(values = c("darkgray","darkred","darkblue","orange","purple"))+
#   annotate("rect",xmin=xmin_list_baseline,xmax=xmax_list_baseline,ymin=20,ymax=22.5,alpha=0.2,fill=c("orange","green","green","red"))+
#   annotate("rect",xmin=xmin_list_d1,xmax=xmax_list_d1,ymin=22.5,ymax=25,alpha=0.2,fill=c("orange","orange"));income_plot_one
# 
# 
# income_plot_two <- ggplot()+geom_line(data= selected_incomes_plot[which(selected_incomes_plot$variable!="X47500"),],aes(x=date,y=value,color=scenario))+facet_grid(rows=vars(variable),scales = "free")+
#   theme_bw()+ylab("Monthly Water Bill") + scale_color_manual(values = c("darkgray","darkred","darkblue","orange","purple"))+
#   annotate("rect",xmin=xmin_list_baseline,xmax=xmax_list_baseline,ymin=55,ymax=57.5,alpha=0.2,fill=c("orange","green","green","red"))+
#   annotate("rect",xmin=xmin_list_d1,xmax=xmax_list_d1,ymin=57.5,ymax=60,alpha=0.2,fill=c("orange","orange"));income_plot_two
# 
# # sum checks
#sum_checks <- dcast(selected_incomes,date~scenario,value.var=c("X47500"))
#sum_checks$year <- format(sum_checks$date,"%Y")
#sum_checks_aggregate <- aggregate(sum_checks[,c(2:3)],by=list(sum_checks$year),FUN=sum)

## compare fixed vs variable HH size ------------------------------------------------------------------------------------------
hh_size_comparison <- bills_fraction[complete.cases(bills_fraction),]
hh_size_comparison <- hh_size_comparison[which(hh_size_comparison$trial%in%c("before_Baseline","before_Baseline_Fixed_HH",
                                                                             "after_Baseline","after_Baseline_Fixed_HH",
                                                                             "after_Drought12_fixedHH")),]
# label based on fixed/variable HH size
hh_size_comparison$hh_size <- "variable_hh"
hh_size_comparison$hh_size[grepl(pattern = "HH",hh_size_comparison$trial)] <- "fixed_hh"

hh_size_comparison_plotting <- melt(hh_size_comparison,id.vars=c("trial","date","hh_size"))
hh_size_comparison_plotting$value <- as.numeric(hh_size_comparison_plotting$value)
hh_size_comparison_plot <- ggplot(data=hh_size_comparison_plotting,aes(fill=trial,y=value,x=variable))+geom_boxplot()+
  theme_bw()+xlab("")+ylab("% of household income spent on water")+ facet_grid(rows=vars(hh_size));hh_size_comparison_plot

# income group affordability changes  ------------------------------------------

MHI = 61000
CPM = 35923

# find the incomes with poverty, belowMHI, above,MHI numbers
deep_poverty <- paste0("X",incomes[1:3]) # 18000
poverty <- paste0("X",incomes[4:6])
near_poverty <- paste0("X",incomes[7:9])
middle_class <- paste0("X",incomes[10:13])
upper_class <-  paste0("X",incomes[1])


bills_threshold <- melt(bills,id.vars = c("period","trial"))
bills_threshold <- bills_threshold[complete.cases(bills_threshold),]
bills_threshold$value <- as.numeric(bills_threshold$value)

#bills_threshold <- bills
bills_threshold_aggregated <- aggregate(bills_threshold$value,by=list(bills_threshold$period,bills_threshold$variable,bills_threshold$trial),FUN=sum)
bills_threshold_aggregated_table <- dcast(bills_threshold_aggregated,formula = Group.2~Group.3)
# drop the last 2 rows
bills_threshold_aggregated_table <- bills_threshold_aggregated_table[-c(17,18),]

# now do the subtraction on the table and re-melt 
bills_threshold_aggregated_table$after_Baseline <- bills_threshold_aggregated_table$after_Baseline - bills_threshold_aggregated_table$before_Baseline
bills_threshold_aggregated_table$after_Drought12 <- bills_threshold_aggregated_table$after_Drought12 - bills_threshold_aggregated_table$before_Baseline
# drop all the other columns
bills_threshold_aggregated_table <- bills_threshold_aggregated_table[,which(colnames(bills_threshold_aggregated_table)%in%c("Group.2","after_Baseline","after_Drought12"))]
bills_threshold_aggregated <- melt(bills_threshold_aggregated_table)
colnames(bills_threshold_aggregated) <- c("income","period","income_change")

bills_threshold_aggregated$bin<-"Over MHI"
bills_threshold_aggregated$bin[which(bills_threshold_aggregated$income%in%belowCPM)] <- "Below Poverty Line"
bills_threshold_aggregated$bin[which(bills_threshold_aggregated$income%in%CPMtoMHI)] <- "Below MHI"
bills_threshold_aggregated$bin <- factor(bills_threshold_aggregated$bin,levels = c("Below Poverty Line","Below MHI","Over MHI"))

# also build bin5 which shows our 5 classes
bills_threshold_aggregated$bin5<-"Upper Class"
bills_threshold_aggregated$bin5[which(bills_threshold_aggregated$income%in%deep_poverty)] <- "Deep Poverty"
bills_threshold_aggregated$bin5[which(bills_threshold_aggregated$income%in%poverty)] <- "Poverty"
bills_threshold_aggregated$bin5[which(bills_threshold_aggregated$income%in%near_poverty)] <- "Near Poverty"
bills_threshold_aggregated$bin5[which(bills_threshold_aggregated$income%in%middle_class)] <- "Middle Class" 
bills_threshold_aggregated$bin5 <- factor(bills_threshold_aggregated$bin5,levels = c("Deep Poverty","Poverty","Near Poverty","Middle Class","Upper Class"))


# now average by bin and plot that
bills_threshold_aggregated_binned <- aggregate(bills_threshold_aggregated$income_change,by = list(bills_threshold_aggregated$period,bills_threshold_aggregated$bin5),FUN=mean)

threshold_plot_binned <- ggplot(bills_threshold_aggregated_binned,aes(y=x,x=Group.2,fill=Group.1))+geom_bar(stat='identity', position=position_dodge())
threshold_plot_binned <- threshold_plot_binned+theme_bw()+ylab("Annual Change in Household Water Bill ") +
  scale_fill_manual(values=c("lightblue","darkblue"),name="Conservation Policy",labels=c("Historical","Optimized"))+xlab("")+theme(legend.position = c(.25,.25))+
  theme(legend.background = element_rect(size=0.5, linetype="solid", 
                                   colour ="darkgray"))+scale_y_continuous(labels=scales::dollar_format())


threshold_plot_binned + ggtitle("Changes in Water Bills During Droughts by Income Class")
#ggsave(threshold_plot_binned,filename = "~/Documents/__college/reseach_stuff/hr2w_droughts/analysis/plots/binned_affordability.pdf",width = 6
      # ,height = 6,units = "in",scale=.85)


# before vs after baselien vs after scenario1  --------------------------------------------------------------------------------
# keep: baseline before, baseline after, drought 1 after 
bills_difference <- bills_fraction

trials <-c("before_Baseline","after_Baseline","after_Drought12")
bills_difference <- bills_difference[which(bills_difference$trial%in%trials),]
bills_difference <- melt(bills_difference,id.vars = c("trial","date"),measure.vars = colnames(bills_difference)[c(1:16)])

bills_difference$month <- format(bills_difference$date,"%m")
bills_difference$year <- format(bills_difference$date,"%Y")

bills_difference_table <- dcast(bills_difference,formula =month+variable~trial,value.var = "value")

# now compute differences
bills_difference_table$after_Baseline <- bills_difference_table$after_Baseline - bills_difference_table$before_Baseline
bills_difference_table$after_Drought12 <- bills_difference_table$after_Drought12 - bills_difference_table$before_Baseline

bills_difference <- melt(bills_difference_table,id.vars = c("variable","month"))
colnames(bills_difference) <- c("income","month","trial","billchange")

# remove just basleine
bills_difference <- bills_difference[which(bills_difference$trial!="before_Baseline"),]
bill_plot <- ggplot(bills_difference,aes(y=billchange,x=income,fill=trial))+geom_hline(yintercept=0,alpha=0.8)+
  geom_boxplot()+theme_bw()+ylab("Monthly Change in % of income spent on water vs Pre-drought");bill_plot

# let's look at average demand for each household 

# make histogram of demand before/after for each household under each conservation policy for each income class

demands_plot <- demands[which(!is.na(demands$period)),]
demands_plot <- demands_plot[,which(colnames(demands_plot)!="X")]
demands_plot[,c(1:16)] <- demands_plot[,c(1:16)]/748

demands_plot <- melt(demands_plot,id.vars = c("date","trial"))
demands_plot$value <- as.numeric(demands_plot$value)
demands_plot<- demands_plot[which(demands_plot$trial%in%c("before_Baseline","after_Baseline")),]

dp1 <- ggplot(demands_plot,aes(x=variable,y=value,fill=trial))+geom_boxplot()+theme_bw()+ylab("Monthly Household water use (CCF)")
dp1


