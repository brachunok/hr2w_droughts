# scenario affordability calculations 

library(ggplot2)
library(ggExtra)
library(reshape2)

# for now, just make an ad-hoc dataframe 
bl_bills <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/baseline/hh_bills.csv")
bl_demands <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/baseline/hh_demand.csv")
bl_outputs <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/baseline/outputs.csv")
bl_outputs$Date <- as.Date(as.character(bl_outputs$Date))

d1_bills <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/drought1/hh_bills.csv")
d1_demands <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/drought1/hh_demand.csv")
d1_outputs <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/drought1/outputs.csv")
d1_outputs$Date <- as.Date(as.character(d1_outputs$Date))

# now label 
bl_bills$scenario = "baseline"
bl_demands$scenario = "baseline"
bl_outputs$scenario = "baseline"

d1_bills$scenario = "drought1"
d1_demands$scenario = "drought1"
d1_outputs$scenario = "drought1"

# now combine them 
bills <- rbind(bl_bills,d1_bills)
demands <- rbind(bl_demands,d1_demands)
outputs <- rbind(bl_outputs,d1_outputs)

# change bills into % of income 
bills <- bills[,-1]
bills$date <- outputs$Date
incomes <- c(7500,12500,17500,22500,27500,32500,37500,42500,47500,55000,67500,87500,112500,137500,175000,250000)
hh_sizes <- c(1,3,3,3,1,1,1.67,1.4,1,1.57,1.86,3.17,3,3.57,4.58,5.38)
hh_counts <- c(2365,1648,1456,1285,1424,1027,1018,1077,874,1931,2352,4064,3110,2091,3263,4895)
bills_fraction <- bills
# for each column, divide each value by 1/12th of the income 
for (j in 1:length(incomes)){
  bills_fraction[,j] <- bills[,j]/(incomes[j]/12)*100
}


# make figures of water bill for 47.5k and for 137.5k 
selected_incomes <- bills[,c(9,13,17,18)]
selected_incomes_plot <- melt(selected_incomes,id.vars = c("date","scenario"))

xmin_list_baseline <- as.Date(c("2009-05-01","2012-05-01","2013-05-01","2014-04-01"))
xmax_list_baseline <- as.Date(c("2009-10-01","2012-10-01","2013-12-01","2015-12-01"))

xmin_list_d1 <- as.Date(c("2009-05-01","2013-05-01"))
xmax_list_d1 <- as.Date(c("2009-10-01","2015-12-01"))


# just plot before/during drought
selected_incomes_plot <- selected_incomes_plot[which(selected_incomes_plot$date<=as.Date("2016-01-01")),]
income_plot_one <- ggplot()+geom_line(data= selected_incomes_plot[which(selected_incomes_plot$variable=="X47500"),],aes(x=date,y=value,color=scenario))+facet_grid(rows=vars(variable),scales = "free")+
  theme_bw()+ylab("Monthly Water Bill") + scale_color_manual(values = c("darkgray","darkred"))+
  annotate("rect",xmin=xmin_list_baseline,xmax=xmax_list_baseline,ymin=20,ymax=22.5,alpha=0.2,fill=c("orange","green","green","red"))+
  annotate("rect",xmin=xmin_list_d1,xmax=xmax_list_d1,ymin=22.5,ymax=25,alpha=0.2,fill=c("orange","orange"));income_plot_one


income_plot_two <- ggplot()+geom_line(data= selected_incomes_plot[which(selected_incomes_plot$variable!="X47500"),],aes(x=date,y=value,color=scenario))+facet_grid(rows=vars(variable),scales = "free")+
  theme_bw()+ylab("Monthly Water Bill") + scale_color_manual(values = c("darkgray","darkred"))+
  annotate("rect",xmin=xmin_list_baseline,xmax=xmax_list_baseline,ymin=55,ymax=57.5,alpha=0.2,fill=c("orange","green","green","red"))+
  annotate("rect",xmin=xmin_list_d1,xmax=xmax_list_d1,ymin=57.5,ymax=60,alpha=0.2,fill=c("orange","orange"));income_plot_two


# now define a before and after period
before_year <- 2010
after_year  <- 2015

# label everything with a before year and an after year 
bills_fraction$period <- NA
bills_fraction$period[format(bills_fraction$date,"%Y")%in%before_year] <- "before"
bills_fraction$period[format(bills_fraction$date,"%Y")%in%after_year] <- "after"

# calculate affordability for the before period and after period and for each scenario and income class
bills_plotting <- melt(bills_fraction,id.vars = c("period","date","scenario"))
bills_plotting <- bills_plotting[!is.na(bills_plotting$period),]

# before vs afterbaselien vs after scenario1 
# keep: baseline before, baseline after, drought 1 after 
bills_plotting$period <- paste0(bills_plotting$period,"_",bills_plotting$scenario)
keep <- c("before_baseline","after_baseline","after_drought1")
bills_plotting <- bills_plotting[which(bills_plotting$period%in%keep),]

# rename scenarios
bills_plotting$period[which(bills_plotting$period=="before_baseline")] ="before"
bills_plotting$period[which(bills_plotting$period=="after_baseline")] ="after_baseline"
bills_plotting$period[which(bills_plotting$period=="after_drought1")] ="after_drought1"

# now just pull out the month 
bills_plotting$month <- format(bills_plotting$date,"%m")
bills_plotting$year <- format(bills_plotting$date,"%Y")

bills_plotting$period <- factor(bills_plotting$period,levels=c("before","after_baseline","after_drought1"))

bill_plot <- ggplot(bills_plotting,aes(y=value,x=variable,fill=period))+geom_boxplot()+theme_bw()+ylab("% of household income on water");bill_plot

# now for water use 
demands$date <- bills$date
demands <- demands[,-1]
demands[,c(1:16)] <- demands[,c(1:16)]/748
demands$period[format(demands$date,"%Y")%in%before_year] <- "before"
demands$period[format(demands$date,"%Y")%in%after_year] <- "after"

# now also calculate gallons per person 
individual_demands <- demands

for(j in 1:16){
  individual_demands[,j] <- demands[,j]*748/(30.4*hh_sizes[j])
}

# this is in gallons per person 
demand_plotting <- melt(demands,id.vars = c("period","date","scenario"))
demand_plotting <- demand_plotting[complete.cases(demand_plotting),]

demand_plotting$period <- paste0(demand_plotting$period,"_",demand_plotting$scenario)
keep <- c("before_baseline","after_baseline","after_drought1")
demand_plotting <- demand_plotting[which(demand_plotting$period%in%keep),]

demand_plotting$period <- factor(demand_plotting$period,levels=c("before_baseline","after_baseline","after_drought1"))

demand_plot <- ggplot(demand_plotting,aes(y=value,x=variable,fill=period))+geom_boxplot()+theme_bw()+ylab("Monthly Household Water Use (CCFs)");demand_plot

# and again for individual demands -----------
individual_demand_plotting <- melt(individual_demands,id.vars = c("period","date","scenario"))
individual_demand_plotting <- individual_demand_plotting[complete.cases(individual_demand_plotting),]

individual_demand_plotting$period <- paste0(individual_demand_plotting$period,"_",individual_demand_plotting$scenario)
keep <- c("before_baseline","after_baseline","after_drought1")
individual_demand_plotting <- individual_demand_plotting[which(individual_demand_plotting$period%in%keep),]

individual_demand_plotting$period <- factor(individual_demand_plotting$period,levels=c("before_baseline","after_baseline","after_drought1"))

individual_demand_plot <- ggplot(individual_demand_plotting,aes(y=value,x=variable,fill=period))+geom_boxplot()+theme_bw()+ylab("Monthly Individual Water Use");individual_demand_plot


# re-cast for the three periods so I can calculate a difference ------------------------------------------------
difference_plot_data_perc_income <- dcast(bills_plotting,formula = month+variable ~period,value.var="value")
difference_plot_data_demand <- dcast(plot_data,formula = month+variable ~period,value.var="household_demand")

# now calculate for affordability
difference_plot_data_perc_income$after_baseline <- difference_plot_data_perc_income$after_baseline -difference_plot_data_perc_income$before
difference_plot_data_perc_income$after_drought1 <- difference_plot_data_perc_income$after_drought1 -difference_plot_data_perc_income$before

# now re-melt
difference_plot_data_perc_income <- melt(difference_plot_data_perc_income,id.vars = c("month","variable"))

# get rid of befores 
#difference_plot_data_perc_income[,3] <- as.character(difference_plot_data_perc_income[,3])
colnames(difference_plot_data_perc_income) <- c("month","income","scenario","income_change")
difference_plot_data_perc_income <- difference_plot_data_perc_income[which(difference_plot_data_perc_income$scenario!=("before")),]

difference_plot <- ggplot(difference_plot_data_perc_income,aes(y=income_change,x=income,fill=scenario))+geom_boxplot()+theme_bw()+ylab("% of household income on water");difference_plot

# make a difference plot, but scale by household size ------------------------------------------------------
# make a dataframe to show population for the x-axis histogram

# to calculate individual income increases, divide income_change by the # of people in the house
# for each income level
difference_plot_data_perc_income$size <- 0
difference_plot_data_perc_income$income <- as.character(difference_plot_data_perc_income$income)

# size/income dataframe to look up against 
size_income_df <- data.frame(income=incomes,size=hh_sizes)
size_income_df$income <- paste0("X",size_income_df$income)

for (j in 1:nrow(size_income_df)){
  difference_plot_data_perc_income$size[which(difference_plot_data_perc_income$income%in%size_income_df$income[j])] <- size_income_df$size[j]
}

difference_plot_data_perc_income$change_per_person <- difference_plot_data_perc_income$income_change/difference_plot_data_perc_income$size

# re-order the income 
difference_plot_data_perc_income$income <- factor(difference_plot_data_perc_income$income,levels=paste0("X",incomes))

difference_plot_per_person <- ggplot(difference_plot_data_perc_income,aes(y=change_per_person,x=income,fill=scenario))+
  geom_hline(yintercept = 0,alpha=0.5)+geom_boxplot()+
  theme_bw()+
  ylab("% of household income on water")+
  ggtitle("Change in income per person");difference_plot_per_person


#calculate a cumulative affordabilty  ----------------------------------------
bills$period <- NA
bills$period[which(format(bills$date,"%Y")%in%before_year)] <- "before"
bills$period[which(format(bills$date,"%Y")%in%after_year)] <- "after"

bills <- bills[complete.cases(bills),]
bills$period <- paste0(bills$period,"_",bills$scenario)
keep <- c("before_baseline","after_baseline","after_drought1")
bills <- bills[which(bills$period%in%keep),]

# calculate what the affordability threshold would be for each income class
# then make a dataframe of differences for each 

#thresholds <- 0.20* incomes/12

# now calculate the difference between the threshold and the value for each row of bills
bills_threshold <- bills
#for(i in 1:nrow(bills_threshold)){
#  bills_threshold[i,c(1:16)] <- bills[i,c(1:16)]-thresholds
  # these are positive if the bill is above the threshold (ie price increase)
  # negative if the bill is below the threshold
#}
bills_threshold <- bills_threshold[,-which(colnames(bills_threshold)=="scenario")]
bills_threshold <- melt(bills_threshold,id.vars = c("date","period"))

# aggregate by income and by period 
bills_threshold_aggregated <- aggregate(bills_threshold$value,by=list(bills_threshold$period,bills_threshold$variable),FUN=sum)
bills_threshold_aggregated_table <- dcast(bills_threshold_aggregated,formula = Group.2~Group.1)

# now do the subtraction on the table and re-melt 
bills_threshold_aggregated_table$after_baseline <- bills_threshold_aggregated_table$after_baseline - bills_threshold_aggregated_table$before_baseline
bills_threshold_aggregated_table$after_drought1 <- bills_threshold_aggregated_table$after_drought1 - bills_threshold_aggregated_table$before_baseline
bills_threshold_aggregated_table <- bills_threshold_aggregated_table[,-4]

bills_threshold_aggregated <-  melt(bills_threshold_aggregated_table,id.vars = c("Group.2"))
colnames(bills_threshold_aggregated) <- c("income","period","income_change")
#bills_threshold_aggregated <- bills_threshold_aggregated[-which(bills_threshold_aggregated$period=="before_baseline")]
# plot these? 
threshold_plot <- ggplot(bills_threshold_aggregated,aes(y=income_change,x=income,fill=period))+geom_bar(stat='identity', position=position_dodge())
threshold_plot+coord_flip()+theme_bw()+ylab("Yearly Dollars Per Household Change Relative to Pre-drought") + ggtitle("Yearly Cumulative Increase or Decrease in Cost of Water Bill ")

# now multiply by the number of people in each bin
cumulative_cost_changes <- bills_threshold_aggregated_table
cumulative_cost_changes[,c(2:3)] <- cumulative_cost_changes[,c(2:3)]*hh_counts  
colSums(cumulative_cost_changes[,c(2:3)])


# poverty-threshold ------------------------------------------------------------

MHI = 61000
CPM = 35923

# find the incomes with poverty, belowMHI, above,MHI numbers
belowCPM <- paste0("X",incomes[1:6])
CPMtoMHI <- paste0("X",incomes[7:10])
overMHI <- paste0("X",incomes[10:16])

bills_threshold_aggregated$bin<-"overMHI"
bills_threshold_aggregated$bin[which(bills_threshold_aggregated$income%in%belowCPM)] <- "belowCPM"
bills_threshold_aggregated$bin[which(bills_threshold_aggregated$income%in%CPMtoMHI)] <- "CPMtoMHI"

# now average by bin and plot that
bills_threshold_aggregated_binned <- aggregate(bills_threshold_aggregated$income_change,by = list(bills_threshold_aggregated$period,bills_threshold_aggregated$bin),FUN=mean)

threshold_plot_binned <- ggplot(bills_threshold_aggregated_binned,aes(y=x,x=Group.2,fill=Group.1))+geom_bar(stat='identity', position=position_dodge())
threshold_plot_binned+theme_bw()+ylab("Yearly Dollars Per Household Change Relative to Pre-drought") + ggtitle("Yearly Cumulative Increase or Decrease in Cost of Water Bill ")

# and cal

# make my cumulative income distribution chart 
cumulative_income_chart <- data.frame(incomes=numeric(16))
cumulative_income_chart$incomes <- factor(incomes)
cumulative_income_chart$people <- hh_counts*hh_sizes
cumulative_income_chart$cumulative <- cumulative_income_chart$people

for (j in 2:nrow(cumulative_income_chart)){
  cumulative_income_chart$cumulative[j] <- cumulative_income_chart$cumulative[j-1]+cumulative_income_chart$people[j]
  
}
options(scipen=999999)
plot(cumulative/sum(cumulative_income_chart$people)~incomes,data=cumulative_income_chart,type="l",ylab="Cumulative Fraction of Population At Income Bucket")
cumulative_income_chart$household_size <- hh_sizes
plot(household_size~incomes,data=cumulative_income_chart,ylab="Average Household Size")
