# try making my affordability/access plots for each income class in sacramento
# before and after the drought and fake reservoir 

library('ggplot2')
library('reshape2')
library('gganimate')

# read in my system overview file
bills <- read.csv("~/Documents/__college/reseach_stuff/water-equity/outputs/hh_bills.csv")
demands <- read.csv("~/Documents/__college/reseach_stuff/water-equity/outputs/hh_demand.csv")
outputs <- read.csv("~/Documents/__college/reseach_stuff/water-equity/outputs/outputs.csv")
# validation for initial sacramento data against benecia, glendale, elsinore data

bills$date = outputs$Date
bills$type = "bill"
demands$date = outputs$Date
demands$type="demand"
hh = rbind(bills,demands)
# figure otu when we built the res and draw a vertical line
vl = outputs$Date[which(outputs$trigger=="YES")]

# melt for easier plotting
hh$date <- as.Date(as.character(hh$date))
hh_melted <- melt(hh[,-1],id.vars = c("date","type"))
hh_melted$date <- as.Date(as.character(hh_melted$date))
p <- ggplot(hh_melted,aes(x=date,y=value,color=variable))+ 
  geom_line()+
  facet_grid(rows=vars(type),scales = "free_y")+
  scale_color_brewer(palette="RdYlBu")+theme_bw()+
  geom_vline(xintercept = as.Date(vl),linetype="dashed",color = "red", size=1)
#ggsave(p,filename = "~/Documents/__college/reseach_stuff/water-equity/analysis/plots/sacramento/drought_bills_demands.pdf",width=8,height=6,units = "in")

# look before/after the conservation
# in this stylized test case look at 2020 the year vs 2028 the year 
data <-  hh[which(hh$date<as.Date("2021-01-01")|hh$date>="2028-12-31"),]
data$sample <- "before"
data$sample[which(data$date>="2023-12-31")] <- "after"
bill_data <- data[which(data$type=="bill"),]
demand_data  <- data[which(data$type!="bill"),]


# now divide by household incomes
#incomes <- c(12500,17250,30000,42500,63500,87500,125000,175000)
#incomes <- incomes/12
# for (j in c(2:9)){
#   bill_data[,j] <- bill_data[,j]/incomes[j]
#   
# }

# similarly, convert the demand data to CCF 
demand_data[,c(2:9)] <- demand_data[,c(2:9)] /748

# make a function which produces my rate as a function of HCF
monthly_bill_sac <- function(hcf){
  flat_rate = 29.52
  bill = flat_rate + hcf*1.2055
  return(bill)
}

sac_inverse1 <- function(dollar){
  hcf = (dollar-29.52)/1.2055
  return(hcf)
}

sac_inverse2 <- function(dollar){
  hcf = (dollar-34.38)/1.2055
  return(hcf)
}

# now make my baseline affordability/access plot for each income class
# melt each one then combine them 
demand_data_melted <- demand_data[,-c(1,11)]
demand_data_melted <- melt(demand_data_melted,id.vars = c("date","sample") ,variable.name = "class",value.name = "demand")

#now add the price to it 
bill_data_melted <- bill_data[,-c(1,11)]
bill_data_melted <- melt(bill_data_melted,id.vars = c("date","sample"),variable.name="class",value.name="bill")

# merge 
plot_data <- merge(bill_data_melted,demand_data_melted,by=c("date","sample","class"))
#plot_data$date_index <- c(1:nrow(plot_data))
plot_data$income <- as.character(plot_data$class)
plot_data$income <- as.numeric(substr(plot_data$income,start =2,stop=nchar(plot_data$income)))
plot_data$bill_frac_income <- plot_data$bill/(plot_data$income/12)
plot_data$violates_income <- FALSE
plot_data$violates_income[which(plot_data$bill_frac_income>=0.015)] <- TRUE
plot_data$monthly_income <- plot_data$income/12

p1 <- ggplot(data=plot_data,aes(x=demand,y=bill_frac_income*100,color=class))+
  geom_point(aes(shape=sample)) +
  scale_color_brewer(palette="RdYlBu")+
  theme_bw() + ylab("% of income on water")+xlab("Monthly Demand (CCF)")

p1

# try a basic side-by-side-boxplot thing-o
p2 <- ggplot(plot_data,aes(y=bill_frac_income,fill=sample))+geom_boxplot()+
 facet_grid(cols=vars(class))+theme_bw()+scale_fill_brewer(palette="RdYlBu")
p2
# shows the distinct changes
# 

p3 <- ggplot(plot_data,aes(x=monthly_income,y=demand,color=sample,shape=violates_income))+
  geom_point()+ theme_bw()+
  ylim(0,100)+
  geom_hline(yintercept = 6)+
  stat_function(fun=function(monthly_income){sac_inverse1(monthly_income*0.02)})+
  stat_function(fun=function(monthly_income){sac_inverse2(monthly_income*0.02)})
p3  

# usage plot by income for validation

p4 <- ggplot(plot_data,aes(y=demand,fill=sample))+geom_boxplot()+
  facet_grid(cols=vars(class))+theme_bw()+scale_fill_brewer(palette="RdYlBu")+
  scale_y_continuous(minor_breaks=c(1:50))+ylim(c(0,20))
p4
