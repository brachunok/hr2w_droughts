#proposal figures
library(RSQLite)
library(ggstatsplot)
library(gghighlight)
library(ggpattern)


install.packages("matrixStats")
library(matrixStats)
# for each account let's look at the number of entries
pp <- read.csv("/Volumes/rachunok/research/hr2w_droughts_data/clean_dbs/merged_clean_safe_bills.csv")
fwb_account_counts <- table(pp$account)
fwb_account_counts <- data.frame(fwb_account_counts)
colnames(fwb_account_counts) <- c("act","entries")

# for each entry, pull the value which matches from the fwb_account_counts table
fwb <- merge(pp,fwb_account_counts,by.x="account",by.y="act")

# remove all bill lengths of 0
fwb<- fwb[which(fwb$bill_length!=0),]

# now sample only to those with a bunch of entries 
fwb <- fwb[which(fwb$entries>145),]

# normalize by bill length
fwb$normtotwuse <- fwb$normtotwuse/fwb$bill_length

# also remove the way-high outliers 
#boxplot(fwb$normtotwuse)
upper_lim <- quantile(fwb$normtotwuse,probs = 0.999)
fwb <- fwb[which(fwb$normtotwuse<=upper_lim),]

# now just plot
#boxplot(fwb$normtotwuse)
#plot(density(fwb$normtotwuse))

fwb$edate <- as.Date(fwb$edate,format="%m/%d/%Y")

# look at single family vs multi-family water use 
fwb$restype <- as.factor(fwb$restype)
#plot(fwb$normtotwuse~fwb$restype)

# all makes sense. SFR uses way more than MFR.
# convert to month

#plot(fwb$normtotwuse*30.4~fwb$restype,xlab="residence type",ylab = "CCF/month",main="CCF by household type")
#abline(h = 5)

# see how it changes by MHI 
#plot(fwb$normtotwuse*30.4~fwb$MHI)
# make a high/low/middle income column
fwb$income_bin <- "middle"
fwb$income_bin[which(fwb$MHI<68000)] <- "low"
fwb$income_bin[which(fwb$MHI>68000*2)] <- "high"


fwb$drought_status <- NA
fwb$drought_status[which(format(fwb$edate,"%Y")=="2013")] <- "before"
fwb$drought_status[which(format(fwb$edate,"%Y")=="2015")] <- "during"
fwb$drought_status[which(format(fwb$edate,"%Y")=="2017")] <- "after"

# now just get rid of the rest 
fwb_selected <-fwb[!is.na(fwb$drought_status),]
# boxplot(fwb_selected$normtotwuse*30.4~fwb_selected$e_bin)

# look at each account's difference betweent the three periods
colnames <- c("account","edate","normtotwuse","drought_status","emon")
pp <- reshape(fwb_selected[,colnames(fwb)%in%colnames],timevar="drought_status",idvar=c("account","emon"),direction = "wide")

pp$drought_conserve <- (pp$normtotwuse.before-pp$normtotwuse.during)/pp$normtotwuse.before*100
pp$drought_rebound <- (pp$normtotwuse.during-pp$normtotwuse.after)/pp$normtotwuse.during*100
pp$pre_to_post <- (pp$normtotwuse.before-pp$normtotwuse.after)/pp$normtotwuse.before*100

# aggregate by account
rebound_differences_aggregated <- aggregate(pp,by=list(pp$account),FUN=median)

# merge FWB data into rebound diff
cons_reb_full <- merge(x=rebound_differences_aggregated,y=fwb[,c(1,5:90,95,96)],by.x="Group.1",by.y="account",all.x = F,all.y = F)
library(dplyr)
cons_reb_full <- distinct(cons_reb_full)

#boxplot(drought_conserve~restype,data=cons_reb_full)
#plot(drought_conserve~MHI,data=cons_reb_full)

# analysis of %age of income ---------------------------------------------------
fwb$norm_bill <- fwb$billtot/fwb$num_units
# (1) aggregate bill totals by drought period and account number
income_analysis <- aggregate(fwb[,c("account","MHI","drought_status","norm_bill")],by=list(fwb$account,fwb$drought_status),FUN = "median")

# (2) calcualte bill totals as %age of MHI for the tract 
income_analysis$poisow <- income_analysis$norm_bill/(income_analysis$MHI/12)*100
income_analysis <- income_analysis[!is.na(income_analysis$poisow),]

income_analysis_wide <- reshape(income_analysis,idvar = "Group.1",direction = "wide",timevar = "Group.2")
income_analysis_wide$fab_change <- (income_analysis_wide$poisow.before-income_analysis_wide$poisow.during)*8

income_analysis_wide$fab_during <- income_analysis_wide$poisow.before - income_analysis_wide$fab_change
income_analysis_plot <- melt(income_analysis_wide[,c(1,11,18)])

fill_data <- income_analysis_wide[,c("Group.1","poisow.before","fab_during")]
bef_ecdf <- ecdf(fill_data$poisow.before)
aft_ecdf <- ecdf(fill_data$fab_during)

ribbon_plot <- data.frame(x=seq(from=0,to=7,length.out=10000))
ribbon_plot$bef <- bef_ecdf(ribbon_plot$x)
ribbon_plot$aft <- aft_ecdf(ribbon_plot$x)
ribbon_plot$lower <- apply(ribbon_plot[,c(2,3)],1,min)
ribbon_plot$upper <- apply(ribbon_plot[,c(2,3)],1,max)
ribbon_plot$lower[which(ribbon_plot$x<=3)] <- NA
ribbon_plot$upper[which(ribbon_plot$x<=3)] <- NA



plot_cdf <- ggplot(data=ribbon_plot)+geom_line(aes(x=x,y=bef))+geom_line(aes(x=x,y=aft),col="red")+
  geom_ribbon(data=ribbon_plot,aes(x=x,ymin=lower,ymax=upper),fill=alpha("#808080",.4)) + theme_bw()+
  labs(x="Percent of Income Spent on Water",y="Fraction of Households",linetype="Drought Period",color="Period")+
  theme(legend.position = c(0.8, 0.25))+theme(legend.background = element_rect(fill="white")) + geom_vline(xintercept = 3,lty="longdash")
plot_cdf

# now for the second one which is water use 
use_analysis <- rebound_differences_aggregated
use_analysis$fab_during <- use_analysis$normtotwuse.before - (2*(use_analysis$normtotwuse.before-use_analysis$normtotwuse.during))
use_analysis$fab_during[which(use_analysis$fab_during<0)] <- 0

head(use_analysis)
use_analysis$normtotwuse.before <- use_analysis$normtotwuse.before*31
use_analysis$fab_during <- use_analysis$fab_during*31

bef_use_ecdf <- ecdf(use_analysis$normtotwuse.before)
aft_use_ecdf <- ecdf(use_analysis$fab_during)

ribbon_use <- data.frame(x=seq(from=0,to=20,length.out=10000))
ribbon_use$bef <- bef_use_ecdf(ribbon_use$x)                         
ribbon_use$aft <- aft_use_ecdf(ribbon_use$x)                         

ribbon_use$lower <- apply(ribbon_use[,c(2,3)],1,min)        
ribbon_use$upper <- apply(ribbon_use[,c(2,3)],1,max)                             

ribbon_use$lower[which(ribbon_use$x>=5)] <- NA
ribbon_use$upper[which(ribbon_use$x>=5)] <- NA


plot_cdf_use <- ggplot(ribbon_use) + geom_line(aes(x=x,y=bef))+geom_line(aes(x=x,y=aft),col="red")+
  geom_ribbon(data=ribbon_use,aes(x=x,ymin=lower,ymax=upper),fill=alpha("#808080",.4))+theme_bw()+
  labs(x="Water Use (CCF)",y="Fraction of Households",linetype="Drought Period",color="Period")+
  theme(legend.position = c(0.8, 0.25))+theme(legend.background = element_rect(fill="white")) + geom_vline(xintercept = 5,lty="longdash")
               
library(cowplot)                                                                                   
output_plot <- plot_grid(plot_cdf,plot_cdf_use,nrow=1)
ggsave(output_plot,filename = "~/Documents/__college/reseach_stuff/hr2w_droughts/analysis/sc/billing_analysis/proposal_figure.png",width=7,height=4,unit="in")
