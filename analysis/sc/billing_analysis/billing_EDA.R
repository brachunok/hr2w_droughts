# bill EDA
# Read full database and filter out important rows

library(RSQLite)
library(ggstatsplot)
library(gghighlight)


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

# look at single vs multi within each 
#table(fwb$income_bin,fwb$restype)
#boxplot(fwb$normtotwuse*30.5~fwb$income_bin,xlab="income bin",ylab = "CCF/Month",main="Water Use by Income Class")
#abline(h=5)

# break it up by time 
# library(ggplot2)
# library(stringr)
# # make the binning operation for the boxplot
# fwb$e_bin <- paste0(fwb$eyr,str_pad(fwb$emon,width=2,side="left",pad="0"))
# fwb$e_bin <- as.factor(fwb$e_bin)
# # gotta do a sample of 100 accounts
# set.seed(1)
# sample_accts <- sample(unique(fwb$account),size = 100,replace = F)
# fwb_sample <- fwb[which(fwb$account%in%sample_accts),]

# this figure is useless
# p1 <- ggplot(fwb_sample,aes(x=edate,y=normtotwuse,group=account))+geom_line(alpha=0.5)
# p1
# #ggsave(filename = "~/Documents/__college/reseach_stuff/hr2w_droughts/analysis/sc/billing_analysis/use_over_time.pdf",width=11,height=8,unit="in")
# 
# # bin by e-bin and make a lil boxplot situation
# 
# boxplot(fwb$normtotwuse*30.4~fwb$e_bin)
# abline(h = 5)

# now what I want to do: add a column for drought v. non-drought periods
# then calculate the 2013(?) year vs the 2014(?) year vs the 2017(?) year

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

# pairs
# pairs(rebound_differences_aggregated[,c(5,7,9,10,11,12)])
summary(rebound_differences_aggregated)

# what is the pre-drought water use of those who conserved vs. those who did not 

# summary(rebound_differences_aggregated[rebound_differences_aggregated$drought_conserve<0,])

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

# couple of these are mindblowing massive. assuming mis-labeled commercial account or 


# (3) calculate the percentages we are after 
income_levels <- c(0:800)*.01
MHI=90000
plot_data <- data.frame(poisow = income_levels)
plot_data$cumulative_perc_drought <- NA
plot_data$cumulative_perc_nondrought <- NA

for(i in 1:nrow(plot_data)){
  this_level <- income_levels[i]
  this_true_during <- which(income_analysis$poisow>=this_level&income_analysis$Group.2=="during")
  plot_data$cumulative_perc_drought[i] <- length(this_true_during)/length(which(income_analysis$Group.2=="during"))*100
  
  this_true_before <- which(income_analysis$poisow>=this_level&income_analysis$Group.2=="before")
  plot_data$cumulative_perc_nondrought[i] <- length(this_true_before)/length(which(income_analysis$Group.2=="before"))*100
}

plot(cumulative_perc_nondrought~poisow,data=plot_data,type="l",xlab="Percentage of Income Spent on Water",ylab="Cumulative %age of Population Spending x% of income on water")
lines(cumulative_perc_drought~poisow,data=plot_data,type="l",col="red")
abline(h=0,col="gray")
abline(v=0,col="gray")

# okay now let's do it for low-income 
plot_data_LI <- data.frame(poisow = income_levels)
plot_data_LI$cumulative_perc_drought <- NA
plot_data_LI$cumulative_perc_nondrought <- NA

li_income_analysis <- income_analysis[which(income_analysis$MHI<MHI),]

for(i in 1:nrow(plot_data_LI)){
  this_level <- income_levels[i]
  this_true_during <- which(li_income_analysis$poisow>=this_level&li_income_analysis$Group.2=="during")
  plot_data_LI$cumulative_perc_drought[i] <- length(this_true_during)/length(which(li_income_analysis$Group.2=="during"))*100
  
  this_true_before <- which(li_income_analysis$poisow>=this_level&li_income_analysis$Group.2=="before")
  plot_data_LI$cumulative_perc_nondrought[i] <- length(this_true_before)/length(which(li_income_analysis$Group.2=="before"))*100
}

plot(cumulative_perc_nondrought~poisow,data=plot_data_LI,type="l",xlab="Percentage of Income Spent on Water",ylab="Cumulative %age of Population Spending x% of income on water",
     main="Low-income households")
lines(cumulative_perc_drought~poisow,data=plot_data_LI,type="l",col="red")
abline(h=0,col="gray")
abline(v=0,col="gray")

# okay now let's do it for high-income 
plot_data_HI <- data.frame(poisow = income_levels)
plot_data_HI$cumulative_perc_drought <- NA
plot_data_HI$cumulative_perc_nondrought <- NA

HI_income_analysis <- income_analysis[which(income_analysis$MHI>MHI*2),]

for(i in 1:nrow(plot_data_HI)){
  this_level <- income_levels[i]
  this_true_during <- which(HI_income_analysis$poisow>=this_level&HI_income_analysis$Group.2=="during")
  plot_data_HI$cumulative_perc_drought[i] <- length(this_true_during)/length(which(HI_income_analysis$Group.2=="during"))*100
  
  this_true_before <- which(HI_income_analysis$poisow>=this_level&HI_income_analysis$Group.2=="before")
  plot_data_HI$cumulative_perc_nondrought[i] <- length(this_true_before)/length(which(HI_income_analysis$Group.2=="before"))*100
}

plot(cumulative_perc_nondrought~poisow,data=plot_data_HI,type="l",xlab="Percentage of Income Spent on Water",ylab="Cumulative %age of Population Spending x% of income on water",
     main="Y% of the Population Spends X% on Income",col="red")
lines(cumulative_perc_drought~poisow,data=plot_data_HI,type="l",lty="dashed",col="red")#="red")
lines(cumulative_perc_nondrought~poisow,data=plot_data_LI,type="l")
lines(cumulative_perc_drought~poisow,data=plot_data_LI,type="l",lty="dashed")
legend(4,90,legend=c("Low-Income, Pre-Drought","Low-Income, During-Drought",
                      "High-Income, Pre-Drought","High-Income, During-Drought"),
       col=c("black","black","red","red"),
       lty=c(1,2,1,2))

abline(h=0,col="gray")
abline(v=0,col="gray")




# make the figure which-- in x -- shows change in %age of income spent on bills pre vs during
# and in y we show change in water use pre vs. during. highlight dots based on high vs. low income 
# get this data by subtracting the befores and afters in income analysis and then merging it with
# cons reb full

income_analysis_wide <- reshape(income_analysis,idvar = "Group.1",direction = "wide",timevar = "Group.2")
income_analysis_wide$poisow_delta_before_to_during <- (income_analysis_wide$poisow.before-income_analysis_wide$poisow.during)
plot_2_data <- merge(x=income_analysis_wide[,c("Group.1","poisow_delta_before_to_during")],y=cons_reb_full,by.x="Group.1",by.y="Group.1")

# add a column for income 
plot_2_data$income_bin <- "middle"
plot_2_data$income_bin[which(plot_2_data$MHI<MHI)] <- "low"
plot_2_data$income_bin[which(plot_2_data$MHI>MHI*2)] <- "high"

plot_2_data$income_bin <- factor(plot_2_data$income_bin,levels=c("low","middle","high"))
# now make the figure 
library(ggplot2)
p2 <- ggplot(plot_2_data)+geom_point(aes(x=poisow_delta_before_to_during,y=drought_conserve,color=income_bin),alpha=0.3)+theme_bw()+
  geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+scale_discrete_manual(aesthetics = "color",values=c("black","black","black"))+gghighlight(use_direct_label = FALSE)+facet_grid(~income_bin)+ylim(c(-100,100))+
  xlim(c(-1,1))+theme(legend.position = "none")+labs(x="Absolute Reduction in Percentage of Income Spent on Water (Before to During Drought)",
                                                         y="%Reduction Water Use, (Before to During Drought) ")

p2

# make a version of the above, but just like a normal CDF with some changes  --------------------------
library(ggplot2)
library(ggsci)

# (1) shrink FWB down to just what I want
fwb_rlb <- fwb[,c("account","MHI","num_units","drought_status","norm_bill","totwuse")]

# (2) re-calculate water bills with 5% limiting behavior 
# do this by making a rate function 
base_rate <- function(x){
  # x is a row of fwb
  rates <- x[109:113]
  use <- x[114:118]
  volume <- rates*use
  fixed <- x[c(129:137)]#,170:177)]
  sum(volume[!is.na(volume)])+ sum(fixed[!is.na(fixed)])
  return(sum(volume[!is.na(volume)])+ sum(fixed[!is.na(fixed)]))
}

limited_rate <- function(x,lim){
  # x is a row of fwb
  rates <- as.numeric(x[109:113])
  use <- as.numeric(x[114:118])
  
  # tot_use
  tot_use <- sum(use[!is.na(use)])
  reduced_use <- tot_use*lim
  reduction_amount <- tot_use -reduced_use
  
  # now remove it from the highest tier, and if there's any left go to the next one 
  tiers <- which(!is.na(rates))
  new_use <- use
  for (tier in order(tiers,decreasing = T)){
    if (reduction_amount>use[tier]){
      new_use[tier] <- 0
      reduction_amount <- reduction_amount-use[tier]
    }else{
      # this happens if the 
      new_use[tier] <- use[tier]- reduction_amount
    }

  }
    
  volume <- rates*new_use
  fixed <- as.numeric(x[c(129:137)])#,170:177)]
  sum(volume[!is.na(volume)])+ sum(fixed[!is.na(fixed)])
  return(sum(volume[!is.na(volume)])+ sum(fixed[!is.na(fixed)]))
}
# get an FWB for the non'-middle income classes
fwb_no_mid <- fwb[which(fwb$income_bin!="middle"),]

for(r in 1:nrow(fwb_no_mid)){
#for(r in c(1:10)){
 fwb_no_mid$calc_0[r] <- limited_rate(fwb_no_mid[r,],1)
  fwb_no_mid$calc_1[r] <- limited_rate(fwb_no_mid[r,],.9)
  fwb_no_mid$calc_2[r] <- limited_rate(fwb_no_mid[r,],.8)
  fwb_no_mid$calc_3[r] <- limited_rate(fwb_no_mid[r,],.7)
  if(round(r/500)==(r/500)){
    print(r)
  }
}


# (3) then aggregate including the new cost 

income_analysis <- aggregate(fwb_no_mid[,c("account","MHI","drought_status","norm_bill","calc_0","calc_1","calc_2","calc_3")],by=list(fwb_no_mid$account,fwb_no_mid$drought_status),FUN = "median")

# (2) calcualte bill totals as %age of MHI for the tract 
income_analysis$poisow <- income_analysis$norm_bill/(income_analysis$MHI/12)*100
income_analysis$calc_0 <- income_analysis$calc_0/(income_analysis$MHI/12)*100
income_analysis$calc_1 <- income_analysis$calc_1/(income_analysis$MHI/12)*100
income_analysis$calc_2 <- income_analysis$calc_2/(income_analysis$MHI/12)*100
income_analysis$calc_3 <- income_analysis$calc_3/(income_analysis$MHI/12)*100


income_analysis <- income_analysis[!is.na(income_analysis$poisow),]
income_analysis$income_bin <- "Middle-Income"
income_analysis$income_bin[which(income_analysis$MHI<MHI)] <- "Low-Income"
income_analysis$income_bin[which(income_analysis$MHI>MHI*2)] <- "High-Income"

#TODO probably make this just one and all before, then do some sort of calculation to re-calculate the bill
income_analysis_plot <- income_analysis[which(income_analysis$Group.2!="after"),,drop=TRUE]
income_analysis_plot <- income_analysis_plot[which(income_analysis_plot$income_bin!="Middle-Income"),,drop=TRUE]

income_analysis_plot$Group.2[which(income_analysis_plot$Group.2=="before")] = "Before"
income_analysis_plot$Group.2[which(income_analysis_plot$Group.2=="during")] = "During"

#save(income_analysis_plot,file = "~/Documents/__college/reseach_stuff/hr2w_droughts/analysis/sc/billing_analysis/intermediate_save.Rdata")
load("~/Documents/__college/reseach_stuff/hr2w_droughts/analysis/sc/billing_analysis/intermediate_save.Rdata")
# melt this data to make colors based on the hypothesized changes
income_analysis_plot_melt <- melt(income_analysis_plot,id.vars =c("Group.1","Group.2","income_bin"),measure.vars = c("calc_0","calc_1","calc_2","calc_3"))

income_analysis_plot_melt <- income_analysis_plot_melt[which(income_analysis_plot_melt$income_bin=="Low-Income"&income_analysis_plot_melt$Group.2=="Before"),]
plot_cdf <- ggplot(income_analysis_plot_melt,aes(x=(value),color=Group.2,linetype=income_bin)) + stat_ecdf()+ xlim(c(0,7))
plot_cdf <- plot_cdf + scale_color_npg() + theme_bw()+ labs(x="Percent of Income Spent on Water",y="Fraction of Households",linetype="Drought Period",color="Income Class")+
   theme(legend.position = c(0.8, 0.25))+theme(legend.background = element_rect(fill="white"))


plot_cdf

#ggsave(plot_cdf,filename = "~/Documents/__college/reseach_stuff/hr2w_droughts/documents/health_people_fig.png",width=4,height=4,scale=1.2)


# now the final plot of the one like we have above 
library(ggplot2)

income_analysis_wide <- reshape(income_analysis,idvar = "Group.1",direction = "wide",timevar = "Group.2")
income_analysis_wide$poisow_delta_before_to_during <- (income_analysis_wide$poisow.during-income_analysis_wide$poisow.before)/income_analysis_wide$poisow.before * 100
plot_2_data <- merge(x=income_analysis_wide[,c("Group.1","poisow_delta_before_to_during")],y=cons_reb_full,by.x="Group.1",by.y="Group.1")

# add a column for income 
plot_2_data$income_bin <- "middle"
plot_2_data$income_bin[which(plot_2_data$MHI<MHI)] <- "low"
plot_2_data$income_bin[which(plot_2_data$MHI>MHI*2)] <- "high"

plot_2_data$income_bin <- factor(plot_2_data$income_bin,levels=c("low","middle","high"))

plot_2_data <- plot_2_data[which(plot_2_data$poisow_delta_before_to_during!=-100&plot_2_data$drought_conserve!=100),]
p2 <- ggplot(plot_2_data)+geom_jitter(aes(x=poisow_delta_before_to_during,y=-drought_conserve,color=income_bin),alpha=0.3)+theme_bw()+
  geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+scale_discrete_manual(aesthetics = "color",values=c("black","black","black"))+ylim(c(-100,100))+
  xlim(c(-100,100))+theme(legend.position = "none")+labs(x="Increase in the Percentage \n of Income Spent on Water ",y="Increase in Water Use ")+
  scale_y_continuous(labels = function(x) paste0(x, "%"),limits = c(-100,100))+scale_x_continuous(labels = function(x) paste0(x, "%"),limits = c(-100,100))

p2
ggsave(p2,filename = "~/Documents/__college/reseach_stuff/hr2w_droughts/documents/health_people_fig2.png",width=3,height=3,scale=1.1)
