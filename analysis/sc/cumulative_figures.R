# make the cool drought CDFs
load("~/Documents/__college/reseach_stuff/hr2w_droughts/analysis/processed_and_binned_bill_data_plot_responses.Rdata")
bill1 <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/review_responses/0_hh_bills.csv")
outputs <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/review_responses/0_outputs.csv")
params <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/review_responses/parameter_list.csv")

file.list <- list.files(path="~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/review_responses/",pattern = "*bill*",full.names = T)
file.names <- list.files(path="~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/review_responses/",pattern = "*bill*")

output.list <- list.files(path="~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/review_responses/",pattern = "*output*",full.names = T)

# filter down to just the ones I want 


# pull the number out of the file names and attach the params
library('stringr')
numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 

file_numbers <- numextract(file.names)
important_params <- data.frame(fileNumb=file_numbers)
important_params$filename <- file.names
# now attach the data from params to file_numbers
important_params <- merge(x=important_params,y=params,by.x="fileNumb",by.y="X")
rows_I_want <- which(important_params$income_elasticity==0.15&important_params$price_elasticity==0.35&important_params$cpe_squared==0)
important_params <- important_params[rows_I_want,]

plot_df <- data.frame(date = as.Date(outputs$Date))
plot_df2 <- data.frame(date=as.Date(outputs$Date))
curtail <- data.frame(date = as.Date(outputs$Date))

# i think I have to also shrink file.list the same
file.list <- file.list[rows_I_want]
file.names <- file.names[rows_I_want]
output.list <- output.list[rows_I_want]
for(i in 1:length(file.list)){
  
  #curtail
  this_outputs <- read.csv(output.list[i])  
  if(!all(is.na(this_outputs$market_buy))){
    # this is saying all of them are not NAs so we have some values
    curtail[i+1] <- this_outputs$market_buy
    colnames(curtail)[i+1] <- file.names[i]
    
  }else{
    
    curtail[i+1] <- this_outputs$residentialDemand*this_outputs$conserveStatus  
    colnames(curtail)[i+1] <- file.names[i]
    
  }
  
  this_bills <- read.csv(file.list[i])
  plot_df[i+1] <- rowMeans(this_bills[,c(2,3,4)])
  plot_df2[i+1] <- rowMeans(this_bills[,c(15,16,17)])
  colnames(plot_df)[i+1] <- file.names[i]
  colnames(plot_df2)[i+1] <- file.names[i]
  #print(file.list[i])
  #print(file.names[i])
}

library(reshape2)
plot_df_melt <- melt(plot_df,id.vars = "date")
plot_df_melt$class = "lowIncome"
plot_df_melt2 <- melt(plot_df2,id.vars="date")
plot_df_melt2$class = "highIncome"

curtail_melt <- melt(curtail,id.vars = "date")

library(ggplot2)

# trying some plot stuff
pp <- plot_df
pp2 <- plot_df2

date_range <- seq(from = as.Date("2014-01-01"),to = as.Date("2015-12-31"),by="month")

# IDEA: subtract out baseline, non-drought years 
baseline_mat <- pp[c(1:12),c(2:ncol(pp))]
baseline_mat[,c(2:ncol(baseline_mat))] <- baseline_mat[,1]

baseline_mat2 <- pp2[c(1:12),c(2:ncol(pp2))]
baseline_mat2[,c(2:ncol(baseline_mat2))] <- baseline_mat2[,1]

pp <- pp[which(pp$date%in%date_range),]
pp2 <- pp2[which(pp2$date%in%date_range),]

# subtract it out
pp[,c(2:ncol(pp))] <- pp[,c(2:ncol(pp))]-rbind(baseline_mat,baseline_mat)

pp2[,c(2:ncol(pp2))] <- pp2[,c(2:ncol(pp2))]-rbind(baseline_mat2,baseline_mat2)

pp[,c(2:ncol(pp))] <- sapply(pp[,c(2:ncol(pp))],FUN=cumsum)
pp2[,c(2:ncol(pp2))] <- sapply(pp2[,c(2:ncol(pp2))],FUN=cumsum)

pp_melt <- melt(pp,id.vars="date")
pp_melt_2 <- melt(pp2,id.vars="date")

pp_melt$class = "low"
pp_melt_2$class = "high"

#pp_melt <- rbind(pp_melt,pp_melt2)
library('ggthemes')
library('ggsci')
# now add all the params to pp_melt

pp_melt <- merge(x=important_params,y=pp_melt,by.x="filename",by.y="variable")
pp_melt_2 <- merge(x=important_params,y=pp_melt_2,by.x="filename",by.y="variable")

pp_melt$drought_characteristic<- factor(pp_melt$drought_characteristic, levels=c("baseline.csv","long.csv","intense.csv","long_intense.csv"))
pp_melt_2$drought_characteristic<- factor(pp_melt_2$drought_characteristic, levels=c("baseline.csv","long.csv","intense.csv","long_intense.csv"))
  

p1 <- ggplot(pp_melt)+geom_line(aes(x=date,y=value,group=fileNumb,linetype=mitigation_decision,color=build_decision))+
  facet_grid(cols=vars(drought_characteristic),rows=vars(class),scales="free")
p1 <- p1 + theme_bw()+ scale_color_colorblind()+ylab("Cost increase ($/month) \n Compared to Non-drought Year")
p1 <- p1+ geom_hline(yintercept = 0,color="gray",alpha=0.95)

p1_2 <- ggplot(pp_melt_2)+geom_line(aes(x=date,y=value,group=fileNumb,linetype=mitigation_decision,color=build_decision))+
  facet_grid(cols=vars(drought_characteristic),rows=vars(class),scales="free")
p1_2 <- p1_2 + theme_bw()+ scale_color_colorblind()+ylab("Cost increase ($/month) \n Compared to Non-drought Year")
p1_2 <- p1_2+ geom_hline(yintercept = 0,color="gray",alpha=0.95)



curtail_melt_full <- merge(x=important_params,y=curtail_melt,by.x="filename",by.y = "variable")
curtail_melt_full$drought_characteristic<- factor(curtail_melt_full$drought_characteristic, levels=c("baseline.csv","long.csv","intense.csv","long_intense.csv"))

curtail_melt_full <- curtail_melt_full[which(curtail_melt_full$date%in%date_range),]


p_2 <- ggplot(curtail_melt_full)+geom_line(aes(x=date,y=value,group=fileNumb,linetype=mitigation_decision,color=build_decision))+
  facet_grid(cols=vars(drought_characteristic)) + theme_bw()+ scale_color_colorblind()+ylab("Mitigation Volume \n (MG/month)")+xlab("")

# now make the water availability figure using files # 0,12,18,6 
bl_drought <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/plot11/0_outputs.csv")
long_drought <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/plot11/12_outputs.csv")
int_drought <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/plot11/18_outputs.csv")
long_int_drought <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/plot11/6_outputs.csv")

# fix the long-intense 


library(TTR)
period = 6
water_availability <- data.frame(date=bl_drought$Date)
water_availability$baseline <- SMA(bl_drought$northCoast+bl_drought$taitStreet+bl_drought$newellInflow,n=period)
water_availability$long <- SMA(long_drought$northCoast+long_drought$taitStreet+long_drought$newellInflow,n=period)
water_availability$int <- SMA(int_drought$northCoast+int_drought$taitStreet+int_drought$newellInflow,n=period)
water_availability$long_int <- SMA(long_int_drought$northCoast+long_int_drought$taitStreet+long_int_drought$newellInflow,n=period)

wa_melt <- melt(water_availability,id.vars="date")
wa_melt$date <- as.Date(wa_melt$date)
#
p0 <- ggplot(wa_melt[which(wa_melt$date%in%date_range),]) + geom_line(aes(x=date,y=value),se = FALSE)+facet_grid(cols=vars(variable))+ theme_bw()+ scale_color_colorblind()
#p0

# stack the three on top of eachother
library(cowplot)
p1_leg <- get_legend(p1)

# make changes to p0
p0 <- p0+  theme(legend.position = "none",
                 axis.title.x=element_blank(),
                 axis.text.x=element_blank())+ ylab("Available Water \n (MG/Month")
  

# now for the mitigaiton plot
p_2 <- p_2 + theme(legend.position = "none",
                 axis.title.x=element_blank(),
                 axis.text.x=element_blank())+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )

p_2_leg <- get_legend(p_2)

# now for the bill icnreases for low

p1 <- p1+  theme(
  strip.background = element_blank(),
  strip.text.x = element_blank()
) + theme(legend.position = "none",
          axis.title.x=element_blank(),
          axis.text.x=element_blank())

# now for all the final plot stuff

p1_2 <- p1_2 + theme(legend.position="none") + 
  theme(
    strip.background = element_blank(),
    
    strip.text.x = element_blank()
  )+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p_x <- get_x_axis(p1_2)

p1_2 <- p1_2 +  theme(
  strip.background = element_blank(),
  strip.text.x = element_blank() ) + 
  theme(legend.position = "none",
          axis.title.x=element_blank(),
          axis.text.x=element_blank())


# now the bill increases for high

g1 <- plot_grid(p0,p_2,p1,p1_2,nrow=4,align = 'v', axis = 'lr',rel_heights = c(.25,.25,.25,.25),greedy=FALSE)
gx <- plot_grid(p_x,p_x,p_x,p_x,nrow=1,align='h')

g2 <- plot_grid(g1,gx,nrow=2,align='v',axis='lr',rel_heights=c(.9,.1))
g3 <- plot_grid(g2,p1_leg,nrow=2,align='v',axis='lr',rel_widths = c(1,0.125))

  
  #plot_grid(g1,p_x,p1_leg,nrow=3,align='v',axis='lr',rel_widths = c(1,0.25))
g3
# 
ggsave(g3,filename = "~/Documents/__college/reseach_stuff/hr2w_droughts/analysis/sc/figure_3_final_2_reviewed.pdf",width=6,height=9,unit="in",scale=1.5)
