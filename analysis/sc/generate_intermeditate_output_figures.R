# general 'runner' to turn scenario files into output plots

library(reshape2)
library(reshape2)
library(tidyr)
library(readr)
library('ggplot2')
library("ggsci")
library('cowplot')
# 
#
#
#
# Figure 1
#
#   -----------
#
# $ -----------
#   -----------
# _____________________________________
#   predrought        drought
# 
# LOW INCOME                                      HIGH INCOME
# where each bar is split out by all the bill componenets 
# (1) fixed charge
# (2) variable charge
# (3) rate increase from infra
# (4) rate increase due to surcharge 

path = "~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/plot10"
params <- read.csv(paste0(path,"/parameter_list.csv"))

bills_list = list.files(path = path,pattern = "*bills*",full.names = T)
bills_name <- list.files(path=path,pattern="*bills*",full.names = F)
numbers <- extract_numeric(bills_name)
usage = list.files(path = path,pattern= "*demand*",full.names=T)
outputs_list = list.files(path = path,pattern= "*output*",full.names=T)

for(i in 1:nrow(params)){
  print(i)
  bills <- read.csv(bills_list[i])
  outputs <- read.csv(outputs_list[i])
  demand <- read.csv(usage[i])
  these_params <- params[which(params$X==numbers[i]),]
  
  df_plot <- data.frame(date=as.Date(outputs$Date))
  df_plot$fixedCharge <- outputs$fixedCharge-outputs$surchargeIncrease -outputs$infrastructureIncrease
  df_plot$surchargeIncrease <- outputs$surchargeIncrease
  df_plot$infrastructureIncrease <- outputs$infrastructureIncrease
  
  # now get the bills for the low and high-income households 
  df_plot_LI <- df_plot
  df_plot_HI <- df_plot
  df_plot_LI$volumetricCharge <- rowMeans(cbind(bills$X7500,bills$X12500,bills$X17500))
  df_plot_LI$volumetricCharge <- df_plot_LI$volumetricCharge-df_plot$fixedCharge -outputs$surchargeIncrease - outputs$infrastructureIncrease
  
  df_plot_HI$volumetricCharge <- rowMeans(cbind(bills$X137500,bills$X175000,bills$X250000))
  df_plot_HI$volumetricCharge <- df_plot_HI$volumetricCharge-df_plot$fixedCharge-df_plot$fixedCharge -outputs$surchargeIncrease - outputs$infrastructureIncrease
  
  df_plot_LI_melt <- melt(df_plot_LI,id.vars = c("date"))
  df_plot_HI_melt <- melt(df_plot_HI,id.vars=c("date"))
  
  df_plot_LI_melt$variable <- factor(df_plot_LI_melt$variable,levels=c("surchargeIncrease","volumetricCharge","infrastructureIncrease","fixedCharge"))
  df_plot_HI_melt$variable <- factor(df_plot_HI_melt$variable,levels=c("surchargeIncrease","volumetricCharge","infrastructureIncrease","fixedCharge"))
  # now lets try a stacked area chart 
 
  p1 <- ggplot() + geom_area(data=df_plot_LI_melt,aes(x=date,y=value,fill=variable),alpha=0.7,color="black")+scale_fill_npg()+theme_bw()+
    labs(x="Date",y="Low-Income Bills (USD)",title = "Low-Income Bills")+ylim(c(0,200))+theme(legend.position = "none")
  
  p2 <- ggplot() + geom_area(data=df_plot_HI_melt,aes(x=date,y=value,fill=variable),alpha=0.7,color="black")+scale_fill_npg()+theme_bw()+
    labs(x="Date",y="High-Income Bills (USD)",title = "High-Income Bills")+ylim(c(0,200))
  p2_let <- get_legend(p2)
  param_text <-
    paste0(
      "Drought: ",
      these_params$drought_characteristic ,
      "\n Income Distribution: ",
      these_params$income_distribution,
      "\n Income Elasticity: ",
      these_params$income_elasticity,
      "\n Fee Passthrough: ",
      these_params$fee_passthrough,
      "\n Price Elasticity: ",
      these_params$price_elasticity,
      "\n Reservoir Capacity: ",
      these_params$reservoir_capacity,
      " MG",
      "\n PBP: ",
      these_params$pay_back_period,
      " yrs",
      "\n Discount Rate: ",
      these_params$discount_rate ,
      " %",
      "\n Mitigation Decision: ",
      these_params$mitigation_decision,
      "\n Build Decision: ",
      these_params$build_decision
    )
  
 
  p2 <- p2+theme(legend.position = "none")
  p_final <- plot_grid(p1,p2,p2_let,nrow=1,rel_widths = c(1,1,.75)) 
  p_final <- p_final  +  draw_label(
      param_text,
      color = "black",
      size = 10,
      hjust = 1,
      vjust = 0,
      y = .02,
      x = .98
    )
  filename <- paste0(path,"/figures/billl_parts_",numbers[i],".pdf")
  ggsave(p_final,filename=filename,width=10,height=6,unit="in")
  
  # now the next figure....
  # need:
  # baseline demand (dashed)
  # demand change from restrictions
  # demand change from price response
  plot_2_df <- data.frame(date = as.Date(outputs$Date))
  plot_2_df_LI <- plot_2_df  
  plot_2_df_LI$final <- rowMeans(cbind(demand$X7500,demand$X12500,demand$X17500))
  plot_2_df_LI$raw   <- rowMeans(cbind(demand$raw_7500,demand$raw_12500,demand$raw_17500))
  plot_2_df_LI$PEDreduction <- outputs$pedReduction*plot_2_df_LI$raw
  plot_2_df_LI$curtail_reduction <- outputs$conserveStatus*plot_2_df_LI$raw

  plot_2_df_LI_melt <- melt(plot_2_df_LI[,c("date","PEDreduction","curtail_reduction","final")],id.vars = "date")
  demand_plot_LI <- ggplot() + geom_area(data=plot_2_df_LI_melt,aes(x=date,y=value/748,fill=variable),alpha=0.7,color="black")  +
    geom_line(data=(plot_2_df_LI),aes(x=date,y=final/748))+scale_fill_manual(values=c("PEDreduction"="#DC0000FF","curtail_reduction"="#4DBBD5FF","final"=NULL))+
    theme_bw()+labs(x="Date",y="CCF Used",title = "Low-income Water Use")+ylim(c(0,12))
   # labs(x="Date",y="High-Income Bills (USD)",title = "High-Income Bills")+ylim(c(0,200))
 
  demand_plot_LI <- demand_plot_LI + theme(legend.position = "none")
  
  plot_2_df_HI <- plot_2_df  
  plot_2_df_HI$final <- rowMeans(cbind(demand$X137500,demand$X175000,demand$X250000))
  plot_2_df_HI$raw   <- rowMeans(cbind(demand$raw_137500,demand$raw_175000,demand$raw_250000))
  plot_2_df_HI$PEDreduction <- outputs$pedReduction*plot_2_df_HI$raw
  plot_2_df_HI$curtail_reduction <- outputs$conserveStatus*plot_2_df_HI$raw
  
  plot_2_df_HI_melt <- melt(plot_2_df_HI[,c("date","PEDreduction","curtail_reduction","final")],id.vars = "date")
  demand_plot_HI <- ggplot() + geom_area(data=plot_2_df_HI_melt,aes(x=date,y=value/748,fill=variable),alpha=0.7,color="black")  +
    geom_line(data=(plot_2_df_HI),aes(x=date,y=final/748))+scale_fill_manual(values=c("PEDreduction"="#DC0000FF","curtail_reduction"="#4DBBD5FF","final"=NULL))+
    theme_bw() + labs(x="Date",y="CCF Used",title="High-income Water Use")+ylim(c(0,12))
  # labs(x="Date",y="High-Income Bills (USD)",title = "High-Income Bills")+ylim(c(0,200))
  demand_leg <- get_legend(demand_plot_HI)
  demand_plot_HI <- demand_plot_HI + theme(legend.position = "none")
  # removing the legend and making a combined figure 
  demand_final <- plot_grid(demand_plot_LI,demand_plot_HI,demand_leg,nrow = 1,rel_widths = c(1,1,.75))
  
  demand_final  <- demand_final  +  draw_label(
    param_text,
    color = "black",
    size = 10,
    hjust = 1,
    vjust = 0,
    y = .02,
    x = .98
  )    
  demand_filename <- paste0(path,"/figures/demand_parts_",numbers[i],".pdf")
  ggsave(demand_final,filename=demand_filename,width=10,height=6,unit="in")
  
  
  # now the figure describing water availability 
  outputs$res_drawdown[is.na(outputs$res_drawdown)] <- 0
  outputs$market_buy[is.na(outputs$market_buy)]<-0
  # raw water availability 
  outputs$Date <- as.Date(outputs$Date)
  
  outputs$total_water <- outputs$northCoast+outputs$taitStreet+outputs$feltonDiversions+outputs$res_drawdown+outputs$ground+outputs$build_prod
  avail_water_melt <- melt(outputs,id.vars = "Date",measure.vars = c("unadjusted_demand","total_water","totalDemand"))
  
  levels(avail_water_melt$variable) <- c("unadjusted_demand","total_water","adjusted_demand")
  water_avail <- ggplot() + geom_line(data=avail_water_melt,aes(x=Date,y=value,color=variable),size=1.)+theme_bw()+scale_color_npg()+
    labs(x="Date",y="MG/month",title = "Available Water, Raw Demand, Adjusted Demand")
  water_avail  <- plot_grid(water_avail)  +  draw_label(
    param_text,
    color = "black",
    size = 10,
    hjust = 1,
    vjust = 0,
    y = .02,
    x = .98
  )    
  avail_filename <- paste0(path,"/figures/water_avail_",numbers[i],".pdf")
  ggsave(water_avail,filename=avail_filename,width=10,height=6,unit="in")
  
  # now a deficit and actions figure
  outputs$deficit_without_changes <- outputs$unadjusted_demand-outputs$total_water -outputs$build_prod
  outputs$deficit_without_changes[outputs$deficit_without_changes<=0]<-0
  
  # 
  outputs$total_mitigation <- outputs$conserveStatus* (outputs$unadjusted_demand-outputs$otherDemand)+ outputs$market_buy
  deficit_melt <- melt(outputs,id.vars = "Date",measure.vars = c("total_mitigation","deficit_without_changes"))
  
  deficit_actions <- ggplot() + geom_line(data=deficit_melt,aes(x=Date,y=value,color=variable))+theme_bw()+scale_color_npg()+
    labs(x="Date",y="MG/Month",title="Deficit volume and mitigation actions")
    
  deficit_filename <- paste0(path,"/figures/deficit_",numbers[i],".pdf")
  ggsave(deficit_actions,filename=deficit_filename,width=10,height=6,unit="in")
  
}

