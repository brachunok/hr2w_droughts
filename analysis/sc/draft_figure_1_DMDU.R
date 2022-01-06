# paper 1 figure drafts

# figure 1 ---------------------------------------------------------------------
# left is a schematic, right is model outputs


# add a whole other column for another drought scenario
# make reservoir level and cost 1/2 hight, make bill and demand bigg

library(ggplot2)
library(reshape2)
library(cowplot)
library(readr)
library(GGally)
library(dplyr)
library(ggrepel)
library(viridis)
library(hrbrthemes)
library(reshape2)
library("scales")
library("ggsci")
library(cowplot)

path = "../../outputs/santa_cruz/experiments/plot1"
#write_path = "./balance_plots"
param_list <- read.csv(paste0(path, "/parameter_list.csv"))
scenarios <- readr::parse_number(list.files(path,pattern="*output*"))

# do a for loop here getting the scenarios from scenarios
i <-1
outputs_c <- read.csv(paste0(path, "/", scenarios[i], '_outputs.csv'))
outputs_c$Date <- as.Date(outputs_c$Date)

outputs_m <- read.csv(paste0(path, "/", scenarios[i+1], '_outputs.csv'))
outputs_m$Date <- as.Date(outputs_m$Date)

# get the bills
bills_c <- read.csv(paste0(path, "/", scenarios[i], '_hh_bills.csv'))
bills_c$Date <- outputs_c$Date

bills_c$dp <- rowMeans(cbind(bills_c$X7500, bills_c$X12500, bills_c$X17500))
bills_c$hi <-
  rowMeans(cbind(bills_c$X137500, bills_c$X175000, bills_c$X250000))

bills_m <- read.csv(paste0(path, "/", scenarios[i+1], '_hh_bills.csv'))
bills_m$Date <- outputs_m$Date

bills_m$dp <- rowMeans(cbind(bills_m$X7500, bills_m$X12500, bills_m$X17500))
bills_m$hi <-
  rowMeans(cbind(bills_m$X137500, bills_m$X175000, bills_m$X250000))


demand_c <- read.csv(paste0(path,"/",scenarios[i],'_hh_demand.csv'))
demand_c$Date <- outputs_c$Date

demand_c$dp <- rowMeans(cbind(demand_c$X7500,demand_c$X12500,demand_c$X17500))
demand_c$hi <- rowMeans(cbind(demand_c$X137500,demand_c$X175000,demand_c$X250000))

demand_c$dp_ped_only <- rowMeans(cbind(demand_c$ped_7500,demand_c$ped_12500,demand_c$ped_17500))
demand_c$hi_ped_only <- rowMeans(cbind(demand_c$ped_137500,demand_c$ped_175000,demand_c$ped_250000))

demand_c$dp_mand_only <- rowMeans(cbind(demand_c$mandonly_7500,demand_c$mandonly_12500,demand_c$mandonly_17500))
demand_c$hi_mand_only <- rowMeans(cbind(demand_c$mandonly_137500,demand_c$mandonly_175000,demand_c$mandonly_250000))

demand_c$dp_baseline <- rowMeans(cbind(demand_c$raw_7500,demand_c$raw_12500,demand_c$raw_17500))
demand_c$hi_baseline <- rowMeans(cbind(demand_c$raw_137500,demand_c$raw_175000,demand_c$raw_250000))


# and now the market versions
demand_m <- read.csv(paste0(path,"/",scenarios[i+1],'_hh_demand.csv'))
demand_m$Date <- outputs_m$Date

demand_m$dp <- rowMeans(cbind(demand_m$X7500,demand_m$X12500,demand_m$X17500))
demand_m$hi <- rowMeans(cbind(demand_m$X137500,demand_m$X175000,demand_m$X250000))

demand_m$dp_ped_only <- rowMeans(cbind(demand_m$ped_7500,demand_m$ped_12500,demand_m$ped_17500))
demand_m$hi_ped_only <- rowMeans(cbind(demand_m$ped_137500,demand_m$ped_175000,demand_m$ped_250000))

demand_m$dp_mand_only <- rowMeans(cbind(demand_m$mandonly_7500,demand_m$mandonly_12500,demand_m$mandonly_17500))
demand_m$hi_mand_only <- rowMeans(cbind(demand_m$mandonly_137500,demand_m$mandonly_175000,demand_m$mandonly_250000))

demand_m$dp_baseline <- rowMeans(cbind(demand_m$raw_7500,demand_m$raw_12500,demand_m$raw_17500))
demand_m$hi_baseline <- rowMeans(cbind(demand_m$raw_137500,demand_m$raw_175000,demand_m$raw_250000))

# now get the DP and HI bills for the other three
bills_c$dp_demand_only <- rowMeans(cbind(bills_c$demandonly_7500,bills_c$demandonly_12500,bills_c$demandonly_17500))
bills_c$hi_demand_only <- rowMeans(cbind(bills_c$demandonly_137500,bills_c$demandonly_175000,bills_c$demandonly_250000))
# &^^ this one is the 'lower bound' 

bills_c$dp_rate_only <- rowMeans(cbind(bills_c$rateonly_7500,bills_c$rateonly_12500,bills_c$rateonly_17500))
bills_c$hi_rate_only <- rowMeans(cbind(bills_c$rateonly_137500,bills_c$rateonly_175000,bills_c$rateonly_250000))

bills_c$dp_no_change <-rowMeans(cbind(bills_c$raw_7500,bills_c$raw_12500,bills_c$raw_17500))
bills_c$hi_no_change <-rowMeans(cbind(bills_c$raw_137500,bills_c$raw_175000,bills_c$raw_250000))

# and the m

# now get the DP and HI bills for the other three
bills_m$dp_demand_only <- rowMeans(cbind(bills_m$demandonly_7500,bills_m$demandonly_12500,bills_m$demandonly_17500))
bills_m$hi_demand_only <- rowMeans(cbind(bills_m$demandonly_137500,bills_m$demandonly_175000,bills_m$demandonly_250000))
# &^^ this one is the 'lower bound' 

bills_m$dp_rate_only <- rowMeans(cbind(bills_m$rateonly_7500,bills_m$rateonly_12500,bills_m$rateonly_17500))
bills_m$hi_rate_only <- rowMeans(cbind(bills_m$rateonly_137500,bills_m$rateonly_175000,bills_m$rateonly_250000))

bills_m$dp_no_change <-rowMeans(cbind(bills_m$raw_7500,bills_m$raw_12500,bills_m$raw_17500))
bills_m$hi_no_change <-rowMeans(cbind(bills_m$raw_137500,bills_m$raw_175000,bills_m$raw_250000))

# NA fix
outputs_c[is.na(outputs_c)] <- 0
outputs_m[is.na(outputs_m)] <- 0

# fix plot error by shrinking northcoast m
outputs_m$northCoast <-  (outputs_m$totalDemand-(outputs_m$market_buy+outputs_m$res_drawdown+outputs_m$taitStreet+outputs_m$ground))

# fix outputs_c to reflect actual water 'dispatch' order
for (i in 1:nrow(outputs_c)){
  def <- (outputs_c$ground[i]+outputs_c$northCoast[i]+outputs_c$taitStreet[i]+outputs_c$res_drawdown[i])-outputs_c$totalDemand[i]
  print(def)
  if ( def > 0){
    
    if (def<outputs_c$res_drawdown[i]){
      outputs_c$res_drawdown[i] <- outputs_c$res_drawdown[i] - def
      
      
    }else{
      def <- def-outputs_c$res_drawdown[i]
      outputs_c$res_drawdown[i] <- 0 
      
      # gotta go to the next
      if (def<outputs_c$northCoast){
        outputs_c$northCoast[i] <- outputs_c$northCoast[i]-def
        
      }else{
          # on to the next thing
          def <- def-outputs_c$northCoast[i]
          outputs_c$northCoast[i] <- 0
          
          if (def<outputs_c$taitStreet[i]){
            outputs_c$taitStreet[i] <- outputs_c$taitStreet[i]-def 
            
          }
      }
    }
      
  }
}

# get parameters
these_params <- param_list[which(param_list$X == scenarios[i]), ]

# set limit dates
ll_date = as.Date("2013-06-01")
ul_date = as.Date("2015-12-31")
outputs_c_melted <-
  melt(
    outputs_c,
    id.vars = "Date",
    measure.vars = c(
      "northCoast",
      "taitStreet",
      "res_drawdown",
      "build_prod",
      "ground",
      "deficit",
      'market_buy'
    )
  )

outputs_m_melted <-
  melt(
    outputs_m,
    id.vars = "Date",
    measure.vars = c(
      "northCoast",
      "taitStreet",
      "res_drawdown",
      "build_prod",
      "ground",
      "deficit",
      'market_buy'
    )
  )


outputs_c_melted$variable <-
  factor(
    outputs_c_melted$variable,
    levels = c(
      "deficit",
      "market_buy",
      "res_drawdown",
      "northCoast",
      "taitStreet",
      "ground",
      "build_prod"
    )
  )

outputs_m_melted$variable <- factor(
  outputs_m_melted$variable,
  levels = c(
    "deficit",
    "market_buy",
    "res_drawdown",
    "northCoast",
    "taitStreet",
    "ground",
    "build_prod"
  )
)

p_top_c <-
  ggplot() + geom_area(data = outputs_c_melted, aes(x = Date, y = value, fill =
                                                      variable)) +
  xlim(c(ll_date,ul_date))+
  geom_line(data = outputs_c, aes(x = Date, y = totalDemand)) +
  geom_line(data = outputs_c,
            aes(x = Date, y = unadjusted_demand),
            linetype = "dashed") +
  theme_bw() +
  scale_fill_manual(
    values = c(
      '#ff00ff',
      '#ffff00',
      '#e31a1c',
      '#a6cee3',
      '#1f78b4',
      '#b2df8a',
      '#33a02c',
      '#fb9a99'
    )
  ) +
  ylab("MG/month") + ggtitle(paste0("Historical Drought, Curtailment")) +
  theme(axis.title.x=element_blank())+ylim(c(0,300))

top_leg <- get_legend(p_top_c)

p_top_c <- p_top_c + theme(legend.position = "none")
# now: we need P_top M

p_top_m <-
  ggplot() + geom_area(data = outputs_m_melted, aes(x = Date, y = value, fill =
                                                      variable)) +
  xlim(c(ll_date,ul_date))+
  geom_line(data = outputs_m, aes(x = Date, y = totalDemand)) +
  
  theme_bw() +
  scale_fill_manual(
    values = c(
      '#ff00ff',
      '#ffff00',
      '#e31a1c',
      '#a6cee3',
      '#1f78b4',
      '#b2df8a',
      '#33a02c',
      '#fb9a99'
    )
  ) +
  ylab("MG/month") + ggtitle(paste0("Historical Drought, Market Water")) +
  theme(axis.title.x=element_blank())+ylim(c(0,300))+ theme(legend.position = "none")


#p_top_m

# now do the bottom parts 
p_utility_cost <-
  ggplot() + geom_line(data = outputs_c, aes(x = Date, y = monthlyCost)) +
  theme_bw() +xlim(c(ll_date,ul_date))+
  ylab("Monthly Utility Cost ($)") +
  scale_y_continuous(labels = scales::dollar_format())+
  geom_line(data=outputs_m,aes(x=Date,y=monthlyCost),color="blue")+
  theme(axis.title.x=element_blank())

# melt the low income and high income costs into each
bills_c_melted <-
  melt(
    bills_c,
    id.vars = "Date",
    measure.vars = c("dp", "hi"),
    variable.name = "Income_Group"
  )

bills_m_melted <-
  melt(
    bills_m,
    id.vars = "Date",
    measure.vars = c("dp", "hi"),
    variable.name = "Income_Group"
  )

# add the utility cost to this, requires some 
scaling_factor <- max(bills_c_melted$value)/max(outputs_c$monthlyCost)


scaling_factor_m <- (.5*max(bills_m_melted$value))/max(outputs_m$monthlyCost)


bills_plot_c <- ggplot() + geom_line(data=bills_c_melted,aes(x=Date,y=value,color=Income_Group))+theme_bw()+ xlim(c(ll_date,ul_date))+
  geom_line(data=outputs_c,aes(x=Date,y=monthlyCost*scaling_factor_m),color="gray")+
  scale_y_continuous(name="Monthly Water Bill (USD)",sec.axis = sec_axis(~./scaling_factor_m,name="Added Utility Cost (USD)"))+
  geom_line(data=bills_c,aes(x=Date,y=dp_no_change),linetype="dashed",color="#F8766D")+
  geom_line(data=bills_c,aes(x=Date,y=hi_no_change),linetype="dashed",color="#00BFC4") + theme(legend.position = "none")
 
  
bills_plot_c

# now add the 'what it should have been
bills_plot <- ggplot() + geom_line(data=bills_c,aes(x=Date,y=dp_no_change),linetype="dashed",color="black")
#bills_plot <- bills_plot + geom_line(data=bills_c,aes(x=Date,y=hi_no_change),linetype="dashed",color="gray")



bills_plot_m <- ggplot() + geom_line(data=bills_m_melted,aes(x=Date,y=value,color=Income_Group))+theme_bw()+ xlim(c(ll_date,ul_date))+
  geom_line(data=outputs_m,aes(x=Date,y=monthlyCost*scaling_factor_m),color="gray")+
  scale_y_continuous(name="Monthly Water Bill (USD)",sec.axis = sec_axis(~./scaling_factor_m,name="Added Utility Cost (USD)"))+
  geom_line(data=bills_c,aes(x=Date,y=dp_no_change),linetype="dashed",color="#F8766D")+
  geom_line(data=bills_c,aes(x=Date,y=hi_no_change),linetype="dashed",color="#00BFC4")

bottom_leg <- get_legend(bills_plot_m)
bills_plot_m <- bills_plot_m + theme(legend.position = "none")
bills_plot_m

# now add the 'what it should have been

plot_c <- plot_grid(p_top_c,bills_plot_c,nrow=2,align = 'v', axis = 'lr')
plot_m <- plot_grid(p_top_m,bills_plot_m,nrow=2,align = 'v',axis='lr')
p_leg <- plot_grid(top_leg,bottom_leg,nrow=2)
p_final <- plot_grid(plot_c,plot_m,p_leg,nrow=1,rel_widths = c(1,1,.15))

ggsave(p_final,filename = "~/Documents/__college/reseach_stuff/hr2w_droughts/analysis/sc/draft_figure_1_AGU.pdf",width=5,height=3,unit="in",scale=1.75)










