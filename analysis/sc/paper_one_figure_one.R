# paper 1 figure drafts

# figure 1 ---------------------------------------------------------------------
# left is a schematic, right is model outputs


# add a whole other column for another drought scenario
# make reservoir level and cost 1/2 hight, make bill and demand bigger


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

p_top <-
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
  ylab("MG/month") + ggtitle(paste0("Historical Drought")) +
  theme(axis.title.x=element_blank())

p_top_no_leg <- p_top + theme(legend.position = "none")

legend <- get_legend(p_top)

# get the reservoir size for this parameter
p_bottom <- ggplot() + geom_line(data = outputs_c, aes(x = Date, y = level)) +
  theme_bw() +xlim(c(ll_date,ul_date))+
  ylab("Reservoir Storage (MG)") + geom_hline(yintercept = these_params$reservoir_capacity) +
  geom_line(data=outputs_m,aes(x=Date,y=level),color="blue")+
  theme(axis.title.x=element_blank())+ylim(0,2800)



# now make output plot 3 which is the cost over time for the utility
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

# now add the 'what it should have been
bills_plot <- ggplot() + geom_line(data=bills_c,aes(x=Date,y=dp_no_change),linetype="dashed",color="black")
#bills_plot <- bills_plot + geom_line(data=bills_c,aes(x=Date,y=hi_no_change),linetype="dashed",color="gray")

# now add the lower bound which is hi/li _demand_only
#bills_plot <- bills_plot + geom_line(data=bills_c,aes(x=Date,y=dp_demand_only),alpha=0.5,color="black",linetype="dotted")
#bills_plot <- bills_plot + geom_line(data=bills_m,aes(x=Date,y=dp_demand_only),alpha=0.5,color="blue",linetype="dotted")

# now the upper bound 
#bills_plot <- bills_plot + geom_line(data=bills_c,aes(x=Date,y=dp_rate_only),alpha=0.5,color="black",linetype="longdash")
#bills_plot <- bills_plot + geom_line(data=bills_m,aes(x=Date,y=dp_rate_only),alpha=0.5,color="blue",linetype="longdash")

# now the final bill
bills_plot <- bills_plot +  geom_line(data = bills_c, aes(x = Date, y = dp)) 
bills_plot <- bills_plot +  geom_line(data = bills_m, aes(x = Date, y = dp),color="blue") +theme_bw()+xlim(c(ll_date,ul_date))+ylim(0,135)
  
bills_plot
bills_plot_no_leg <-
  bills_plot + theme(legend.position = "none") + ylab("Monthly Water Billl ($)")+
  theme(axis.title.x=element_blank())
bills_legend <- get_legend(bills_plot)

# now make the demand plot --------------------

# first melt demand
demand_melted <-
  melt(
    demand_c,
    id.vars = "Date",
    measure.vars = c("dp", "hi"),
    variable.name = "Income_Group"
  )


demand_plot <- ggplot() + geom_line(data=demand_c,aes(x=Date,y=dp_baseline),linetype="dashed",color="black") +xlim(c(ll_date,ul_date))
demand_plot <- demand_plot + geom_line(data=demand_m,aes(x=Date,y=dp_baseline),linetype="dashed",color="blue")

# now add the lines which are PED only but make it area?
#demand_plot <- demand_plot + geom_line(data=demand_c,aes(x=Date,y=dp_ped_only),alpha=0.5,color="black",linetype="dotted")
#demand_plot <- demand_plot + geom_line(data=demand_m,aes(x=Date,y=dp_ped_only),alpha=0.5,color="blue",linetype="dotted")

# now the curtailment only but make it area
#demand_plot <- demand_plot + geom_line(data=demand_c,aes(x=Date,y=dp_ped_only),alpha=0.5,color="black",linetype="longdash")
#demand_plot <- demand_plot + geom_line(data=demand_m,aes(x=Date,y=dp_ped_only,),alpha=0.5,color="blue",linetype="longdash")

# add baseline
demand_plot <- demand_plot + geom_line(data=demand_c,aes(x=Date,y=dp),color="black")
demand_plot <- demand_plot + geom_line(data=demand_m,aes(x=Date,y=dp),color="blue") + theme_bw()+ylim(0,4000)


demand_plot
demand_plot_no_leg <-
  demand_plot + theme(legend.position = "none") + ylab("Monthly Water Use (G)")+
  theme(axis.title.x=element_blank())
demand_legend <- get_legend(demand_plot)

# list of parameters in text form
output_plot <-
  plot_grid(
    p_top_no_leg,
    p_bottom,
    p_utility_cost,
    demand_plot_no_leg,
    bills_plot_no_leg,
    labels = c('a', 'b', 'c', 'd','e'),
    ncol = 1,
    align = "v",
    rel_heights = c(1.5,1,1,2,2)
  )
legend_plot <- plot_grid(legend, NULL, NULL, demand_legend,bills_legend, ncol = 1)

# now for the entirely seperate drought -----------------------------------------------
i <-3
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

# get parameters
these_params <- param_list[which(param_list$X == scenarios[i]), ]

# set limit dates
ll_date = as.Date("2012-06-01")
ul_date = as.Date("2016-12-31")
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

p_top <-
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
  ylab("MG/month") + ggtitle(paste0("Hypothesized Long, Intense Drought")) +
  theme(axis.title.x=element_blank())

p_top_no_leg <- p_top + theme(legend.position = "none")

legend <- get_legend(p_top)

# get the reservoir size for this parameter
p_bottom <- ggplot() + geom_line(data = outputs_c, aes(x = Date, y = level)) +
  theme_bw() +xlim(c(ll_date,ul_date))+
  ylab("Reservoir Storage (MG)") + geom_hline(yintercept = these_params$reservoir_capacity) +
  geom_line(data=outputs_m,aes(x=Date,y=level),color="blue")+
  theme(axis.title.x=element_blank())+ylim(0,2800)



# now make output plot 3 which is the cost over time for the utility
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

# now add the 'what it should have been
bills_plot <- ggplot() + geom_line(data=bills_c,aes(x=Date,y=dp_no_change),linetype="dashed",color="black")
#bills_plot <- bills_plot + geom_line(data=bills_c,aes(x=Date,y=hi_no_change),linetype="dashed",color="gray")

# now add the lower bound which is hi/li _demand_only
#bills_plot <- bills_plot + geom_line(data=bills_c,aes(x=Date,y=dp_demand_only),alpha=0.5,color="black",linetype="dotted")
#bills_plot <- bills_plot + geom_line(data=bills_m,aes(x=Date,y=dp_demand_only),alpha=0.5,color="blue",linetype="dotted")

# now the upper bound 
#bills_plot <- bills_plot + geom_line(data=bills_c,aes(x=Date,y=dp_rate_only),alpha=0.5,color="black",linetype="longdash")
#bills_plot <- bills_plot + geom_line(data=bills_m,aes(x=Date,y=dp_rate_only),alpha=0.5,color="blue",linetype="longdash")

# now the final bill
bills_plot <- bills_plot +  geom_line(data = bills_c, aes(x = Date, y = dp)) 
bills_plot <- bills_plot +  geom_line(data = bills_m, aes(x = Date, y = dp),color="blue") +theme_bw()+xlim(c(ll_date,ul_date))+ylim(0,135)


bills_plot
bills_plot_no_leg <-
  bills_plot + theme(legend.position = "none") + ylab("Monthly Water Billl ($)")+
  theme(axis.title.x=element_blank())
bills_legend <- get_legend(bills_plot)

# now make the demand plot --------------------

# first melt demand
demand_melted <-
  melt(
    demand_c,
    id.vars = "Date",
    measure.vars = c("dp", "hi"),
    variable.name = "Income_Group"
  )


demand_plot <- ggplot() + geom_line(data=demand_c,aes(x=Date,y=dp_baseline),linetype="dashed",color="black") +xlim(c(ll_date,ul_date))
demand_plot <- demand_plot + geom_line(data=demand_m,aes(x=Date,y=dp_baseline),linetype="dashed",color="black")

# now add the lines which are PED only but make it area?
#demand_plot <- demand_plot + geom_line(data=demand_c,aes(x=Date,y=dp_ped_only),alpha=0.5,color="black",linetype="dotted")
#demand_plot <- demand_plot + geom_line(data=demand_m,aes(x=Date,y=dp_ped_only),alpha=0.5,color="blue",linetype="dotted")

# now the curtailment only but make it area
#demand_plot <- demand_plot + geom_line(data=demand_c,aes(x=Date,y=dp_ped_only),alpha=0.5,color="black",linetype="longdash")
#demand_plot <- demand_plot + geom_line(data=demand_m,aes(x=Date,y=dp_ped_only,),alpha=0.5,color="blue",linetype="longdash")

# add baseline
demand_plot <- demand_plot + geom_line(data=demand_c,aes(x=Date,y=dp),color="black")
demand_plot <- demand_plot + geom_line(data=demand_m,aes(x=Date,y=dp),color="blue") + theme_bw()+ylim(0,4000)


demand_plot
demand_plot_no_leg <-
  demand_plot + theme(legend.position = "none") + ylab("Monthly Water Use (G)")+
  theme(axis.title.x=element_blank())
demand_legend <- get_legend(demand_plot)

# list of parameters in text form
output_plot_intense <-
  plot_grid(
    p_top_no_leg,
    p_bottom,
    p_utility_cost,
    demand_plot_no_leg,
    bills_plot_no_leg,
    labels = c('a', 'b', 'c', 'd','e'),
    ncol = 1,
    align = "v",
    rel_heights = c(1.5,1,1,2,2)
  )
legend_plot <- plot_grid(legend, NULL, NULL, demand_legend,bills_legend, ncol = 1)


output_plot2 <-
  plot_grid(output_plot,output_plot_intense,
            legend_plot,
            ncol = 3,
            rel_widths = c(4,4, 1))

ggsave(output_plot2,filename = "draft_figure_1.pdf",width = 9,height=6,scale = 2)
