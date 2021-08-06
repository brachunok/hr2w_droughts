# make a 'stackgraph'
# top plot shows the monthly demand and how we are meeting that demand

# bottom shows the reservoir levels
library(ggplot2)
library(reshape2)
library(cowplot)
library(readr)


path = "../../outputs/santa_cruz/experiments"
write_path = "./balance_plots"
param_list <- read.csv(paste0(path,"/parameter_list.csv"))
scenarios <-c(359,357)
#scenarios <- readr::parse_number(list.files(path,pattern="*output*")

# do a for loop here getting the scenarios from scenarios
for ( i in 1:length(scenarios)){
  
  outputs <- read.csv(paste0(path,"/",scenarios[i],'_outputs.csv'))
  outputs$Date <- as.Date(outputs$Date)
  
  # get the bills
  bills <- read.csv(paste0(path,"/",scenarios[i],'_hh_bills.csv'))
  bills$Date <- outputs$Date
  
  bills$dp <- rowMeans(cbind(bills$X7500,bills$X12500,bills$X17500))
  bills$hi <- rowMeans(cbind(bills$X137500,bills$X175000,bills$X250000))
  
  # NA fix 
  outputs[is.na(outputs)] <- 0
  
  # get parameters
  these_params <- param_list[which(param_list$X==scenarios[i]),]
  
  outputs_melted <- melt(outputs,id.vars = "Date",measure.vars = c("northCoast","taitStreet","res_drawdown","build_prod","ground","deficit",'market_buy'))
  outputs_melted$variable <- factor(outputs_melted$variable,levels=c("deficit","market_buy", "res_drawdown","northCoast","taitStreet","ground","build_prod"))
  
  p_top <- ggplot()+geom_area(data=outputs_melted,aes(x=Date,y=value,fill=variable))+
    geom_line(data=outputs,aes(x=Date,y=totalDemand))+
    geom_line(data=outputs,aes(x=Date,y=unadjusted_demand),linetype = "dashed")+
    theme_bw()+
    scale_fill_manual(values=c('#ff00ff', '#ffff00','#e31a1c','#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99') )+
    ylab("MG/month") + ggtitle(paste0("Water Sources & Reservoir levels: Scenario ",scenarios[i]))
  
  p_top_no_leg <- p_top+ theme(legend.position="none")
  
  legend <- get_legend(p_top)
    
  # get the reservoir size for this parameter
  p_bottom <- ggplot()+geom_line(data=outputs,aes(x=Date,y=level))+
    theme_bw()+
    ylab("Reservoir Storage (MG)") + geom_hline(yintercept = these_params$reservoir_capacity)
  
  
  
  # now make output plot 3 which is the cost over time for the utility
  p_utility_cost <- ggplot()+ geom_line(data=outputs,aes(x=Date,y=monthlyCost))+theme_bw()+
    ylab("Monthly Utility Cost ($)")+
    scale_y_continuous(labels=scales::dollar_format())
  
  # melt the low income and high income costs into each 
  bills_melted <- melt(bills,id.vars = "Date",measure.vars = c("dp","hi"),variable.name = "Income_Group")
  bills_plot <- ggplot() + geom_line(data=bills_melted,aes(x=Date,y=value,color=Income_Group)) + theme_bw()
  bills_plot_no_leg <- bills_plot + theme(legend.position="none") + ylab("Monthly Water Billl ($)")
  bills_legend <- get_legend(bills_plot)
    
  # list of parameters in text form
  output_plot <- plot_grid(p_top_no_leg,p_bottom,p_utility_cost,bills_plot_no_leg,labels = c('a','b','c','d'),ncol=1,align = "v")
  legend_plot <- plot_grid(legend,NULL,NULL,bills_legend,ncol=1)
  output_plot2 <- plot_grid(output_plot,legend_plot,ncol=2,rel_widths = c(4,1))
  
  
  # and output plot 4/5 which are the bills of the lower class and upper class households
  param_text <- paste0("Drought: ",these_params$drought_characteristic ,
                       "\n Income Distribution: ", these_params$income_distribution,
                       "\n Income Elasticity: ",these_params$income_elasticity,
                       "\n Fee Passthrough: ",these_params$fee_passthrough,
                       "\n Reservoir Capacity: ",these_params$reservoir_capacity," MG",
                       "\n PBP: ",these_params$pay_back_period, " yrs",
                       "\n Discount Rate: ",these_params$discount_rate ," %",
                       "\n Mitigation Decision: ",these_params$mitigation_decision,
                       "\n Build Decision: ", these_params$build_decision)
                       
 output_plot2 <- output_plot2 +    draw_label(param_text, color = "black", size = 10,hjust = 1,vjust = 0, y = .5,x=.95) +theme(text=element_text(family="mono"))
 #output_plot2
 ggsave(output_plot2,filename = paste0(write_path,"/",scenarios[i],".pdf"),width = 11,height = 8,units = "in")
 print(paste0("Writing scenario: ",scenarios[i]))
}


