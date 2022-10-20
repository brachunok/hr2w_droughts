# analyze rate structure outputs

source("~/Documents/__college/reseach_stuff/hr2w_droughts/analysis/sc/process_outputs.R")
source("~/Documents/__college/reseach_stuff/hr2w_droughts/analysis/sc/paper_one_analyses/analyze_outputs_lock_in.R")
library(ggplot2)
library(reshape)
library('ggpattern')
load("~/Documents/__college/reseach_stuff/hr2w_droughts/analysis/processed_and_binned_bill_data_adam_sim.Rdata")

outputs_plots <- melt(outputs,id.vars = c("drought_characteristic","mitigation_decision","rate_structure","fee_passthrough"),measure.vars = 
                        c("deep_poverty_total_avg","upper_class_total_avg","deep_poverty_perc_max_avg","upper_class_perc_max_avg"))
p1 <- ggplot() + geom_bar(data=outputs_plots,aes(x=rate_structure,y=value,fill=fee_passthrough),stat='identity',position='dodge',color="black")+
  facet_grid(cols=vars(rate_structure),rows=vars(variable),scales = "free")
p1


# do a before-after (above below) for total dollar bills and %ages of income
p2_data <- outputs[,c(5,9,13,14:16,27:29,30:32,43:45,49,50:52,63:65,66:68,79:81)]

# now average some columns 
p2_data$li_total_before <- rowMeans(p2_data[,c(4:6)])
p2_data$li_total_after  <- rowMeans(p2_data[,c(10:12)])

p2_data$hi_total_before <- rowMeans(p2_data[,c(7:9)])
p2_data$hi_total_after  <- rowMeans(p2_data[,c(13:15)])

p2_data$li_perc_before  <- rowMeans(p2_data[,c(16:19)])
p2_data$li_perc_after   <- rowMeans(p2_data[,c(23:25)])

p2_data$hi_perc_before <- rowMeans(p2_data[,c(20:22)])
p2_data$hi_perc_after  <- rowMeans(p2_data[,c(26:28)])

p2_melt <- melt(p2_data,id.vars = c("mitigation_decision","rate_structure","fee_passthrough"),measure.vars = c("li_total_before","li_total_after","hi_total_before","hi_total_after",
                                                                                             "li_perc_before","li_perc_after","hi_perc_before","hi_perc_after"))

# add some quick other labels
p2_melt$income <- "low"
p2_melt$time <- "before"
p2_melt$measure <- "total"

# label highs
p2_melt$income[grepl("hi",p2_melt$variable)] <- "high"
p2_melt$time[grepl("after",p2_melt$variable)] <- "after"
p2_melt$measure[grepl("perc",p2_melt$variable)] <- "perc"

# set the factor orders 
p2_melt$income <- factor(p2_melt$income,levels=c("low","high"))
p2_melt$time <- factor(p2_melt$time,levels=c("before","after"))
p2_melt$measure <- factor(p2_melt$measure,levels=c("total","perc"))

p2_melt$rate_structure <- factor(p2_melt$rate_structure,levels=c("ibp","fixed","one_tier","dpb"))


p2 <- ggplot() + geom_bar(data=p2_melt[which(p2_melt$measure!="perc"),],aes(x=rate_structure,fill=fee_passthrough,y=value),stat='identity',position='dodge')+ 
  facet_grid(rows=vars(income),cols=vars(time))+labs(title="Yearly Cost of Water,  before and after drought w/ different rate structures")
p2

p3 <- ggplot() + geom_bar(data=p2_melt[which(p2_melt$measure=="perc"),],aes(x=rate_structure,fill=fee_passthrough,y=value),stat='identity',position='dodge')+ 
  facet_grid(rows=vars(income),cols=vars(time),scales = "free")+labs(title="Yearly Cost of Water AS %age OF INCOME,  before and after drought w/ different rate structures")
p3

write.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/analysis/sc/rate_analysis_plot_data.csv",x=p2_melt)
