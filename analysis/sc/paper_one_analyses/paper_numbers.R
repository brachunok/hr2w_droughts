# numbers which go into the paper in "Househodl Affordability Differs from utility cost"
#
library("scales")
library("ggsci")
library(cowplot)
library('ggpattern')
library('ggrepel')

options(scipen=999)
load("~/Documents/__college/reseach_stuff/hr2w_droughts/analysis/processed_and_binned_bill_data_plot_responses.Rdata")


outputs <- outputs[which(outputs$price_elasticity==0.35),]


outputs <- outputs[which(outputs$cpe_squared==0.0),]
outputs <- outputs[which(outputs$income_elasticity==0.15),]

# 
outputs.list <- list.files("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/review_responses/",pattern = "outputs*",full.names = T)
outputs.names <- list.files("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/review_responses/",pattern = "outputs*",full.names = F)
#shrink down the outputs to the ones from the x column of 'outputs' and pull out the surcharges

all_outputs <- read.csv(outputs.list[1])
all_outputs$filename <- outputs.names[1]

for(f in 2:length(outputs.list)){
  this_outputs <- read.csv(outputs.list[f])
  this_outputs$filename <- outputs.names[f]
  
  # make a giant list and append this
  all_outputs <- rbind(all_outputs,this_outputs)
  
}

# shrink to just the ones with fielname x 
library('stringr')
numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 
all_outputs$file_numb <- as.numeric(numextract(all_outputs$filename))

# now shrink all_outputs down to just the ones in outputs$x
rows_we_want <- which(all_outputs$file_numb%in%outputs$X)
all_outputs <- all_outputs[rows_we_want,]

# this is all outputs for just the experiments we want
surcharges <- all_outputs$surchargeIncrease
surcharges <- surcharges[!is.na(surcharges)]
max(surcharges)

# now maximum bill decreases will come from the outputs with the greatest curtailment so let's figure that out

all_outputs$filename[which(all_outputs$conserveStatus==max(all_outputs$conserveStatus))]

# so we know it's 182. Let's pull that bill and look at it

max_curtail_bill <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/review_responses/182_hh_bills.csv")

li_diff <- max_curtail_bill$raw_12500-max_curtail_bill$demandonly_12500
hi_diff <- max_curtail_bill$raw_250000-max_curtail_bill$demandonly_250000

# look at bill changes for all cases (bummer)
bill.list <- list.files("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/review_responses/",pattern = "bill*",full.names = T)
bill.names <- list.files("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/review_responses/",pattern = "bill*",full.names = F)
bill.numbs <- numextract(bill.names)
bill.list <- bill.list[which(as.numeric(bill.numbs)%in%all_outputs$file_numb)]

# now loop through and make a giant thing of bills
all_bills <- read.csv(bill.list[1])

for(b in 2:length(bill.list)){
  this_bill <- read.csv(bill.list[b])
  
  all_bills <- rbind(all_bills,this_bill)
}


# calculate the differences
all_bills$x12500_increase <- all_bills$X12500-all_bills$raw_12500
all_bills$x7500_increase <- all_bills$X7500-all_bills$raw_7500
all_bills$x17500_increase <- all_bills$X17500-all_bills$raw_17500

# for high income
all_bills$x250000_increase <- all_bills$X250000 - all_bills$raw_250000

# get the affordability ratios
baseline_bill <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/review_responses/62_hh_bills.csv")
baseline_outputs <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/review_responses/62_outputs.csv")
bill_dates <- as.Date(baseline_outputs$Date)

average_li_pre_drought_bills <- rowMeans(baseline_bill[c(1:12),c("X7500","X12500","X17500")])
summary(average_li_pre_drought_bills)
summary(average_li_pre_drought_bills/(mean(c(7500,12500,17500))/12))*100

average_hi_pre_drought_bills <- rowMeans(baseline_bill[c(1:12),c("X137500","X175000","X250000")])
summary(average_hi_pre_drought_bills)
summary(average_hi_pre_drought_bills/(mean(c(137500,175000,250000))/12))*100

# now get the after bills
drought_rows <-which(format(bill_dates,"%Y")=="2014") 

average_li_drought_bills <- rowMeans(baseline_bill[c(64:75),c("X7500","X12500","X17500")])
summary(average_li_drought_bills)
summary(average_li_drought_bills/(mean(c(7500,12500,17500))/12))*100

average_hi_drought_bills <- rowMeans(baseline_bill[c(64:75),c("X137500","X175000","X250000")])
summary(average_hi_drought_bills)
summary(average_hi_drought_bills/(mean(c(137500,175000,250000))/12))*100
