# process experiments
library(tidyr)

# read the experiment output files, and make a dataframe containing the parameters and all the values about the run
path = "../../outputs/santa_cruz/experiments/review_responses2/"

before_year= 2010
after_year = 2015

#
bills = list.files(path = path,pattern = "*bills*",full.names = T)
bills_filename = list.files(path = path,pattern = "*bills*",full.names = F)
usage = list.files(path = path,pattern= "*demand*",full.names=T)
outputs = list.files(path = path,pattern= "*output*",full.names=T)
parameter_list <- read.csv(paste0(path,"/parameter_list.csv"))

date_col <- as.Date(read.csv(outputs[1])$Date)
# define the thigns we will want to put into the calculations
incomes <- c(7500,12500,17500,22500,27500,32500,37500,42500,47500,55000,67500,87500,112500,137500,175000,250000)
deep_poverty <- paste0("X",incomes[1:3]) # 18000
poverty <- paste0("X",incomes[4:6])
near_poverty <- paste0("X",incomes[7:9])
middle_class <- paste0("X",incomes[10:13])
upper_class <-  paste0("X",incomes[14:16])

# get the rows for before and after
before_rows <- which(format(date_col,"%Y")==before_year)
after_rows  <- which(format(date_col,"%Y")==after_year)

# make a 'processed' file
processed <- parameter_list
processed[,c(paste0("B_total",incomes),paste0("A_total",incomes))] <- NA
b_total_cols <- which(grepl("B_total",colnames(processed)))
a_total_cols <- which(grepl("A_total",colnames(processed)))
processed$max_deficit <- NA
processed$total_deficit <- NA
processed$market_buy <- NA
processed$total_utility_cost <- NA

# the p columns are agerage %age of income
processed[,c(paste0("B_avg_perc_",incomes),paste0("A_avg_perc_",incomes))] <- NA
b_avg_perc_cols <- which(grepl("B_avg_perc",colnames(processed)))
a_avg_perc_cols <- which(grepl("A_avg_perc",colnames(processed)))

# also make columns for max %age of income
processed[,c(paste0("B_max_perc_",incomes),paste0("A_max_perc_",incomes))] <- NA
b_max_perc_cols <- which(grepl("B_max_perc",colnames(processed)))
a_max_perc_cols <- which(grepl("A_max_perc",colnames(processed)))

for ( i in 1:length(bills)){
  
  # process this specific bills file
  this_bill <- read.csv(bills[i])
  this_demand <- read.csv(usage[i])
  
  this_bill$date <- date_col
  this_demand$date <- date_col

  # also add the %age of income spent in the 'before' year
  this_bill_i <- this_bill
  
  for (j in 2:17){
    this_bill_i[,j] <- this_bill[,j]/(incomes[j-1]/12)
  }
  
  # For each file, I want (for now)
  # before (fixed at 2010)
  # and after ('the worst of the drought for the low income folks')
  
  # make after rows the one with the highest bill for the lowest income class
  # so now we have the income dataframe for each
  # find the year with the highest %age for the lowest income class
  max_year <- format(this_bill$date[which(this_bill_i$X7500==max(this_bill_i$X7500))[1]],'%Y')
  
  after_rows <- which(format(this_bill$date,"%Y")==max_year)
  after_rows_i = after_rows
  
  this_bill_after <- this_bill[after_rows,]
  this_bill_before <- this_bill[before_rows,]
  
  total_bill_after <- colSums(this_bill_after[,-which(colnames(this_bill_after)%in%c("X","date"))])
  total_bill_before <- colSums(this_bill_before[,-which(colnames(this_bill_before)%in%c("X","date"))])
  
  names(total_bill_after) <- paste0("A_total",incomes)
  names(total_bill_before) <- paste0("B_total",incomes)
  
  # pull out the specific number from the filename
  this_parameter_index <- readr::parse_number(bills_filename[i])
  # that's the row of 'parameter list' and processed we need to attach to
  this_row <- which(parameter_list$X==this_parameter_index)
  
  processed[this_row,a_total_cols] <- t(total_bill_after)
  processed[this_row,b_total_cols] <- t(total_bill_before)

    # get outputs
  this_outputs <- read.csv(outputs[i])
  processed$max_deficit[this_row]  <- max(this_outputs$deficit)
  processed$total_deficit[this_row] <- sum(this_outputs$deficit)
  
  # also label total utility cost and market buy
  processed$total_utility_cost[this_row] <- sum(this_outputs$monthlyCost,na.rm=T)
  processed$market_buy[this_row] <- sum(this_outputs$market_buy,na.rm=T)
  # so now we have the income dataframe for each
  # find the year with the highest %age for the lowest income class
  max_year <- format(this_bill$date[which(this_bill_i$X7500==max(this_bill_i$X7500))[1]],'%Y')
  
  after_rows_i <- which(format(this_bill$date,"%Y")==max_year)
  
  # now write affordatbility for this as the after year and the before year.
  # calculated as the max %-age of income 
  processed[this_row,b_avg_perc_cols] <- colSums(this_bill_i[before_rows,c(2:17)])/12
  processed[this_row,a_avg_perc_cols] <-colSums(this_bill_i[after_rows_i,c(2:17)])/12
  
  for (j in 1:length(b_max_perc_cols)){
    processed[this_row,b_max_perc_cols[j]] <- max(this_bill_i[before_rows,j+1])
    processed[this_row,a_max_perc_cols[j]] <- max(this_bill_i[after_rows_i,j+1])
  }
  
  if (floor(i/50)==i/50){
    print(i/length(bills)*100)
  }
 
}
#processed <- processed[complete.cases(processed),]
save(processed,file = "../../analysis/sc/processed_plot_responses2.Rdata")
 
