# process experiments

# read the experiment output files, and make a dataframe containing the parameters and all the values about the run

path = "~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments"

before_year= 2010
after_year = 2016

#
bills = list.files(path = path,pattern = "*bills*",full.names = T)
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
processed[,c(paste0("B",incomes),paste0("A",incomes))] <- NA

for ( i in 1:length(bills)){
  
  # process this specific bills file
  this_bill <- read.csv(bills[i])
  this_demand <- read.csv(usage[i])
  
  this_bill$date <- date_col
  this_demand$date <- date_col

  # For each file, I want (for now)
  # the total water bill in 2016 for each bin
  # the total water bill in 2010 for each bin
  
  this_bill_after <- this_bill[after_rows,]
  this_bill_before <- this_bill[before_rows,]
  
  total_bill_after <- colSums(this_bill_after[,-which(colnames(this_bill_after)%in%c("X","date"))])
  total_bill_before <- colSums(this_bill_before[,-which(colnames(this_bill_before)%in%c("X","date"))])
  
  names(total_bill_after) <- paste0("A",incomes)
  names(total_bill_before) <- paste0("B",incomes)
  
  processed[i,] <- cbind(parameter_list[i,],t(total_bill_before),t(total_bill_after))
  
}
processed <- processed[complete.cases(processed),]
