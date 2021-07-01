# storage-yield EDA
library(reservoir)
df <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/data/santa_cruz/sc_input_data.csv")
res_inflow <- df$newellInflow+df$feltonDiversions+df$taitStreet

df$year <- format(as.Date(df$date),"%Y")
# calculate the mean annual runoff 
mar_list <- aggregate(res_inflow,by=list(df$year),FUN=mean)
mar <- median(mar_list$x)

LEN = 100
sy_df <- data.frame(storage=numeric(LEN),yield=numeric(LEN))
sy_df$yield <- seq(from=1,to=3*mar,length.out=LEN)
present_yield <- yield(res_inflow,capacity = 2800,reliability = 1,plot=FALSE,)

# for changing yield values lets see what happens
for ( i in 1:nrow(sy_df)){
  
  # calculate the storage needed for this yield?
  this_storage <- storage(res_inflow,yield=sy_df$yield[i],reliability = 1,plot=FALSE)
  
  sy_df$storage[i] <-this_storage$Required_storage
}    

sy_df$storage_MAR <- sy_df$storage/mar
sy_df$yield_MAR <- sy_df$yield/mar

plot(yield_MAR~storage_MAR,data=sy_df)
points(x=sy_df$storage_MAR[56],y=sy_df$yield_MAR[56],col="red",cex=2,pch=20)
