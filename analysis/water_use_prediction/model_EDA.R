# 

df <- read.csv("/Volumes/rachunok/research/hr2w_droughts_data/clean_dbs/merged_clean_unsafe_bills.csv")

fwb_account_counts <- table(df$account)
fwb_account_counts <- data.frame(fwb_account_counts)
colnames(fwb_account_counts) <- c("act","entries")

# for each entry, pull the value which matches from the fwb_account_counts table
fwb <- merge(df,fwb_account_counts,by.x="account",by.y="act")

# remove all bill lengths of 0
fwb<- fwb[which(fwb$bill_length!=0),]

# now sample only to those with a bunch of entries 
fwb <- fwb[which(fwb$entries>145),]

# normalize by bill length
fwb$normtotwuse <- fwb$normtotwuse/fwb$bill_length

# normalize by units
fwb$normtotwuse <- fwb$normtotwuse/fwb$num_units

# also remove the way-high outliers 
upper_lim <- quantile(fwb$normtotwuse,probs = 0.999)
fwb <- fwb[which(fwb$normtotwuse<=upper_lim),]

fwb$edate <- as.Date(fwb$edate,format="%m/%d/%Y")

# take a random sample and start model building 
library(randomForest)
library(VSURF)

# start with coviriates to test
covs <- colnames(fwb)[c(5:90,95,96,97,105,107)]
resp <- log(fwb$normtotwuse)
rf_df <- cbind(fwb[,which(colnames(fwb)%in%covs)],resp)

# remove infs 
rf_df <- rf_df[which(!is.infinite(rf_df$resp)),]

# convert to factors
rf_df$restype <- factor(rf_df$restype)
rf_df$status <- factor(rf_df$status)

set.seed(10)
samples <- sample(c(1:nrow(rf_df)),size = 10000,replace = FALSE)
rf_df_sample <- rf_df[samples,]
sample_covs <- rf_df_sample[,which(colnames(rf_df_sample)%in%covs)]
sample_resp <- rf_df_sample$resp

# find the NAs 
for(i in 1:ncol(rf_df_sample)){
  print(colnames(rf_df_sample)[i])
  print(table(is.na(rf_df_sample[,i])))
}

# let's impute them?
rf_df_sample_imputed <- rfImpute(resp~.,data=rf_df_sample)

sample_covs <- rf_df_sample_imputed[,which(colnames(rf_df_sample)%in%covs)]
sample_resp <- rf_df_sample_imputed$resp

m1 <- randomForest(resp~.,data=rf_df_sample_imputed,ntree=1000,mtry=100)
#m1_tuned <- tuneRF(x = sample_covs,y=sample_resp,doBest = T)


v_imp_1 <- varImpPlot(m1)

partialPlot(m1,pred.data=rf_df_sample_imputed,x.var="num_units")
partialPlot(m1,pred.data=rf_df_sample_imputed,x.var="emon")
partialPlot(m1,pred.data=rf_df_sample_imputed,x.var="bill_length")


# use VSURF or variable selection on the smaller subset to pick the variables
# we include, then add more data
vsurf_output <- VSURF(sample_covs,sample_resp,ntree=500,mtry=50)

#vsurf output is what I want on Monday morning.