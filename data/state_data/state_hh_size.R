# household size changes.... again
# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")
setwd("~/Documents/__college/reseach_stuff/hr2w_droughts/data/state_data")
ddi <- read_ipums_ddi("usa_00005.xml")
df <- read_ipums_micro_list(ddi)
dfp  <- df$PERSON 
dfh <- df$HOUSEHOLD
#ipums_view(ddi)

# year: year (i picked this)
# serial: ipums sample for which the data was taken from. shoudl all be the same because data is ACS
# serial: household serial number from ipums
# boring stuff until...

# NUMPREC: the number of people under each income (here state and household )
# HHWT: number of households in the US represented by this sample. Multiply by this to get actual numbers.
#       #  documentation says it includes 2 decimals but it looks like those are dealt with 

# state fips is state, we just need 06 for california

# county fips can be used (eventually) to narrow it down even more 

# GQ is group quarters status. we want to get rid of 0s which are vacant places
# HHINCOME is household income in 2016 dollars 

#keep_columns <- c("NUMPREC","HHWT","GQ","STATEFIP","HHINCOME","COUNTYFIP")
#df <- df_raw[,keep_columns]

# now filter out the unnecessary data

# FOR THESE TO WORK, ALL THE FOLLOWING DFs NEED TO BE DFHs 
#df <- dfh
#df <- df[which(df$STATEFIP==6),] # only california

# remove 9999999s from income which are NAs 
df <- df[which(df$HHINCOME!=9999999),]
df <- df[which(df$HHINCOME>=0),]

# so now using all of these points, let's actually calculate it for each bins
income_cutoffs <- c(10000,15000,20000,25000,30000,35000,40000,45000,50000,60000,75000,100000,125000,150000,200000)
income_names <- c("7.5","12.5","17.5","22.5","27.5","32.5","37.5","42.5","47.5","55","67.5","87.5","112.5","137.5","175","250")
df$bin <- NA
for(i in 1:length(income_cutoffs)){
  this_cutoff <- income_cutoffs[i]
  df$bin[which((df$HHINCOME<=this_cutoff)&is.na(df$bin))  ] <- income_names[i]  
}
df$bin[which(is.na(df$bin))] <-"250"

# now aggregate mean household size by bin INCLUDING THE WEIGHTS (so you cant just aggregate )
weighted_average_size <- data.frame(income_names)
weighted_average_size$stateSize <- NA

for (i in 1:length(income_names)){
  # get the particular bin
  this_name <- income_names[i]
  this_df <- df[which(df$bin==this_name),]
  
  this_weighted_average <- sum(this_df$HHWT*this_df$NUMPREC)/sum(this_df$HHWT)
  weighted_average_size$stateSize[which(weighted_average_size$income_names==this_name)] <- this_weighted_average  
}

# do some exploring of the HH size data-------------------------------------------

# add bins to DFH

# so now using all of these points, let's actually calculate it for each bins
income_cutoffs <- c(10000,15000,20000,25000,30000,35000,40000,45000,50000,60000,75000,100000,125000,150000,200000)
income_names <- c("7.5","12.5","17.5","22.5","27.5","32.5","37.5","42.5","47.5","55","67.5","87.5","112.5","137.5","175","250")
dfh$bin <- NA
for(i in 1:length(income_cutoffs)){
  this_cutoff <- income_cutoffs[i]
  dfh$bin[which((dfh$HHINCOME<=this_cutoff)&is.na(dfh$bin))  ] <- income_names[i]  
}
dfh$bin[which(is.na(dfh$bin))] <-"250"



# calculate average valeus for all the households/people in the median and above median groups
# and also for the below poverty group 
this_state <- 06
this_fips <- 87 # santa cruz 

this_dfh <- dfh[which(dfh$COUNTYFIP==this_fips&dfh$STATEFIP==this_state),]
this_dfp <- dfp[which(dfp$SERIAL%in%this_dfh$SERIAL),]

this_dfp$bin <- NA
this_dfp$bin3 <- NA

# assign the bins from dfh to dfp
for (i in 1:nrow(this_dfp)){
  this_dfp$bin[i] <- this_dfh$bin[this_dfp$SERIAL[i]==this_dfh$SERIAL]  
}

# now assign 'bin3'
MHI = 61000
CPM = 35923

# find the incomes with poverty, belowMHI, above,MHI numbers
belowCPM <- paste0(income_names[1:6])
CPMtoMHI <- paste0(income_names[7:10])
overMHI <- paste0(income_names[10:16])

this_dfp$bin3[which(this_dfp$bin%in%belowCPM)] <- "BelowCPM"
this_dfp$bin3[which(this_dfp$bin%in%CPMtoMHI)] <- "CPMtoMHI"
this_dfp$bin3[which(this_dfp$bin%in%overMHI)] <- "overMHI"


# make income bins a factor in the right order
this_dfp$bin <- factor(this_dfp$bin, levels=income_names)
this_dfp$bin3 <- factor(this_dfp$bin3, levels=c("BelowCPM","CPMtoMHI","overMHI"))

# expand dfp based on weights 
library('hutils')
library('labelled')
library('ggplot2')
this_dfp_weighted <- weight2rows(this_dfp,weight.var = "PERWT")
# now make some plots of person characteristics by bin
# we need to account for the weights, so use ENmisc's wtd.boxplot to do so

boxplot(AGE~bin,data=this_dfp_weighted,main="Age by Income Class: Santa Cruz")
boxplot(AGE~bin3,data=this_dfp_weighted,main="Age by Income Class: Santa Cruz")

this_dfp_weighted$SEX <- to_factor(this_dfp_weighted$SEX)
plot(this_dfp_weighted$SEX~this_dfp_weighted$bin)
plot(this_dfp_weighted$SEX~this_dfp_weighted$bin3)

this_dfp_weighted$RACE <- to_factor(this_dfp_weighted$RACE)

# make a ggplot
ggplot(this_dfp_weighted,aes(fill=RACE,x=bin))+geom_bar(position="fill")+theme_bw()+scale_fill_brewer(palette="RdYlBu")
ggplot(this_dfp_weighted,aes(fill=RACE,x=bin3))+geom_bar(position="fill")+theme_bw()+scale_fill_brewer(palette="RdYlBu")


# same with hispanic origin
this_dfp_weighted$HISPAN <- to_factor(this_dfp_weighted$HISPAN)
ggplot(this_dfp_weighted,aes(fill=HISPAN,x=bin))+geom_bar(position="fill")+theme_bw()+scale_fill_brewer(palette="RdYlBu")
ggplot(this_dfp_weighted,aes(fill=HISPAN,x=bin3))+geom_bar(position="fill")+theme_bw()+scale_fill_brewer(palette="RdYlBu")

#  insurance
this_dfp_weighted$HCOVANY<- to_factor(this_dfp_weighted$HCOVANY)
ggplot(this_dfp_weighted,aes(fill=HCOVANY,x=bin))+geom_bar(position="fill")+theme_bw()+scale_fill_brewer(palette="RdYlBu")
ggplot(this_dfp_weighted,aes(fill=HCOVANY,x=bin3))+geom_bar(position="fill")+theme_bw()+scale_fill_brewer(palette="RdYlBu")


# VA listed isability
this_dfp_weighted$VETDISAB<- to_factor(this_dfp_weighted$VETDISAB)
ggplot(this_dfp_weighted,aes(fill=VETDISAB,x=bin))+geom_bar(position="fill")+theme_bw()+scale_fill_brewer(palette="RdYlBu")
ggplot(this_dfp_weighted,aes(fill=VETDISAB,x=bin3))+geom_bar(position="fill")+theme_bw()+scale_fill_brewer(palette="RdYlBu")

# make an aggregate disabiltiy measure
this_dfp_weighted$DIFFREM<- to_character(this_dfp_weighted$DIFFREM) #2 is disability "Has cognitive difficulty"
this_dfp_weighted$DIFFPHYS<- to_character(this_dfp_weighted$DIFFPHYS) # 2 is a physicaldisabilty "Has ambulatory difficulty" 
this_dfp_weighted$DIFFMOB<- to_character(this_dfp_weighted$DIFFMOB) # 2 is difficulty moving  "Has independent living difficulty"
this_dfp_weighted$DIFFCARE <- to_character(this_dfp_weighted$DIFFCARE) # 2 is difficulty  "YES" 
this_dfp_weighted$DIFFSENS<- to_character(this_dfp_weighted$DIFFSENS) # 2 is difficulty "Has vision or hearing difficulty"
this_dfp_weighted$DIFFEYE<- to_character(this_dfp_weighted$DIFFEYE) # 2 is difficulty moving "YES"
this_dfp_weighted$DIFFHEAR<- to_character(this_dfp_weighted$DIFFHEAR) # 2 is difficulty moving "YES"

# nowmake an aggregate disability column
this_dfp_weighted$aggregate_disability <- (this_dfp_weighted$DIFFREM=="Has cognitive difficulty")|(this_dfp_weighted$DIFFPHYS=="Has ambulatory difficulty")|(this_dfp_weighted$DIFFMOB=="Has independent living difficulty")|(this_dfp_weighted$DIFFCARE=="Yes")|(this_dfp_weighted$DIFFSENS=="Has vision or hearing difficulty")|(this_dfp_weighted$DIFFEYE=="Yes")|(this_dfp_weighted$DIFFHEAR=="Yes")
ggplot(this_dfp_weighted,aes(fill=aggregate_disability,x=bin))+geom_bar(position="fill")+theme_bw()+scale_fill_brewer(palette="RdYlBu")
ggplot(this_dfp_weighted,aes(fill=aggregate_disability,x=bin3))+geom_bar(position="fill")+theme_bw()+scale_fill_brewer(palette="RdYlBu")


# education 
this_dfp_weighted$SCHOOL <- to_factor(this_dfp_weighted$SCHOOL)
ggplot(this_dfp_weighted,aes(fill=SCHOOL,x=bin))+geom_bar(position="fill")+theme_bw()+scale_fill_brewer(palette="RdYlBu")
ggplot(this_dfp_weighted,aes(fill=SCHOOL,x=bin3))+geom_bar(position="fill")+theme_bw()+scale_fill_brewer(palette="RdYlBu")

this_dfp_weighted$EDUC <- to_factor(this_dfp_weighted$EDUC)
ggplot(this_dfp_weighted,aes(fill=EDUC,x=bin))+geom_bar(position="fill")+theme_bw()+scale_fill_brewer(palette="RdYlBu")
ggplot(this_dfp_weighted,aes(fill=EDUC,x=bin3))+geom_bar(position="fill")+theme_bw()+scale_fill_brewer(palette="RdYlBu")

# lets do a quick ANOVA to see which of these are statistically significant 

model1 <- glm(bin3~SEX+AGE+RACE+HISPAN+SCHOOL+EDUC+aggregate_disability,data=this_dfp_weighted,family="binomial")
#summary(model1)
#confint(model1)


# calculate income distributions for various counties and regions in California 
library('hutils')
library('labelled')
library('ggplot2')

ca_dfh <- dfh[which(dfh$STATEFIP==6),]
ca_dfh_weighted <- weight2rows(ca_dfh,weight.var = "HHWT")
ca_dfh_weighted <- ca_dfh_weighted[which(ca_dfh_weighted$HHINCOME!=9999999),]
ca_dfh_weighted <- ca_dfh_weighted[which(ca_dfh_weighted$HHINCOME>=0),]

# these look like okay estimates for now. based on comparison to the state distribution 
# and other metrics 
income_cutoffs <- c(10000,15000,20000,25000,30000,35000,40000,45000,50000,60000,75000,100000,125000,150000,200000)
income_names <- c("7.5","12.5","17.5","22.5","27.5","32.5","37.5","42.5","47.5","55","67.5","87.5","112.5","137.5","175","250")
ca_dfh_weighted$bin <- NA
for(i in 1:length(income_cutoffs)){
  this_cutoff <- income_cutoffs[i]
  ca_dfh_weighted$bin[which((ca_dfh_weighted$HHINCOME<=this_cutoff)&is.na(ca_dfh_weighted$bin))  ] <- income_names[i]  
}
ca_dfh_weighted$bin[which(is.na(ca_dfh_weighted$bin))] <-"250"

# now also assign a 5 bin
ca_dfh_weighted$bin5 <- NA
ca_dfh_weighted$bin5[which(ca_dfh_weighted$bin%in%c(income_names[1:3]))] <- "Deep Poverty"
ca_dfh_weighted$bin5[which(ca_dfh_weighted$bin%in%c(income_names[4:6]))] <- "Poverty"
ca_dfh_weighted$bin5[which(ca_dfh_weighted$bin%in%c(income_names[7:9]))] <- "Near Poverty"
ca_dfh_weighted$bin5[which(ca_dfh_weighted$bin%in%c(income_names[10:13]))] <- "Middle Class"
ca_dfh_weighted$bin5[which(ca_dfh_weighted$bin%in%c(income_names[14:16]))] <- "Upper Class"


ca_dfh_weighted$bin <- factor(ca_dfh_weighted$bin,labels=income_names)
ca_dfh_weighted$bin5 <- factor(ca_dfh_weighted$bin5,labels=c("Deep Poverty","Poverty","Near Poverty","Middle Class","Upper Class"))

# melt just bin and county data to try to do an elaborate faceting thing 
ca_dfh_weighted_melt <- ca_dfh_weighted[,c(11,19,20)]

# now make histograms
ca_dfh_weighted_melt_aggregated <- table(ca_dfh_weighted_melt$bin,ca_dfh_weighted_melt$COUNTYFIP)

  
for (j in 1:ncol(ca_dfh_weighted_melt_aggregated)){
  ca_dfh_weighted_melt_aggregated[,j] <- ca_dfh_weighted_melt_aggregated[,j]/sum(ca_dfh_weighted_melt_aggregated[,j])
}

# now melt so we can just make barcharts like plebs...
ca_dfh_weighted_melt_aggregated <- melt(ca_dfh_weighted_melt_aggregated)
#ca_dfh_weighted_melt_aggregated$Var1 <- factor(ca_dfh_weighted_melt_aggregated$Var1,levels =c("Deep Poverty","Poverty","Near Poverty","Middle Class","Upper Class"))

# get rid of fips 0 because idk what it means

ca_dfh_weighted_melt_aggregated <- ca_dfh_weighted_melt_aggregated[which(ca_dfh_weighted_melt_aggregated$Var2!=0),]
#ca_dfh_weighted_melt_aggregated <- ca_dfh_weighted_melt_aggregated[which(ca_dfh_weighted_melt_aggregated$Var2%in%c(37,53,67,19,59,85,87)),]

ggplot(ca_dfh_weighted_melt_aggregated,aes(x=Var1,y=value,group=Var2))+geom_smooth()+theme_minimal()+ggtitle("Proportion of Each County's Population Making Each Income")+facet_grid(cols=vars(Var2))

# 