# get ACS data per county for florida

library("tidycensus")
library('tidyverse')
library('stringr')
census_api_key("defee5ea305745f65c3c8fd02105ae354ff653b0",install = T)

#I want ------------------------------
# the joint distribution of median household income and
# household size for the geography of interest

# This is table B19019 
#https://censusreporter.org/tables/B19019/
# more data here: https://www.socialexplorer.com/data/ACS2017_5yr/metadata/?ds=ACS17_5yr&table=B19019
tables<- c("B19019")
acs_df2 <- get_acs(geography="block_group",year=2018,state="California",table=tables[1])

# save the data as an Rdata for future use
#save(acs_df,file = "./california_2018_MHI_by_size.Rdata")
load("./california_2018_MHI_by_size.Rdata")

# for now get just the census tracts we need 
tracts <- read.csv("./sacramento_district_tracts.csv")
# pad tracts GEOID with a 0
tracts$GEOID <- as.character(tracts$GEOID)
tracts$GEOID <- str_pad(tracts$GEOID,width=11,side = "left",pad = "0")
# check if they are in each
table(tracts$GEOID%in%acs_df$GEOID)
#^^^ if this doesnt' say all true, there's a problem

acs_df <- acs_df[which(acs_df$GEOID%in%tracts$GEOID),]

# get rid of the 001 because those are totals
acs_df <- acs_df[which(acs_df$variable!="B19019_001"),]

#acs_df <- acs_df[which(!is.na(acs_df$estimate)),]

# relate variable to household size
sizes <- data.frame(size=numeric(7),varible=character(7))
sizes$varible <- unique(acs_df$variable)
sizes$size <- c(1:7)

acs_df$size <- 0
for(i in 1:nrow(sizes)){
  acs_df$size[which(acs_df$variable==sizes$varible[i])] <- sizes$size[i]
}

# now aggregate over the census tracts. Mean of estimate (MHI) by size
acs_df_agg <- aggregate(acs_df$estimate,by=list(acs_df$size),FUN=median,na.rm=TRUE)

plot(acs_df$estimate~acs_df$size)
lines(acs_df_agg$x~acs_df_agg$Group.1)

plot(acs_df$size~acs_df$estimate)
tracts <- unique(acs_df$GEOID)

plot(estimate~size,data=acs_df[which(acs_df$GEOID==tracts[1]),],type="l")
for (tract in tracts){
  lines(estimate~size,data=acs_df[which(acs_df$GEOID==tract),],type="l")
}
# 002 is 1-person households 
# 003 is 2-person
# 003 is 3-person 
# ...
# 007 is 7-or-more-person

# divide into the bins we are using and calculate average size for each 
cutoffs <- c(15000,25000,35000,50000,75000,100000,150000,200000)

# add the bins to the dataframe
acs_df$income_bin <- FALSE
b = 1
for (income in cutoffs){
  acs_df$income_bin[which(acs_df$estimate<income&acs_df$income_bin==FALSE)] <- b
  b = b+1
}
# make all the 0s 8s to signify the highest class
acs_df$income_bin[which(acs_df$income_bin==0)] <- 8

# then make all the income NAs NA in the bin. the above code will make it 0 if it's a 
# above the highest class or if it's NA so differentiate them .
acs_df$income_bin[which(is.na(acs_df$estimate))] <- NA

# now get a mean household size by income bin 
acs_df_agg2 <- aggregate(acs_df$size,by=list(acs_df$income_bin), FUN=mean)


# get the MHI estimate (001) per tract
load("./california_2018_MHI_by_size.Rdata")
totals_df <- acs_df[which(acs_df$variable=="B19019_001"),]
plot(ecdf(totals_df$estimate))

