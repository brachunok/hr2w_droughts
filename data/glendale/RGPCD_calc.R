library(tidyverse)


setwd("/Users/ctenner/Desktop/Classes/Github/hr2w_droughts/data/glendale")
CA <- read.csv("CA_rgpcd_data.csv")

glen <- CA %>% 
  mutate(
    Month = strptime(reporting_month, format = "%m/%d/%y")
  ) %>% 
  filter(
    grepl("Glendale",supplier_name),
    Month > strptime("03/05/16", format = "%m/%d/%y")
  ) 