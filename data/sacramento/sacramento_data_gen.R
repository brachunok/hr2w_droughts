# sacramento generator

# take in the static values, and write all the appropriate data files

# all in units of million gallons per month
groundwater <- 369.4
surface_water <-  1913
other_demand <- 902

# simulation parameters
LENGTH = 120 # length in months
START = as.Date("2020-01-01")

# drought parameters: TBD
drought_start = 30  # months in that we start
drought_ramp_period = 12 # length of time to get the max
drought_duration = 40 # time we stay at the max
drought_intensity_max = 0.25

# where do we save it
base_path <- "~/Documents/__college/reseach_stuff/water-equity/data/"

# TOUCH NOTHING BELOW THIS LINE UNLESS YOU ARE CHANGING FUNCTIONALITY-----------

# make date ranges
date_range <- seq(from=START,length.out=LENGTH,by="month")

# make Dataframe
input_data_df <- data.frame(date=date_range,surface=surface_water,groundwater= groundwater,other= other_demand)

# make my drought perturbations

adjustments = seq(from =1, to =1,length.out=LENGTH)
adjustments[c(drought_start:(drought_start+drought_ramp_period))] <- 1- drought_intensity_max/drought_ramp_period*c(1:drought_ramp_period)

# now increase the duration to the full length
adjustments[c((drought_start+drought_ramp_period):(drought_start+drought_ramp_period+drought_duration))] <- 1-drought_intensity_max

input_data_df$surface <- surface_water*adjustments

# write to the appropriate files
write.csv(file = paste0(base_path,"sacramento_inputs_drought.csv"),input_data_df)
print(paste0("writing to.. ",base_path,"sacramento_inputs_drought.csv"))

      