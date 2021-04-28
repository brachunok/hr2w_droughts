# what all the files mean/do (at least the important ones )

In the general workflow order...

sacramento_district_tracts are the census tracts which overlap with the specific water
utility we are modeling. (This is figured out with GIS outside the repo workflow)


get_acs_data pulls the household income and household size data for the tracts in 
sacramento_district_tracts

rgpcd_state is the residential gallons per capita per day of water used for the state.
We create a baseline as an average of the monthly water use during 2018,19,20
Presently this is all done in excel like a filthy casual.

sacramento_data_gen takes in the valeus from UWMP and creates the output files
sacramento_inputs_drought and the (coming soon) sacramento_inputs_baseline.
Those CSVs are the input data for our model

california_2018_MHI_by_size.Rdata is the midly-pre-processed rdata file of 
the MHI/size relationship from the ACS for california in 2018, so we don't have 
to re-download census data every time 
net