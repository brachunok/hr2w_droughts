# discrete (weekly) simulation
# this is the general overview of how this works

#(1) read in all the data and import modules etc..
#(2) define all of my classes which are
# .   inflows, reservoirs, demands, transfers
# (3) define all of my connections, capacities etc.
#(4) then for each time step do the following

    # get the next inflow step
    #  update the reservoir levels
    # then see if I can meet my demand.
    # if I can, do so, and record everything
    # if I cant' start looking through my priorities

# starting a todo list
# (1) move the income elasticity out of the function and let it be specified
    # based on how groups change vs MHI (ie a 5% reduction for a group below)
    # MHI reduces their demand by X---> DONE


# (4) see how conservation was calculated --> was it a reduction by person
    # or by residence?

# (5) make sure my income bins all line up. Both here in the model,
#       and in the household size calculation

# (6) make sure i'm getting the right household data and that the sumprod of counts + sizes is
#       close to total population

# notes Global water intelligence, Bluefield research, AWWA
#    https://energyconsumersaustralia.worldsecuresystems.com/grants/508/AP-508-Impacts-Consequences-Low-Income-Households-Rising-Energy-Bills.pdf

# sacramento data used currently to the best knowledge.
# from here: https://www.cityofsacramento.org/~/media/Corporate/Files/DOU/2015%20UWMP%20June%202016Appendices.pdf

# packages
import os, sys
#sys.path.append("/Users/brachuno/Documents/__college/reseach_stuff/water-equity/code")

import datetime
import math
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from pathlib import Path
from wrtools import *
# if wrtools isn't found, make sure the code dir is inthe path

# define our relative path base
repo_home = Path("./").absolute().parents[1]
# ^^ may not work if you are runnign interactively. Should work if you run the script all as once
# i.e. click 'run' vs running line by line


# threshold for making an economic decision
RES_THRESHOLD = 0.25
YED = 0.43 # YED and PED from dalhausen 2003, they do a meta-analysis, we use
PED= 0.41  # the mean values of their analysis

# read in the input data
input_data = pd.read_csv(repo_home / 'data'/'sacramento'/'sacramento_inputs_drought.csv')
# ALL DATA IN INPUT DATA IS MILLIONGALLONS/MONTH

# convert the input data date
input_data['date'] = pd.to_datetime(input_data['date'],format="%Y-%m-%d", errors = 'coerce')

# get my baseline demand
baseline_demand = pd.read_csv(repo_home / 'data'/'sacramento'/"city_of_sacramento_processed.csv")
# units are GPCD`

# initialize my dataframe of outputs
outputs = pd.DataFrame(columns=['Date','baselineDemand','','surface','ground',
                               'level', 'deficit','release','restriction','trigger',
                               'conserveStatus','buildStatus','fixedCharge','tieredPrices','pedReduction'])

outputs['Date'] = pd.to_datetime(input_data['date'],format="%Y-%m-%d", errors = 'coerce')
outputs['month'] = pd.DatetimeIndex(outputs['Date']).month

# n is a row index that records how many weeks we've done this for
#(needed because the looping below is by month, then by week )
n = -1

# okay let's go through our simulation
# ending date currently is defined by just a 'number of rows'
# each row in the data is a week

LENGTH = len(outputs['Date'])
# so now trim our outputs down to LENGTH long to define our simulation runtime
outputs = outputs.loc[:LENGTH]

# make convenient dataframes of all my water sources
inflows = input_data[['date','surface']]
groundwaters = input_data[['date','groundwater']]

# initialize my flows
inflow = Inflow(inflows['surface'].iloc[0])

# do the same for groudwater
groundwater = GroundWater(groundwaters['groundwater'].iloc[0])

# other demand
other_demand = input_data[['date','other']]

# now for my cities
city = City(1,YED)#

#change reservoir size and set volume
#reservoir = Reservoir(52) # unit is MG
#reservoir.set_volume(52/2)

reservoir = Reservoir(30000) # this is a rough estimate bsed on
reservoir.set_volume(30000)  # how much


# define a demand function
# define all my demand data
# we are going to define a series of bins,the ith location
# in each vector has the population, househodl size, income, and leakage
# volumes for eachpopulation

# total number of people living in each household size (I think?)
populations = [22270,39529,43426,61799,97988,76275,96847,47323]

# sizes from the get_acs_data script
household_sizes = [1.23,1.93,2.53,3.11,3.42,3.86,9.47,4.28]
#populations = [ 7652. , 13583., 14923., 21236., 33672., 26211., 33280., 16262.]
populations = np.divide(populations,household_sizes)

income = [12500,17250,30000,42500,62500,87500,125000,175000]
##household_sizes = [1,1,1,1,1,1,1,1]
#household_sizes = [2.91,2.91,2.91,2.91,2.91,2.91,2.91,2.91]
household_sizes = [1.23,1.93,2.53,3.11,3.42,3.86,9.47,4.28]
# make the houshold demand dataframe
hh_demand = pd.DataFrame(columns=income)
hh_bills  = pd.DataFrame(columns=income)

#TODO: update these
# sizes are estimated based on a simple regression
# using the data provided in the income historical tables.
# because the ACS doesn't report those specifically

leakages = [.1,.1,.1,.1,.1,.1,.1,.1]

# now add these to the city object

city.set_bins(populations,income,household_sizes,leakages)

# fInitial demand is 158 GPCD from table 5-3 in UWMP
# daily demands

# define a utility
ut = Utility("utility",0.03)
ut.add_option("res1",50,60,60,1000000)
#ut.add_option("res2",60,60,60,2000000)
#ut.add_option("res3",80,30,60,2100000)

# add a drought option in california, the drought was a
# 25% reduction which lasted a year

ut.set_fixed_charge(29.52)
ut.set_tier_prices([1.2055])
ut.set_tiers([999999])# set an upper limit

# initialize states
decision_trigger = False
conservation_fraction = 0
percentage_change_quantity = 0

# we do this for every month
#for m in range(outputs['Date'].count()):
for m in range(3):
    # set my date
    this_date = outputs['Date'].iloc[m]
    this_month = outputs['month'].iloc[m]

    if decision_trigger==True:
        # if we are making a decision...
        if conservation_trigger==True:
            # our decision is conservation

            # nothing happens right now except we record it.
            # the conservaation happens in the 'conservation_fraction' value
            outputs['conserveStatus'].iloc[m] = conservation_fraction
            #*make it such that only one decision can happen
            #* update billing based on costs
            #*
        else:
            # our decision is build
            option = ut.check_pending(m)

            # if the option is false, do nothing we must move on
            if option !=False:
                # if i'ts not false, then it's a row from options
                reservoir.capacity = reservoir.capacity + option['capacity']
                outputs['buildStatus'] = "built!"
            else:
                # record that we have an active decision and record it
                outputs['buildStatus'].iloc[m] = "buildilng"

        # update my rate structure
        household_cost = additional_monthly_cost/sum(city.counts)
        # ^^ this is costs per month

        # pass costs on in the fixed cost for right now
        ut.set_fixed_charge(ut.fixed_charge+ household_cost)

        # pass costs on in the volumetric rate
        # divide the total cost we have to recoup by the total city demand
        # to understand how much money needs to be returned per gallon
        # which then needs to be converted to $/ccf and added to the rate
        #unit_cost_increase = additional_monthly_cost/city.get_utility_demand(baseline_demand['mean'].mean())*748
        #ut.set_tier_prices([ut.prices[0]+unit_cost_increase])

    # regardless of what we've done, record the bill structure
    outputs.loc[outputs.index==m,'fixedCharge']=ut.fixed_charge
    outputs.loc[outputs.index==m,'conserveStatus'] = conservation_fraction

    # if last month's price is this month's price, do nothing

    if m > 0:
        if outputs['fixedCharge'].iloc[m] !=outputs['fixedCharge'].iloc[m-1]:
            print('inloop')
            # this means we updated the fixed charge above
            # so we have to update the demand based on that
            old_price = outputs['fixedCharge'].iloc[m-1]
            new_price = outputs['fixedCharge'].iloc[m]

            percentage_change_price = (new_price-old_price)/old_price

            percentage_change_quantity = PED*percentage_change_price


    # get_utility _demand outputs all the values in
    # gallons so divide by a million to get million-gallons

    this_baseline = baseline_demand['mean'].loc[baseline_demand['reporting_month'].eq(this_month)][this_month-1]*(1-conservation_fraction)*(1-percentage_change_quantity)
    #^^ This is very hacky but it works. The index at the end someone needs to always get the ith value.
    # not just the first. Maybe fixibale by re-indexing but this shoud be airtight for now.

    outputs.loc[outputs.index==m,'pedReduction'] = percentage_change_quantity
    ut.demand = city.get_utility_demand(this_baseline)/1000000 + other_demand['other'].iloc[m]
    # this is setting the utility demand in MG

    # write the demand
    outputs.loc[outputs.index==m,'baselineDemand'] = ut.demand

    #Record demand for an average house from each class
    # update demand
    class_demands = city.get_total_household_demands(this_baseline)
    hh_demand.loc[m] = class_demands

    # get the bills for each
    class_bills = []
    for c in class_demands:
        class_bills.append(ut.get_bill(c/748))

    hh_bills.loc[m] = class_bills

    # for this particular month, here are our inflows
    this_inflow = inflows['surface'].loc[inflows['date'].eq(this_date)]
    # INFLOWS AND SURFACE WATER NEED TO BE MONTHLY TOTALS

    # record inflows
    outputs.loc[outputs.index==m,'surface']= this_inflow.to_numpy()[0]

    # udpate groundwater flow
    this_ground = groundwaters['groundwater'].loc[groundwaters['date'].eq(this_date)]
    # record groundwater
    outputs.loc[outputs.index==m,'ground'] = this_ground.to_numpy()[0]

    # simulate them going into the reservoir
    release =reservoir.make_sop(this_inflow,ut.demand-this_ground)
    #release = release.to_numpy()[0]

    # and any reservoir adjustemnts we have to make

    # record release and volume
    outputs.loc[outputs.index==m,'level'] = reservoir.volume
    outputs.loc[outputs.index==m,'release'] = release

    # calculate and record any shortages
    outputs.loc[outputs.index==m,'deficit'] = max(ut.demand-release-this_ground.to_numpy()[0],0)



    # calculate minimum res level for making decisions
    if outputs['trigger'].eq("YES").any() == False:
        # We do this^^^ because we only want to make one decision
        # and watch how it plays out
        if (((reservoir.volume/reservoir.capacity)<=RES_THRESHOLD)) :
            decision_trigger = True
        else:
            decision_trigger = False
            conservation_fraction=0 # just make sure this is 0



        # make the decisions at the end of the simulated hydrology
        # outside the for loop, check if we have to make any decisions
        if decision_trigger == True:

            # okay, so here we have incited the trigger
            # now we have to figure out what decisions to make...
            #TODO: record the decision and make sure we
            outputs['trigger'].iloc[m]= "YES"
            needed_deficit = max(ut.demand-release,0)
            cheapest_index = ut.get_cheapest_monthly(needed_deficit)
            if cheapest_index==-1:
                cheapest_cost = 10000000000000
            else:
                cheapest_cost = ut.monthly_costs[cheapest_index]

            # see how much conservation would cost assuming we are just going into stage 1 (from UWMP)
            s1_need = .25*(inflows['surface'].iloc[m]+groundwaters['groundwater'].iloc[m])
            s1_cost = ut.calculate_cost_of_conservation_EVEN_by_household(s1_need,city,baseline_demand)

            # make some sort of decision as to what to do
            # currently decide between a conservation of cost s1_cost and the cheapest option
            conservation_trigger = False

            if s1_cost<=cheapest_cost:
                # this is if we conserve
                conservation_trigger = True
                conservation_fraction = 0.25 # from UWMP, will need to be expanded
                additional_monthly_cost = s1_cost
            else:
                ut.choose_option(cheapest_index, m)
                conservation_fraction=0
                additional_monthly_cost = cheapest_cost
            # decide whether to conserve or build.
    else:
        decision_trigger = False
# record the outputs
outputs.to_csv(repo_home / 'outputs'/'sacramento'/"outputs.csv")
hh_demand.to_csv(repo_home / 'outputs'/'sacramento'/ "hh_demand.csv")
hh_bills.to_csv(repo_home / 'outputs'/'sacramento'/"hh_bills.csv")
