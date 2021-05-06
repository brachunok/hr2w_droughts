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
input_data = pd.read_csv(repo_home / 'data'/'santa_cruz'/'sc_input_data.csv')
# ALL DATA IN INPUT DATA IS MILLIONGALLONS/MONTH

# convert the input data date
input_data['date'] = pd.to_datetime(input_data['date'],format="%Y-%m-%d", errors = 'coerce')

# get my baseline demand
baseline_demand = pd.read_csv(repo_home / 'data'/'santa_cruz'/'rcpgd_sc.csv')
baseline_demand.columns = ["reporting_month","mean"]
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
inflows = input_data[['date','northCoast','taitStreet','newellInflow','feltonDiversions']]
groundwaters = input_data['date']
# TODO, update groundwater

# initialize my flows
#inflow = Inflow(inflows['surface'].iloc[0])

# do the same for groudwater
#groundwater = GroundWater(groundwaters['groundwater'].iloc[0])

#TOOD: update groundwater


# other demand
other_demand = pd.read_csv(repo_home / 'data'/'santa_cruz'/'sc_monthly_non_res_demand.csv')
# todo: update temporal other demand

# now for my cities
city = City(1,YED)#

#change reservoir size and set volume
reservoir = Reservoir(2800,20) # Loch Lomond. Capacity in MG and monthly env release in MG
reservoir.set_volume(2800)


# total number of people living in each household size (
populations = [2365,1648,1456,1285,1424,1027,1018,1077,874,1931,2352,4064,3110,2091,3263,4895]

# sizes from the get_acs_data script
household_sizes = [1,3,3,3,1,1,1.67,1.4,1,1.57,1.86,3.17,3,3.57,4.58,5.38]

#populations = [ 7652. , 13583., 14923., 21236., 33672., 26211., 33280., 16262.]
#populations = np.divide(populations,household_sizes)

income = [7500,12500,17500,22500,27500,32500,37500,42500,47500,55000,67500,87500,112500,137500,175000,250000]


# make the houshold demand dataframe
hh_demand = pd.DataFrame(columns=income)
hh_bills  = pd.DataFrame(columns=income)

leakages = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]

# now add these to the city object

city.set_bins(populations,income,household_sizes,leakages)


# define a utility
ut = Utility("utility",0.03)
ut.add_option("res1",50,60,60,1000000)
#ut.add_option("res2",60,60,60,2000000)
#ut.add_option("res3",80,30,60,2100000)

# add a drought option in california, the drought was a
# 25% reduction which lasted a year

ut.set_fixed_charge(13.55)
ut.set_tier_prices([9.03,10.86,12.78,15.74])
ut.set_tiers([5,7,9,10,999999])# set an upper limit

#13.55 + 1(CCF) + 9.03(0-5CCF) + 10.86(6-7CCF) + 12.78(8-9CCF) + 15.74(CCF 10+ )

# initialize states
decision_trigger = False
conservation_fraction = 0
percentage_change_quantity = 0


#TODO: i'm here. figure out what santa cruz did and model it
# we do this for every month
for m in range(outputs['Date'].count()):
#for m in range(3):
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

    # calculate my other demand
    this_other= other_demand['commercial_industrial'].loc[baseline_demand['reporting_month'].eq(this_month)][this_month-1]

    outputs.loc[outputs.index==m,'pedReduction'] = percentage_change_quantity
    ut.demand = city.get_utility_demand(this_baseline)/1000000 + this_other
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
    #this_inflow = inflows['surface'].loc[inflows['date'].eq(this_date)]
    surface_inflows = inflows[['northCoast','taitStreet']].loc[inflows['date'].eq(this_date)].sum(axis=1)
    # INFLOWS AND SURFACE WATER NEED TO BE MONTHLY TOTALS

    # record inflows
    outputs.loc[outputs.index==m,'surface']= surface_inflows.to_numpy()[0]

    # udpate groundwater flow
    this_ground = 10
    # TODO: update

    # record groundwater
    outputs.loc[outputs.index==m,'ground'] = this_ground #this_ground.to_numpy()[0]

    # calculate how much is going into the reservoir
    res_inflows = inflows[['feltonDiversions','newellInflow']].loc[inflows['date'].eq(this_date)].sum(axis=1)

    #TOOD: write this amount to outputs

    #TODO
    # calculate how much we are withdrawing
    res_demand = input_data['newellUsed'].loc[input_data['date'].eq(this_date)]
    # TODO: write this amount to outputs


    # simulate them going into the reservoir
    release =reservoir.make_fixed_environmental_release(res_inflows,res_demand)

    #TODO: evaporation

    # record release and volume
    outputs.loc[outputs.index==m,'level'] = reservoir.volume
    outputs.loc[outputs.index==m,'release'] = release

    # calculate and record any shortages
    outputs.loc[outputs.index==m,'deficit'] = max(ut.demand-res_demand-this_ground)# max(ut.demand-release-this_ground.to_numpy()[0],0)

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
outputs.to_csv(repo_home / 'outputs'/'santa_cruz'/"outputs.csv")
hh_demand.to_csv(repo_home / 'outputs'/'santa_cruz'/ "hh_demand.csv")
hh_bills.to_csv(repo_home / 'outputs'/'santa_cruz'/"hh_bills.csv")
