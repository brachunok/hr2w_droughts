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

# cutoffs
cutoffs = np.array([0,5,15,25,35,50])

conservation_policies = pd.DataFrame(index= cutoffs)
conservation_policies['res_reductions']=np.array([0,5,16,27,38,52])
conservation_policies['other_reductions']=[0,5,15,25,35,50]

# read in the decisionmaking file
decisions = pd.read_csv(repo_home / 'data'/'santa_cruz'/'sc_decisions.csv')

# name the one we want 'conserve_stage'
decisions['conserve_stage']=decisions['baseline']

# threshold for making an economic decision
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
outputs = pd.DataFrame(columns=['Date','totalDemand','residentialDemand','otherDemand','surface','ground',
                               'level', 'deficit','release','restriction','trigger',
                               'conserveStatus','fixedCharge','tieredPrices','pedReduction','revenueLost'])

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


ut.set_fixed_charge(13.55)
ut.set_tier_prices([9.03,10.86,12.78,15.74])
ut.set_tiers([5,7,9,999999])# set an upper limit

#13.55 + 1(CCF) + 9.03(0-5CCF) + 10.86(6-7CCF) + 12.78(8-9CCF) + 15.74(CCF 10+ )

# initialize states
decision_trigger = False
conservation_fraction = 0
percentage_change_quantity = 0
res_reduction = 0
nonres_reduction = 0

# we do this for every month
for m in range(outputs['Date'].count()):
#for m in range(3):
    # set my date
    this_date = outputs['Date'].iloc[m]
    this_month = outputs['month'].iloc[m]

    # read in my baseline inputs for residential and non-residential demands
    this_baseline = baseline_demand['mean'].loc[baseline_demand['reporting_month'].eq(this_month)][this_month-1]
    this_other= other_demand['commercial_industrial'].loc[baseline_demand['reporting_month'].eq(this_month)][this_month-1]

    # udpate residential demand if it needs to be based on what 'demand_changes' says
    # the demand changes column will perminantly alter demand to 1-(demandchanges*100) *old demand
    if decisions['demand_changes'].iloc[m]!=0:
        print("demand changed to ",(1-decisions['demand_changes'].iloc[m]))
        baseline_demand['mean'] = baseline_demand['mean']*(1-decisions['demand_changes'].iloc[m])

    # read from input file to determine what decisions we are making

    if (decisions['conserve_stage'].iloc[m]!=decisions['conserve_stage'].iloc[m-1]):

        this_decision = decisions['conserve_stage'].iloc[m]
        this_conservation = conservation_policies.iloc[this_decision]

        # the conservaation happens in the 'conservation_fraction' value
        outputs.loc[outputs.index==m,'conserveStatus'] = this_conservation['res_reductions']
        res_reduction = this_conservation['res_reductions']/100
        nonres_reduction = this_conservation['other_reductions']/100

        # now calculate lost revenue from the reduction
        #TODO: also calculate a cost here as well for a larger project
        # calculate the volume needed
        volume_reduced = (res_reduction)*(this_baseline)#+((1-nonres_reduction)*this_other)
        cost_of_conservation = ut.calculate_cost_of_conservation_EVEN_by_household(volume_reduced, city,baseline_demand)
        print(cost_of_conservation)
        additional_monthly_cost = cost_of_conservation

        outputs.loc[outputs.index==m,'revenueLost']=additional_monthly_cost

        # update my rate structure
        household_cost = additional_monthly_cost/np.multiply(city.household_sizes,city.counts).sum()
        # ^^ this is costs per month

        # pass costs on in the fixed cost for right now
        ut.set_fixed_charge(ut.fixed_charge+ household_cost)


    # regardless of what we've done, record the bill structure
    outputs.loc[outputs.index==m,'fixedCharge']=ut.fixed_charge
    outputs.loc[outputs.index==m,'conserveStatus'] = res_reduction


    if outputs['fixedCharge'].iloc[m] !=outputs['fixedCharge'].iloc[m-1] and m> 0:

        # this means we updated the fixed charge above
        # so we have to update the demand based on that
        old_price = outputs['fixedCharge'].iloc[m-1]
        new_price = outputs['fixedCharge'].iloc[m]

        percentage_change_price = (new_price-old_price)/old_price

        percentage_change_quantity = PED*percentage_change_price

    # now make adjustments based on conservation, and price changes
    adjusted_baseline = this_baseline*(1-res_reduction)*(1-percentage_change_quantity)

    # calculate my other demand
    adjusted_other = this_other*(1-nonres_reduction)

    outputs.loc[outputs.index==m,'pedReduction'] = percentage_change_quantity
    ut.demand = city.get_utility_demand(adjusted_baseline)/1000000 + adjusted_other
    # this is setting the utility demand in MG

    # write the demand
    outputs.loc[outputs.index==m,'totalDemand'] = ut.demand
    outputs.loc[outputs.index==m,'residentialDemand'] = city.get_utility_demand(this_baseline)/1000000
    outputs.loc[outputs.index==m,'otherDemand'] = this_other

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

    # reservoir inflows
    ll_inflows = inflows[['feltonDiversions','newellInflow']].loc[inflows['date'].eq(this_date)].sum(axis=1)
    ll_inflows = ll_inflows.iloc[0]


    #TODO: update to make sure a deficit is passed on

    # udpate groundwater flow
    this_ground = 10
    # TODO: update

    # record groundwater
    outputs.loc[outputs.index==m,'ground'] = this_ground #this_ground.to_numpy()[0]

    # calculate my surface water deficit to determine how much we can draw from the reservoir
    # this is demand- surface-ground
    surface_deficit = ut.demand - this_ground - surface_inflows.iloc[0]

    # the policy (keep 87% full by April) means we can draw down 70mg from the res each month
    # drawdown is the minimum of (surface deficit) and (res_inflows + 70mg)
    # if the drawdown is above that, we will conserve, and move the conservation status to whatever of the 4 stages
    # will meet the demand change

    # reservoir is untouched unless we need it (ie, surface deficit > 0)
    ll_demand = 0

    if (surface_deficit > 0):
        # this means we have to do some sort of drawdown

        drawdown = min(surface_deficit,ll_inflows+70)

        if surface_deficit < ll_inflows + 70:
            drawdown = surface_deficit
            ll_demand = drawdown



        else:
            # we need more than we have
            drawdown = ll_inflows + 70
            ll_demand = drawdown

# =============================================================================
#             if this_month ==4:
#                 # if it's april, adjust all the conservation values
#                 # figure out how much we ahve to drawdown
#                 conserve_volume = surface_deficit - drawdown
#                 conservation_fraction = conserve_volume/2473
#
#                 print(conservation_fraction)
#                 decision_trigger = True
#                 conservation_trigger = True
#
#                 #cutoffs = [5,15,25,35,50]
#                 # res_reductions = [5,16,27,38,52]
#                 # other_reductions = [5,15,25,35,50]
#
#                 this_stage=0
#                 for f in range(0,cutoffs.size):
#                     if cutoffs[f]>=conservation_fraction*100:
#                         this_stage = cutoffs[f]
#                         res_reduction = res_reductions[f]*.01
#                         nonres_reduction = other_reductions[f]*.01
#                         break
#
# =============================================================================
                # figure out which reductions we should be passing on
                # write all these to the conesrvation triggers for next month


    # TODO: writeamount to outputs

    # simulate them going into the reservoir
    release =reservoir.make_fixed_environmental_release(ll_inflows,ll_demand)

    #TODO: evaporation

    # record release and volume
    outputs.loc[outputs.index==m,'level'] = reservoir.volume
    outputs.loc[outputs.index==m,'release'] = release

    # calculate and record any deficits

    this_deficit = max(ut.demand-ll_demand-this_ground-surface_inflows.iloc[0],0)
    outputs.loc[outputs.index==m,'deficit'] = this_deficit




# record the outputs
outputs.to_csv(repo_home / 'outputs'/'santa_cruz'/"outputs.csv")
hh_demand.to_csv(repo_home / 'outputs'/'santa_cruz'/ "hh_demand.csv")
hh_bills.to_csv(repo_home / 'outputs'/'santa_cruz'/"hh_bills.csv")
