# packages
import os, sys
#sys.path.append("/Users/brachuno/Documents/__college/hr/water-equity/code")

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

#"L" is the lines we write to the outputs text file
today = datetime.datetime.now()
L = ["Date: ",today.strftime("%d/%m/%Y %H:%M:%S"),"/n"]

# cutoffs
cutoffs = np.array([0,5,15,25,35,50])

conservation_policies = pd.DataFrame(index= cutoffs)
conservation_policies['res_reductions']=np.array([0,5,16,27,38,52])
conservation_policies['other_reductions']=[0,5,15,25,35,50]

#conservation_policies['res_reductions']=np.array([0,5,12,27,38,52])
            #conservation_policies['other_reductions']=[0,5,12,25,35,50]

L.append(["Cutoffs: ",cutoffs])
L.append(["res reductions: ",conservation_policies['res_reductions']])

# read in the decisionmaking file
decision_file = repo_home / 'data'/'santa_cruz'/'sc_decisions.csv'
decisions = pd.read_csv(decision_file)

L.append(["Decision file: ", decision_file])

# name the one we want 'conserve_stage'
scenario = 'baseline'
#scenario = 'drought1'
L.append(["Scenario: ",scenario])
decisions['conserve_stage']=decisions[scenario]

# THIS IS HOW WE PICK THE SCENARIOS
output_folder='baseline'
#output_folder = 'drought_custom'
#output_folder='drought1'

L.append(['Output Folder: ',output_folder])

# threshold for making an economic decision
#YED = 0.43 # YED and PED from dalhausen 2003, they do a meta-analysis, we use
YED = 0.1 # .1 comes from wicham 'water affordability in the united states'
PED= 0.4  # the mean values of their analysis

L.append(["YED: ",YED])
L.append(["PED: ",PED])

# read in the input data
input_data = pd.read_csv(repo_home / 'data'/'santa_cruz'/'sc_input_data.csv')
# ALL DATA IN INPUT DATA IS MILLIONGALLONS/MONTH

# convert the input data date
input_data['date'] = pd.to_datetime(input_data['date'],format="%Y-%m-%d", errors = 'coerce')

# get my baseline demand
baseline_demand = pd.read_csv(repo_home / 'data'/'santa_cruz'/'rcpgd_sc.csv')
baseline_demand.columns = ["reporting_month","mean"]
# units are GPCD`

# scale up seasonality pattern so that residential demand matches
#baseline_demand['mean'] = baseline_demand['mean']*1.32

# initialize my dataframe of outputs
outputs = pd.DataFrame(columns=['Date','totalDemand','residentialDemand','otherDemand','surface','ground',
                               'level', 'deficit','release','restriction','trigger',
                               'conserveStatus','fixedCharge','tieredPrices','pedReduction','annualRevenueLost','monthlyVolumeReduced'])

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
groundwaters = input_data[['date','groundwater']]

# other demand
other_demand = pd.read_csv(repo_home / 'data'/'santa_cruz'/'sc_monthly_non_res_demand.csv')

# et
et = pd.read_csv(repo_home / 'data'/'santa_cruz'/'sc_monthly_et.csv')

# now for my cities
city = City(1,YED,61000.01)#

# TODO: need to set so that the dmeand is calculated based on a pre-specified MHI
# not just the mean of ht bins

#change reservoir size and set volume
reservoir = Reservoir(2800,20) # Loch Lomond. Capacity in MG and monthly env release in MG
reservoir.set_volume(2800)


# total number of houses of each size
populations = np.array([2365,1648,1456,1285,1424,1027,1018,1077,874,1931,2352,4064,3110,2091,3263,4895])
populations = np.multiply(populations,1)
L.append(["Populations: ",populations])

# sizes from the get_acs_data script
#household_sizes = [1.533492 ,1.770660 ,1.727516, 2.583219, 2.342015, 2.953631, 2.050650, 2.170571, 3.291193, 2.528014 ,2.455181, 2.765476, 3.049735 ,2.922616, 2.993665, 3.077490]
household_sizes = [1.870420, 1.875249, 2.201320, 2.373557 ,2.483636 ,2.583789 ,2.678085, 2.736607, 2.687910 ,2.792010, 2.883071, 2.947434, 3.085348, 3.139869 ,3.206045, 3.180690]

L.append(["Household Sizes: ",household_sizes])
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


# rates from here: https://www.cityofsantacruz.com/government/city-departments/water/monthly-water-costs-calculator
base_charge = 10.99
ut.set_fixed_charge(base_charge)

ut.set_tier_prices([10.03,11.86,13.78,16.74])
ut.set_tiers([5,2,2,999999])


# initialize states
decision_trigger = False
conservation_fraction = 0
percentage_change_quantity = 0
res_reduction = 0
nonres_reduction = 0

print("Running scenario: ",scenario)


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

        #volume_reduced = (res_reduction)*(this_baseline)#+((1-nonres_reduction)*this_other)
        outputs.loc[outputs.index==m,'monthlyVolumeReduced'] =  (city.get_utility_demand(this_baseline)-city.get_utility_demand(this_baseline*(1-res_reduction)))/1000000
        # Reduction in MG/month

        yearly_cost_of_conservation = ut.calculate_cost_of_conservation_EVEN_by_household(res_reduction, city,baseline_demand)
        additional_monthly_cost = yearly_cost_of_conservation/12

        outputs.loc[outputs.index==m,'annualRevenueLost']=yearly_cost_of_conservation

        # update my rate structure
        household_cost = additional_monthly_cost/city.counts.sum()
        # ^^ this is costs per month/household

        # pass costs on in the fixed cost for right now
        if(res_reduction !=0):
            ut.set_fixed_charge(ut.fixed_charge+ household_cost)
        else:
            ut.set_fixed_charge(base_charge)
        #print(household_cost)

    # regardless of what we've done, record the bill structure
    outputs.loc[outputs.index==m,'fixedCharge']=ut.fixed_charge
    outputs.loc[outputs.index==m,'conserveStatus'] = res_reduction


    if outputs['fixedCharge'].iloc[m] > outputs['fixedCharge'].iloc[m-1] and m> 0:
        print("inpedloop")
        # we only do this when the charge goes up, not when it goes     down,
        # because that amount is incorperated into the rebound which is calculated
        # seperately
        # this means we updated the fixed charge above
        # so we have to update the demand based on that
        fixed_charge_difference = outputs['fixedCharge'].iloc[m] - outputs['fixedCharge'].iloc[m-1]
        print("fixed charge diff: ",fixed_charge_difference)
        ped_class_demands = city.get_total_household_demands(this_baseline)
        mean_demand = ped_class_demands.mean()
        print("mean demand: ",mean_demand)
        percentage_change_price = fixed_charge_difference/ut.get_bill(mean_demand/748)
        print("bill is: $",ut.get_bill(mean_demand/748))
        percentage_change_quantity = PED*percentage_change_price
        # TODO
        #here. Updating PED to work based on total bills
        print("%age change quantity: ",percentage_change_quantity)

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
    class_demands = city.get_total_household_demands(this_baseline*(1-res_reduction))
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

    # udpate groundwater
    this_ground = groundwaters['groundwater'].iloc[m]

    # record groundwater
    outputs.loc[outputs.index==m,'ground'] = this_ground

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

    # TODO: writeamount to outputs

    # simulate them going into the reservoir
    release =reservoir.make_fixed_environmental_release(ll_inflows-et['et'].iloc[this_month-1],ll_demand)
    # et gets included here because evaporative lossese/ precipitation gains
    # are beneficial uses which count toward previously stored water, not current uses


    # record release and volume
    outputs.loc[outputs.index==m,'level'] = reservoir.volume
    outputs.loc[outputs.index==m,'release'] = release

    # calculate and record any deficits

    this_deficit = max(ut.demand-ll_demand-this_ground-surface_inflows.iloc[0],0)
    outputs.loc[outputs.index==m,'deficit'] = this_deficit



# record the outputs
with open(repo_home / 'outputs'/'santa_cruz'/output_folder/"params.txt","w") as filehandle:
    filehandle.writelines("%s\n" % param for param in L)
outputs.to_csv(repo_home / 'outputs'/'santa_cruz'/ output_folder /"outputs.csv")
hh_demand.to_csv(repo_home / 'outputs'/'santa_cruz'/ output_folder / "hh_demand.csv")
hh_bills.to_csv(repo_home / 'outputs'/'santa_cruz'/ output_folder / "hh_bills.csv")
