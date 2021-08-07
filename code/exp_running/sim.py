# packages
import os, sys
#sys.path.append("/Users/brachuno/Documents/__college/hr/water-equity/code")

import datetime
import math
import copy
import pandas as pd
import numpy as np
import itertools
import numpy_financial as npf

import multiprocessing
from joblib import Parallel, delayed


from pathlib import Path
from wrtools import *

# CHANGE FOR DIFFERENT MACHINES
num_cores = 4
# make our own 'expand grid' function. This is from stack exchange, but apparently it
# is in the pandas documentation


# define all the parameters we want to run in a large dataframe and we will pull from
# them wherevere we need to in the code in order to run each set
# note, we have to pad each column with NAs until they are all he same length. Currently 4

run_parameters = pd.DataFrame()
run_parameters['run','drought_characteristics','income_distribution','income_elasticity','fee_passthrough','reservoir_capacity','pay_back_period','discount_rate','mitigation_decision'] = []
#run_parameters.index = run_parameters['run']
# TODO set up the index

drought_characteristics = ["baseline.csv","long.csv","intense.csv","long_intense.csv"]
income_distribution = ["state"]#,"low_income","high_income","inequality"]
income_elasticity= [0.1,.41]#,0.7]
fee_passthrough = ["zero_threshold"]#,"high_threshold","income_threshold"]
reservoir_capacity = [1400,2800,5600]
pay_back_period = [30] #[20,30,40]
discount_rate = [3]#[1.5,3,4.5]
mitigation_decision = ['baseline','improved','market']
build_decision = ['none','desal','npr','grrp-r','grrp-l','dpr']
water_cost = [484*3.06,800*3.06,10000,22326.39]

# define this 'ex[pand-grid' function. Trying to replicaaate expand grid in R

def expandgrid(*itrs):
   product = list(itertools.product(*itrs))
   return {'Var{}'.format(i+1):[x[i] for x in product] for i in range(len(itrs))}

# give it the lists above and it returns our combos
parameter_list = expandgrid(drought_characteristics,income_distribution,income_elasticity,fee_passthrough,reservoir_capacity,pay_back_period,discount_rate,mitigation_decision,build_decision,water_cost)
parameter_list = pd.DataFrame.from_dict(parameter_list)
parameter_list.columns = ['drought_characteristic','income_distribution','income_elasticity','fee_passthrough','reservoir_capacity','pay_back_period','discount_rate','mitigation_decision','build_decision','water_cost']

# remove any 'baseline' or 'improved' which isn't 'none'

#remove_list = (parameter_list['build_decision']=="none") & (parameter_list['mitigation_decision'].isin(['build']))
#remove_list2 = (parameter_list['mitigation_decision'].isin(['improved','baseline'])) & (parameter_list['build_decision'].isin(['desal','desal','npr','grrp-r','grrp-l','dpr']))


#remove_list3 = (parameter_list['mitigation_decision'].isin(['baseline','improved']) & ((parameter_list['pay_back_period'].isin(['20','40']))|(parameter_list['discount_rate'].isin([1.5,4.5]))))
#parameter_list = parameter_list[~(remove_list | remove_list2 | remove_list3)]

# define our relative path base
repo_home = Path("./").absolute().parents[1]
# ^^ may not work if you are runnign interactively. Should work if you run the script all as once
# i.e. click 'run' vs running line by line

#  write a dictionary file with all of the parameters in it
parameter_list.to_csv(repo_home / 'outputs'/'santa_cruz'/ 'experiments' /"parameter_list.csv")

# below is our simulation
def sim_function(p):
    today = datetime.datetime.now()
    L = ["Date: ",today.strftime("%d/%m/%Y %H:%M:%S"),"/n"]
    print(p)
    # cutoffs
    cutoffs = np.array([0,5,15,25,35,50])
    conservation_policies = pd.DataFrame(index= cutoffs)

    this_mitigation_decision = parameter_list['mitigation_decision'].iloc[p]

    if this_mitigation_decision=="baseline" :
        conservation_policies['res_reductions']=np.array([0,5,16,27,38,52])
        conservation_policies['other_reductions']=[0,5,15,25,35,50]
        scenario = "baseline"
        L.append(["Cutoffs: ",cutoffs])
        L.append(["res reductions: ",conservation_policies['res_reductions']])
        conservation_threshold = 364

    elif this_mitigation_decision=="improved":
        # this is if we do the improved policy
        conservation_policies['res_reductions']=np.array([0,5,12,27,38,52])
        conservation_policies['other_reductions']=[0,5,12,25,35,50]
        scenario = "improved"
        L.append(["Cutoffs: ",cutoffs])
        L.append(["res reductions: ",conservation_policies['res_reductions']])
        conservation_threshold = 291

    elif this_mitigation_decision=="market":
        market_cost = parameter_list['water_cost'].iloc[p] # convert $ /af to $/MG
        scenario = "market"
        conservation_threshold = 364

    elif this_mitigation_decision =="build":
        scenario="build"
        conservation_threshold = 364
    else:
        print("unsupported mitigation decision ")

    # read in the decisionmaking file
    decision_file = repo_home / 'data'/'santa_cruz'/'sc_decisions.csv'
    decisions = pd.read_csv(decision_file)

    L.append(["Decision file: ", decision_file])
    L.append(["Scenario: ",scenario])
    #decisions['conserve_stage']=decisions[scenario]

    # threshold for making an economic decision
    #YED = 0.43 # YED and PED from dalhausen 2003, they do a meta-analysis, we use

    YED = parameter_list['income_elasticity'].iloc[p]
    PED= 0.4  # the mean values of their analysis

    L.append(["YED: ",YED])
    L.append(["PED: ",PED])

    # read in the input data
    this_file = parameter_list['drought_characteristic'].iloc[p]
    input_data = pd.read_csv(repo_home / 'data'/'santa_cruz'/'generated_drought_scenarios'/ this_file )

    # ALL DATA IN INPUT DATA IS MILLIONGALLONS/MONTH

    # convert the input data date
    input_data['date'] = pd.to_datetime(input_data['date'],format="%Y-%m-%d", errors = 'coerce')

    # get my baseline demand
    baseline_demand = pd.read_csv(repo_home / 'data'/'santa_cruz'/'rcpgd_sc.csv')
    baseline_demand.columns = ["reporting_month","mean"]
    # units are GPCD`

    # scale up seasonality pattern so that residential demand matches
    baseline_demand['mean'] = baseline_demand['mean']*1.32

    # initialize my dataframe of outputs
    outputs = pd.DataFrame(columns=['Date','totalDemand','residentialDemand','otherDemand','northCoast','taitStreet','newellInflow','feltonDiversions','res_drawdown',
                                   'level', 'deficit','release','restriction','trigger',
                                   'conserveStatus','fixedCharge','tieredPrices','pedReduction','monthlyCost','monthlyVolumeReduced',
                                   'unadjusted_demand','build_prod','market_buy'])

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

    groundwaters = pd.read_csv(repo_home/'data'/'santa_cruz'/'sc_groundwater.csv')

    groundwaters = groundwaters[['date','groundwater']]

    # other demand
    other_demand = pd.read_csv(repo_home / 'data'/'santa_cruz'/'sc_monthly_non_res_demand.csv')

    # et
    et = pd.read_csv(repo_home / 'data'/'santa_cruz'/'sc_monthly_et.csv')

    # now for my cities
    city = City(1,YED,61000.01)#

    #change reservoir size and set volume
    this_capacity = parameter_list['reservoir_capacity'].iloc[p]
    reservoir = Reservoir(this_capacity,20) # Loch Lomond. Capacity in MG and monthly env release in MG
    reservoir.set_volume(this_capacity)

    hh_size_options = pd.read_csv(repo_home/'data'/'state_data'/'three_distributions.csv' )
    #total number of 33880 houses in santa cruz
    populations = hh_size_options[parameter_list['income_distribution'].iloc[p]]
    #populations = np.array([2365,1648,1456,1285,1424,1027,1018,1077,874,1931,2352,4064,3110,2091,3263,4895])
    populations = np.multiply(populations,33880)
    L.append(["Populations: ",populations])

    # sizes from the get_acs_data script
    #household_sizes = [1.533492 ,1.770660 ,1.727516, 2.583219, 2.342015, 2.953631, 2.050650, 2.170571, 3.291193, 2.528014 ,2.455181, 2.765476, 3.049735 ,2.922616, 2.993665, 3.077490]
    household_sizes = [1.870420, 1.875249, 2.201320, 2.373557 ,2.483636 ,2.583789 ,2.678085, 2.736607, 2.687910 ,2.792010, 2.883071, 2.947434, 3.085348, 3.139869 ,3.206045, 3.180690]

    L.append(["Household Sizes: ",household_sizes])
    #populations = [ 7652. , 13583., 14923., 21236., 33672., 26211., 33280., 16262.]
    #populations = np.divide(populations,household_sizes)

    income = [7500,12500,17500,22500,27500,32500,37500,42500,47500,55000,67500,87500,112500,137500,175000,250000]
    income_names = [str(s) for s in income]

    # make the houshold demand dataframe
    hh_demand = pd.DataFrame(columns=income)

    # hh_bills_columns are actual bills income,
    # the ones with unadjusted anything are raw_income
    # the ones with adjusted rates but not adjusted demand are rateonly_income
    # the ones with unadjusted rates but adjusted demand are demandonly_income
    bills_names = copy.copy(income_names)
    bills_names.extend([ "raw_" + s for s in income_names])
    bills_names.extend(["rateonly_" + s for s in income_names])
    bills_names.extend( ["demandonly_" + s for s in income_names])

    hh_bills  = pd.DataFrame(columns=bills_names)

    leakages = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]

    # now add these to the city object
    city.set_bins(populations,income,household_sizes,leakages)

    # define a utility and it's discount rate
    ut = Utility("utility", parameter_list['discount_rate'].iloc[p])
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
    build_production =0
    update_rates = False
    market_buy=0
    build_monthly_cost= 0
    additional_monthly_cost = 0
    outputs['monthlyCost']=0
    print("Running scenario: ",scenario)
    print("build scenario: ",parameter_list['build_decision'].iloc[p])
    # we do this for every month
    for m in range(outputs['Date'].count()):

        # set my date
        this_date = outputs['Date'].iloc[m]
        this_month = outputs['month'].iloc[m]

        # read in my baseline inputs for residential and non-residential demands
        this_baseline = baseline_demand['mean'].loc[baseline_demand['reporting_month'].eq(this_month)][this_month-1]
        this_other= other_demand['commercial_industrial'].loc[baseline_demand['reporting_month'].eq(this_month)][this_month-1]

        # write the un-messed-with-demand to file
        outputs.loc[outputs.index==m,'unadjusted_demand']=city.get_utility_demand(this_baseline)/1000000 + this_other

        # udpate residential demand if it needs to be based on what 'demand_changes' says
        # the demand changes column will perminantly alter demand to 1-(demandchanges*100) *old demand
        #if decisions['demand_changes'].iloc[m]!=0:
        #    print("demand changed to ",(1-decisions['demand_changes'].iloc[m]))
        #    baseline_demand['mean'] = baseline_demand['mean']*(1-decisions['demand_changes'].iloc[m])


        # adding another option-related thing here in which:
        # we look at the total demand for the rest of the year
        # and see how much water we will have available for the rest of the year
        #
        # our goal is to issue conservation

        #(1) if it's april
        if this_month == 4:
            res_reduction=0
            nonres_reduction = 0 # set these to 0 now, and update if we need to.
            # if the previous year was non-zero and this year was 0, this would reset it

            # in april, we look ahead at the summer demand and the summer
            # water availability
            this_summer_demand =  sum(other_demand['commercial_industrial'].iloc[4:10])
            #get the dataframe of the summer months
            summer_res_demand = baseline_demand['mean'].iloc[4:10]

            for s in range(6):
                this_summer_demand = this_summer_demand + city.get_utility_demand(baseline_demand['mean'].iloc[s])/1000000

            # the result is the total amount of water we need over this summer in MG

            # now estimate how much we will have. from the desal variable, or 14*6 is a
            # rough  approximation of groundwater
            summer_supply = build_production*6

            # and get the surface water and LL water
            summer_supply =summer_supply+ inflows[['northCoast','taitStreet','feltonDiversions','newellInflow']].iloc[m:m+6].sum().sum()

            if summer_supply < this_summer_demand:

                # actually do conservation
                # let's be conservative with it: whatever the difference is,
                # see what %age of residential demand that is and ask them to conserve
                summer_difference = (this_summer_demand-summer_supply)
                #print(summer_difference/summer_supply)
                # now we want a reservoir which is 83% full at the end of the summer
                # and we will assume now it's full. So we can also use 17/6% of the reservoir's
                # capacity every month also

                #if summer_difference >= 0.13*reservoir.volume: this works out to 364mg.
                # but it wouldn't be the same if our reservoir was larger. so make it a fixed value.
                # this is based on the idea that they can only use about that much while still having enough
                # for 2 more bad years which is their policy
                if summer_difference >= conservation_threshold:
                    print("resfrac:", summer_difference/reservoir.volume)
                                        # Here we create an if statement which selects whether
                    # we are conserving or market-buying.

                    if this_mitigation_decision in ["baseline","improved"]:

                        # so now we have to amke this summer_difference up
                        ideal_reduction_fraction = (summer_difference)/this_summer_demand
                        print("ideal reduction:",ideal_reduction_fraction)

                        # now set res reduction values and lets see what happens?
                        res_reduction = ideal_reduction_fraction
                        nonres_reduction = ideal_reduction_fraction
                        outputs.loc[outputs.index==m,'conserveStatus'] = ideal_reduction_fraction
                        # RES REDUCTION HAS TO REMAIN THIS UNTIL OCTOBER

                        outputs.loc[outputs.index==m,'monthlyVolumeReduced'] =  (city.get_utility_demand(this_baseline)-city.get_utility_demand(this_baseline*(1-res_reduction)))/1000000
                        yearly_cost_of_conservation = ut.calculate_cost_of_conservation_EVEN_by_household(res_reduction, city,baseline_demand)
                        additional_monthly_cost = yearly_cost_of_conservation/12
                        outputs.loc[m:(m+11),'monthlyCost']= outputs.loc[m:(m+11),'monthlyCost'] + yearly_cost_of_conservation/12

                        update_rates = True

                    elif this_mitigation_decision =="market":
                        res_reduction=0
                        nonres_reduction=0

                        #amount_to_buy = summer_difference
                        #print("buying: ",amount_to_buy," mg of water on the market at a cost of $",amount_to_buy*market_cost)
                        #additional_monthly_cost = amount_to_buy*market_cost/12.0
                        #outputs.loc[m:(m+11),'monthlyCost']= outputs.loc[m:(m+11),'monthlyCost']+ amount_to_buy*market_cost
                        #update_rates = True

                        # make a dataframe of the projected demand and projected supply
                        # calculate a column of the difference.
                        # in the water balance later on, pull from that dataframe nearby to where
                        # we have desal production
                        # write to outputs
                else:
                    # this is what happens if we have some kind of difference, but it isn't enough
                    # to conserve. Basically do the same as what we would do below
                    res_reduction = 0
                    nonres_reduction = 0
                    update_rates = True
                    additional_monthly_cost=0
                    # set all the conservations equal to 0 and move on
            else:
                # this is what happens after the drought is over
                 res_reduction = 0
                 nonres_reduction = 0
                 update_rates = True
                 additional_monthly_cost=0
                 #ut.set_fixed_charge(base_charge)
                 #print('ending conservation: ',this_date)

        # read from input file to determine what decisions we are making
        # this only happens if we are conserving. THIS DOES NOT GET CALLED IF WE BUILD because the
        # build column in sc_decisions is all 0s

        # Below is the old code which picked the stages
        # if (decisions['conserve_stage'].iloc[m]!=decisions['conserve_stage'].iloc[m-1]):

        #     update_rates = True
        #     this_decision = decisions['conserve_stage'].iloc[m]
        #     this_conservation = conservation_policies.iloc[this_decision]

        #     # the conservaation happens in the 'conservation_fraction' value
        #     outputs.loc[outputs.index==m,'conserveStatus'] = this_conservation['res_reductions']
        #     res_reduction = this_conservation['res_reductions']/100
        #     nonres_reduction = this_conservation['other_reductions']/100

        #     # now calculate lost revenue from the reduction
        #     # calculate the volume needed

        #     #volume_reduced = (res_reduction)*(this_baseline)#+((1-nonres_reduction)*this_other)
        #     outputs.loc[outputs.index==m,'monthlyVolumeReduced'] =  (city.get_utility_demand(this_baseline)-city.get_utility_demand(this_baseline*(1-res_reduction)))/1000000
        #     # Reduction in MG/month

        #     yearly_cost_of_conservation = ut.calculate_cost_of_conservation_EVEN_by_household(res_reduction, city,baseline_demand)
        #     additional_monthly_cost = yearly_cost_of_conservation/12

        #     outputs.loc[outputs.index==m,'annualRevenueLost']=yearly_cost_of_conservation


        if (parameter_list['build_decision'].iloc[p]!="none") and (m ==0):

            # if we build, update the supplies right away, increase the rates
            update_rates=True

            this_build_decision=parameter_list['build_decision'].iloc[p]
            if this_build_decision=='desal':
                # make a desal value and add it to the water balance ad
                # set it to something.
                # changing this to be the consultant's report from 07/22 weekly
                # report
                # producton is 2800AF/yr ->
                # cost is 115mil capex
                # opex is 3.3 mil


                build_production = 76.03 # unit is MG/month
                total_cost = 115000000
                additional_annual_cost =3300000

            elif this_build_decision=='npr':
                # 282 AF/yr -> 91.890103 mg/yr
                build_production = 7.65 #mg/month

                total_cost = 1100000
                additional_annual_cost = 225000
                # set cost
            elif this_build_decision=="grrp-r":
                # update reservoir size
                build_production = 14.9 #mg/month
                total_cost = 20400000
                additional_annual_cost = 330000
                # then set cost

            elif this_build_decision=="grrp-l":
                # update reservoir size
                build_production =  64.87 #mg/month
                total_cost = 70500000
                additional_annual_cost = 3100000

            elif this_build_decision=="dpr":
                # update reservoir size
                build_production = 97.32 #mg/month
                total_cost = 110600000
                additional_annual_cost = 6000000
            else:
                print("unrecgonized build decision")

            # turn cost into 'additonal monthly cost' usint PBP and irr
            # intereste rate is currently integer so divide by 100 to get fractional which pmt needs
            # divide by 12 because they are APRs and we are calculating monthly payments
            # PBP is in years, so multiply by 12
            # whole thing multiplied by -1 to get a positive value for the rest of the calculations
            build_monthly_cost=  -1*npf.pmt(ut.irr/100/12, parameter_list['pay_back_period'].iloc[p]*12, total_cost)+ additional_annual_cost/12
            outputs.loc[m:,'monthlyCost'] = build_monthly_cost
        # update my rate structure based on the selected parameter
        this_fee_passthrough = parameter_list['fee_passthrough'].iloc[p]

        if update_rates== True:
            #print("adjusting rates!")
            if this_fee_passthrough=="zero_threshold" :
                print(this_date,additional_monthly_cost)
                household_cost = additional_monthly_cost/city.counts.sum()
                # ^^ this is costs per month/household

                # pass costs on in the fixed cost for right now
                #if(res_reduction !=0):
                ut.set_fixed_charge(base_charge+ build_monthly_cost/city.counts.sum() + household_cost)

                #WHERE I AM: fix it so that scenario 8 goes back to normal again

                #divide costs up into build fixed costs and variable fixed costs (market/conservation)

                #ALSO fix issue where costs are all positive for conservation
                #in non-basleiine scenarios?

                #lif parameter_list['build_decision'].iloc[p]!="none":
                #    ut.set_fixed_charge(ut.fixed_charge+ household_cost)

            #else:
                #ut.set_fixed_charge(base_charge)
                #percentage_change_quantity = 0


            elif this_fee_passthrough == "high_threshold":
                print("high threshold passthrough")
                #increase the volumetric charge for households above 5CCF
                # in other work it' 8CCF, but this is more suited to the lower
                # residential water use in Santa Cruz

                #(1) figure out how much water is being used in those tiers
                high_threshold_cutoff = 5 # PARAMETER TO SET
                total_water_use_above_threshold = list()
                for h in range(len(baseline_demand['mean'])):
                    household_demands = (city.get_total_household_demands(baseline_demand['mean'].iloc[h])/748)-high_threshold_cutoff

                    # Multiply by the number of households
                    total_household_demands = np.multiply(household_demands,city.counts)

                    # remove any negatives and then multiply
                    total_water_use_above_threshold.append(total_household_demands[total_household_demands>0].sum())

                #(2) Divide the total cost by the volume of water and add hat to the upper
                # tier prices. This will likely provide less revenue because we are assuming
                # it's used across the board.
                volumetric_increase = additional_monthly_cost/np.array(total_water_use_above_threshold).sum()

                #
                # for testing: 5313810.667 is the amount of annual revenue lost
                # in a 15% drought
                # divide this by 3 and add it to the volumetric rates
                ut.set_tier_prices([10.03,11.86+volumetric_increase/3,13.78+volumetric_increase/3,16.74+volumetric_increase/3])


            elif this_fee_passthrough == "income_threshold":
                print("high threshold passthrough")
                # increase the charge for only households making lots of money
                # TODO: this is going to require changing the way we calculate
                # rates and is a slightly more involved process

            else:
                print("unrecgonized fee passthrough parameter on parameter list line", p)

        # here, we do
        # regardless of what we've done, record the bill structure
        outputs.loc[outputs.index==m,'fixedCharge']=ut.fixed_charge
        #outputs.loc[outputs.index==m,'tieredPrices']=ut.prices
        #TODO: write the tiers. Requires making 3 new columns
        outputs.loc[outputs.index==m,'conserveStatus'] = res_reduction

        update_rates = False

        if outputs['fixedCharge'].iloc[m] > outputs['fixedCharge'].iloc[m-1] and m> 0:
            print("inpedloop")
            # we only do this when the charge goes up, not when it goes     down,
            # because that amount is incorperated into the rebound which is calculated
            # seperately
            # this means we updated the fixed charge above
            # so we have to update the demand based on that
            fixed_charge_difference = outputs['fixedCharge'].iloc[m] - outputs['fixedCharge'].iloc[m-1]
            #print("fixed charge diff: ",fixed_charge_difference)
            ped_class_demands = city.get_total_household_demands(this_baseline)
            mean_demand = ped_class_demands.mean()
            #print("mean demand: ",mean_demand)
            percentage_change_price = fixed_charge_difference/ut.get_bill(mean_demand/748)
            #print("bill is: $",ut.get_bill(mean_demand/748))
            percentage_change_quantity = PED*percentage_change_price
        else:
            percentage_change_quantity=0
            #print("%age change quantity: ",percentage_change_quantity)

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



        # for this particular month, here are our inflows
        #this_inflow = inflows['surface'].loc[inflows['date'].eq(this_date)]
        surface_inflows = inflows[['northCoast','taitStreet']].loc[inflows['date'].eq(this_date)].sum(axis=1)
        # INFLOWS AND SURFACE WATER NEED TO BE MONTHLY TOTALS

        # record the specific inflows
        outputs.loc[outputs.index==m,['northCoast','taitStreet']] = inflows[['northCoast','taitStreet']].loc[inflows['date'].eq(this_date)]

        # record inflows
        outputs.loc[outputs.index==m,'surface']= surface_inflows.to_numpy()[0]

        # reservoir inflows
        ll_inflows = inflows[['feltonDiversions','newellInflow']].loc[inflows['date'].eq(this_date)].sum(axis=1)
        outputs.loc[outputs.index==m,['feltonDiversions','newellInflow']]= inflows[['feltonDiversions','newellInflow']].loc[inflows['date'].eq(this_date)]
        ll_inflows = ll_inflows.iloc[0]

        # udpate groundwater
        this_ground = groundwaters['groundwater'].iloc[m]

        # record groundwater
        outputs.loc[outputs.index==m,'ground'] = this_ground

        # calculate my surface water deficit to determine how much we can draw from the reservoir
        # this is demand- surface-ground
        surface_deficit = ut.demand - this_ground - surface_inflows.iloc[0] - build_production

        # the policy (keep 87% full by April) means we can draw down 70mg from the res each month
        # drawdown is the minimum of (surface deficit) and (res_inflows + 70mg)
        # if the drawdown is above that, we will conserve, and move the conservation status to whatever of the 4 stages
        # will meet the demand change

        # reservoir is untouched unless we need it (ie, surface deficit > 0)
        ll_demand = 0

        if (surface_deficit > 0):
            # this means we have to do some sort of drawdown

            if this_mitigation_decision in ["baseline","improved"]:
                # this is just drawdown whatever we need.
                # the demand has already been adjusted via conservation, so
                # we know we won't overdraw

                drawdown = min(surface_deficit,reservoir.volume)
                ll_demand = drawdown
                outputs.loc[outputs.index==m,'res_drawdown']= ll_demand

            elif this_mitigation_decision=="market":

                # drawdown what we can, but no more than inflows + 70
                drawdown = min(surface_deficit,ll_inflows+70)
                ll_demand = drawdown

                # write this
                outputs.loc[outputs.index==m,'res_drawdown']= ll_demand

                #if there is a gap between the deficit and the drawdown, buy
                market_buy = max(surface_deficit-drawdown,0)
                outputs.loc[outputs.index==m,'market_buy']=market_buy
                outputs.loc[outputs.index==m,'monthlyCost'] = outputs.loc[outputs.index==m,'monthlyCost']  + market_buy*market_cost

                # increase the bills also
                ut.set_fixed_charge(base_charge+ build_monthly_cost/city.counts.sum() + (market_buy*market_cost)/city.counts.sum())

                print("buying: ",market_buy," mg of water on the market at a cost of $",market_buy*market_cost)

                outputs.loc[m,'monthlyCost']= outputs.loc[m ,'monthlyCost']+ market_buy*market_cost

        # first write a abseline bill, this is assuming res_reduction = 0 and the bill is normal
        baseline_class_demands = city.get_total_household_demands(this_baseline)
        baseline_class_bills = []
        previous_fixed_charge = ut.fixed_charge
        ut.set_fixed_charge(base_charge)

        reduced_class_demands = city.get_total_household_demands(this_baseline*(1-res_reduction))
        reduced_class_normal_bill = []
        for c in baseline_class_demands:
            baseline_class_bills.append(ut.get_bill(c/748))

        for c in reduced_class_demands:
            reduced_class_normal_bill.append(ut.get_bill(c/748))

        # now adjust the fixed charge back and use the non-adjusted demand
        ut.set_fixed_charge(previous_fixed_charge)
        baseline_class_adjusted_bill = []
        for c in baseline_class_demands:
            baseline_class_adjusted_bill.append(ut.get_bill(c/748))

        # write the bills
        #Record demand for an average house from each class
        # update demand
        class_demands = city.get_total_household_demands(this_baseline*(1-res_reduction))
        hh_demand.loc[m] = class_demands

        #print("fixed: ",ut.fixed_charge)
        # get the bills for each
        class_bills = []
        for c in class_demands:
            class_bills.append(ut.get_bill(c/748))
            # this is writing the bills for each income class assuming we've made all the changes

        this_bills = []
        this_bills.extend(class_bills)
        this_bills.extend(baseline_class_bills)
        this_bills.extend(baseline_class_adjusted_bill)
        this_bills.extend(reduced_class_normal_bill)
        hh_bills.loc[m] = this_bills

        # check to see if we are withdrawing more than the reservoir amount
        # simulate them going into the reservoir
        release =reservoir.make_fixed_environmental_release(ll_inflows-et['et'].iloc[this_month-1],ll_demand)


        # record release and volume
        outputs.loc[outputs.index==m,'level'] = reservoir.volume
        outputs.loc[outputs.index==m,'release'] = release

        # record the desal used
        outputs.loc[outputs.index==m,'build_prod'] = build_production

        # CTEST COMMENT
        #deficit is deficit if we don't do market water

        this_deficit = max(ut.demand-ll_demand-this_ground-surface_inflows.iloc[0]-build_production-market_buy,0)
        outputs.loc[outputs.index==m,'deficit'] = this_deficit


    paramstring = str(parameter_list.index[p]) + "_params.txt"
    # record the outputs
    with open(repo_home / 'outputs'/'santa_cruz'/'experiments'/ paramstring,"w") as filehandle:
        filehandle.writelines("%s\n" % param for param in L)

    outstring = str(parameter_list.index[p]) + "_outputs.csv"
    hhdstring = str(parameter_list.index[p]) + "_hh_demand.csv"
    hhbstring = str(parameter_list.index[p]) + "_hh_bills.csv"

    outputs.to_csv(repo_home / 'outputs'/'santa_cruz'/ 'experiments'/ outstring)
    hh_demand.to_csv(repo_home / 'outputs'/'santa_cruz'/ 'experiments'/ hhdstring)
    hh_bills.to_csv(repo_home / 'outputs'/'santa_cruz'/ 'experiments'/ hhbstring)

# now multiprocess it
Parallel(n_jobs=num_cores)(delayed(sim_function)(i) for i in range(0,max(parameter_list.index)+1))
