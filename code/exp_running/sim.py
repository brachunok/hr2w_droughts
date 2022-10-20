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
from tqdm import tqdm
# CHANGE FOR DIFFERENT MACHINES
num_cores = 2



drought_characteristics = ['baseline.csv',"long_intense.csv","long.csv","intense.csv"]
income_distribution = ["state"]#,"low_income","high_income","inequality"]
income_elasticity= [0.15]
fee_passthrough = ["zero_threshold"]#,"one_tier","multi_tier"]
reservoir_capacity = [2800]
pay_back_period = [30] #[20,30,40]
discount_rate = [3]#[1.5,3,4.5]
mitigation_decision = ['baseline','market']
build_decision = ['none','desal','grrp-r']# 'npr','grrp-r','grrp-l','dpr']
water_cost = [22326.39]
cross_price = [0.013,-0.013]
cpe_squared = [0.00]
price_elasticity = [0,0.325]#,0.4,0.8]#,0.8] #[-.25,0,.25,.5,.75,1,1.25,1.5,1.75,2,3,4,5,10]
rate_structure = ['ibp']#["fixed","one_tier","ibp","dpb"]



# define this 'ex[pand-grid' function. Trying to replicaaate expand grid in R

def expandgrid(*itrs):
   product = list(itertools.product(*itrs))
   return {'Var{}'.format(i+1):[x[i] for x in product] for i in range(len(itrs))}

# give it the lists above and it returns our combos
parameter_list = expandgrid(drought_characteristics,income_distribution,income_elasticity,fee_passthrough,reservoir_capacity,pay_back_period,discount_rate,mitigation_decision,build_decision,water_cost,price_elasticity,rate_structure,cross_price,cpe_squared)
parameter_list = pd.DataFrame.from_dict(parameter_list)
parameter_list.columns = ['drought_characteristic','income_distribution','income_elasticity','fee_passthrough','reservoir_capacity','pay_back_period','discount_rate','mitigation_decision','build_decision','water_cost','price_elasticity','rate_structure','cross_price','cpe_squared']

# remove any 'baseline' or 'improved' which isn't 'none'

remove_list = (parameter_list['cross_price']==0.013) & (parameter_list['price_elasticity']!=0)
remove_list2 = (parameter_list['cross_price']==-0.013) & (parameter_list['price_elasticity']==0)
#remove_list3 = (parameter_list['price_elasticity']==1.06) & (parameter_list['cross_price']==0)

#remove_list3 = (parameter_list['mitigation_decision'].isin(['baseline','improved']) & ((parameter_list['pay_back_period'].isin(['20','40']))|(parameter_list['discount_rate'].isin([1.5,4.5]))))
parameter_list = parameter_list[~(remove_list | remove_list2)]# | remove_list3 )]

parameter_list.reset_index(drop=True,inplace=True)

# define our relative path base
repo_home = Path("./").absolute().parents[1]
# ^^ may not work if you are runnign interactively. Should work if you run the script all as once
# i.e. click 'run' vs running line by line

#  write a dictionary file with all of the parameters in it
parameter_list.to_csv(repo_home / 'outputs'/'santa_cruz'/ 'experiments' / 'review_responses2' / "parameter_list.csv")

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
    PED = parameter_list['price_elasticity'].iloc[p] # the mean values of their analysis

    cpe = parameter_list['cross_price'].iloc[p]
    cpe2 = parameter_list['cpe_squared'].iloc[p]

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

    # get my baseline demand


    # scale up seasonality pattern so that residential demand matches
    baseline_demand['mean'] = baseline_demand['mean']*1.32

    # initialize my dataframe of outputs
    outputs = pd.DataFrame(columns=['Date','totalDemand','residentialDemand','otherDemand','northCoast','taitStreet','newellInflow','feltonDiversions','res_drawdown','ground',
                                   'level', 'deficit','release','restriction','trigger',
                                   'conserveStatus','fixedCharge','tieredPrices','pedReduction','monthlyCost','monthlyVolumeReduced',
                                   'unadjusted_demand','build_prod','market_buy','infrastructureIncrease','surchargeIncrease'])

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
    city = City(YED,61000.01,cpe,cpe2)

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
    income = [7500,12500,17500,22500,27500,32500,37500,42500,47500,55000,67500,87500,112500,137500,175000,250000]
    # sizes from the get_acs_data script
    #household_sizes = [1.533492 ,1.770660 ,1.727516, 2.583219, 2.342015, 2.953631, 2.050650, 2.170571, 3.291193, 2.528014 ,2.455181, 2.765476, 3.049735 ,2.922616, 2.993665, 3.077490]
    household_sizes = [1.870420, 1.875249, 2.201320, 2.373557 ,2.483636 ,2.583789 ,2.678085, 2.736607, 2.687910 ,2.792010, 2.883071, 2.947434, 3.085348, 3.139869 ,3.206045, 3.180690]

    L.append(["Household Sizes: ",household_sizes])
    #populations = [ 7652. , 13583., 14923., 21236., 33672., 26211., 33280., 16262.]
    #populations = np.divide(populations,household_sizes)
    leakages = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]


    # now add these to the city object
    city.set_bins(populations,income,household_sizes,leakages,PED)



    # define a utility and it's discount rate
    ut = Utility(parameter_list['discount_rate'].iloc[p])


    this_rate_structure = parameter_list['rate_structure'].iloc[p]
    #["fixed","one_tier","ibp","dpb"]
    if this_rate_structure == "ibp":
        # rates from here: https://www.cityofsantacruz.com/government/city-departments/water/monthly-water-costs-calculator
        base_charge = 11.26 #10.99
        ut.set_fixed_charge(base_charge)

        ut.set_baseline_fixed_charge(base_charge)

       # ut.set_tier_prices([10.03,11.86,13.78,16.74])
       # baseline_rates = [10.03,11.86,13.78,16.74]
        ut.set_tier_prices([10.6,11.58,13.64,16.74])
        baseline_rates = [10.6,11.58,13.64,16.74]
        ut.set_tiers([6,2,2,999999])
        ut.set_baseline_tiers_and_prices([6,2,2,999999],[10.6,11.58,13.64,16.74])

        # now also adjust the income and price elasticity
        # for IBP the price is -.26, the income is -.36
        # convention is that negavice is input into the model as a
        # positive



    elif this_rate_structure == "fixed":

        base_charge = 74.06
        ut.set_fixed_charge(base_charge)
        ut.set_baseline_fixed_charge(base_charge)
        ut.set_tier_prices([0,0,0,0])
        baseline_rates = [0,0,0,00]
        ut.set_tiers([6,2,2,999999])
        ut.set_baseline_tiers_and_prices([6,2,2,999999],[0,0,0,0])


    elif this_rate_structure == "one_tier":

        base_charge = 11.26
        ut.set_fixed_charge(base_charge)
        ut.set_baseline_fixed_charge(base_charge)
        ut.set_tier_prices([10.99])
        baseline_rates = [10.99]
        ut.set_tiers([999999])
        ut.set_baseline_tiers_and_prices([999999],[10.99])


    elif this_rate_structure == "dpb":

        base_charge = 11.26
        ut.set_fixed_charge(base_charge)
        ut.set_baseline_fixed_charge(base_charge)
        ut.set_baseline_tiers_and_prices([6,2,2,999999],[11.38,10.4,8.34,5.15])
        ut.set_tier_prices([11.38,10.4,8.34,5.15])

        baseline_rates = [11.38,10.4,8.34,5.15]
        ut.set_tiers([6,2,2,999999])


    else:
        print("UNSUPPORTED RATE STRUCTURE")



    income_names = [str(s) for s in income]
    rev_contrib = pd.DataFrame(0,index=np.arange(0,outputs['Date'].count()),columns=['a'+s for s in income_names] + ['b' + s for s in income_names])

    # make the houshold demand dataframe

    # the ones with unadjusted anything are raw_income
    # the ones with adjusted rates but not adjusted demand are rateonly_income
    # the ones with unadjusted rates but adjusted demand are demandonly_income
    # suffixes are _f for fixed, then _1 ... for all the tiers
    tier_suffixes = ["_f"]
    tier_suffixes.extend(["_"+str(s+1) for s in range(0,len(ut.tiers))])
    bills_names = copy.copy(income_names)
    #bills_names.extend([b+s for b in income_names for s in tier_suffixes ])
    bills_names.extend([ "raw_" + s for s in income_names])
    #bills_names.extend([ "raw_" + b + s for b in income_names for s in tier_suffixes])
    bills_names.extend(["rateonly_" + s for s in income_names])
    #bills_names.extend([ "rateonly_" + b + s for b in income_names for s in tier_suffixes])
    bills_names.extend( ["demandonly_" + s for s in income_names])
    #bills_names.extend([ "rateonly_" + b + s for b in income_names for s in tier_suffixes])

    # now do all the above with a 1...5 afterwards
    suffix_bills = []


    demands_names = copy.copy(income_names)
    demands_names.extend([ "raw_" + s for s in income_names])
    #demands_names.extend(["adjusted_" + s for s in income_names])

    hh_bills  = pd.DataFrame(columns=bills_names)
    hh_demand = pd.DataFrame(columns=demands_names)

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
    household_cost = 0
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
        outputs.loc[outputs.index==m,'unadjusted_demand']=city.get_utility_demand_no_ped(this_baseline)/1000000 + this_other

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

            ut.set_tier_prices(baseline_rates)

            # in april, we look ahead at the summer demand and the summer
            # water availability
            this_summer_demand =  sum(other_demand['commercial_industrial'].iloc[4:10])
            #get the dataframe of the summer months
            summer_res_demand = baseline_demand['mean'].iloc[4:10]

            for s in range(6):
                this_summer_demand = this_summer_demand + city.get_utility_demand(baseline_demand['mean'].iloc[s],ut)/1000000

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

                        outputs.loc[outputs.index==m,'monthlyVolumeReduced'] =  (city.get_utility_demand_no_ped(this_baseline)-city.get_utility_demand_no_ped(this_baseline*(1-res_reduction)))/1000000
                        yearly_cost_of_conservation = ut.calculate_cost_of_conservation_EVEN_by_household(res_reduction, city,baseline_demand)
                        # calculate cost even used to also output: ,yearly_tier_use_baseline, yearly_tier_use_adjusted
                        # they are used for calculating different rates

                        additional_monthly_cost = yearly_cost_of_conservation/12
                        outputs.loc[m:(m+11),'monthlyCost']= outputs.loc[m:(m+11),'monthlyCost'] + yearly_cost_of_conservation/12
                        update_rates = True

                        # write the reduction to an output file
                        #rev_contrib.loc[m]=pd.array(ut.calculate_contribution_from_each_household(res_reduction,city,baseline_demand))

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

            elif this_build_decision =='desal_old':

                build_production = 0 # unit is MG/month
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

                outputs.loc[m:,'infrastructureIncrease'] = build_monthly_cost/city.counts.sum()
                outputs.loc[m:,'surchargeIncrease'] = household_cost



            elif this_fee_passthrough == "one_tier":
                print("one tier passthrough")

                toss_out,yearly_tier_use_baseline, yearly_tier_use_adjusted = ut.calculate_cost_of_conservation_EVEN_by_household(res_reduction, city,baseline_demand)
                # also get an updated tier use
                tier_use = yearly_tier_use_baseline / 748
                tier_use_reduced = yearly_tier_use_adjusted/748
                # this unit is CCFs

                # calculate the fixed, variable and total revenue before
                original_fixed_revenue = city.counts.sum()*ut.fixed_charge*12
                original_variable_revenue = np.dot(tier_use,ut.prices)
                original_total_revenue =original_fixed_revenue+original_variable_revenue

                # after fixed is the same, so caculate the variable and total revenue after curtailment
                after_variable_revenue = np.dot(tier_use_reduced,ut.prices)
                after_total_revenue = original_fixed_revenue + after_variable_revenue

                # what %age of after-curtailment variable revenue are we getting?
                variable_percentage_after = after_variable_revenue/(after_total_revenue)

                # get the total amount of revenue we need
                revenue_recovery = original_total_revenue-after_total_revenue

                # if we assume the after-curtailment %ages are the same, how much
                # revenue needs to come from fixed vs volume?

                fixed_revenue_recovery = revenue_recovery*(1-variable_percentage_after)
                variable_revenue_recovery = revenue_recovery*variable_percentage_after

                # calculate the fixed charge increase
                household_cost = fixed_revenue_recovery / city.counts.sum()/12
                ut.set_fixed_charge(base_charge+ build_monthly_cost/city.counts.sum() + household_cost)
                outputs.loc[m:,'surchargeIncrease'] = household_cost

                # calculate the tier increases
                tier_increase = variable_revenue_recovery/tier_use_reduced[0]

                #make the adjustment
                prices = ut.prices
                prices[0] = prices[0]+ tier_increase

                ut.set_tier_prices(prices)
                outputs.loc[outputs.index==m,'tieredPrices']=str(prices)

            elif this_fee_passthrough == "multi_tier":
                print("multi-tier passthrough")

                # moreor less redo this but pass on all fees to higher(ad hoc) tiers
                toss_out,yearly_tier_use_baseline, yearly_tier_use_adjusted = ut.calculate_cost_of_conservation_EVEN_by_household(res_reduction, city,baseline_demand)

                # also get an updated tier use
                tier_use = yearly_tier_use_baseline / 748
                tier_use_reduced = yearly_tier_use_adjusted/748

                # calculate the fixed, variable and total revenue before
                original_fixed_revenue = city.counts.sum()*ut.fixed_charge*12

                original_variable_revenue = np.multiply(tier_use,ut.prices)
                original_total_revenue =original_fixed_revenue+original_variable_revenue.sum()

                # after fixed is the same, so caculate the variable and total revenue after curtailment
                after_variable_revenue = np.multiply(tier_use_reduced,ut.prices)
                after_total_revenue = original_fixed_revenue + after_variable_revenue.sum()

                # what %age of after-curtailment variable revenue are we getting?
                fixed_percentage_after = original_fixed_revenue/(after_total_revenue)

                variable_percentages_after = after_variable_revenue/(after_total_revenue)

                # get the total amount of revenue we need
                revenue_recovery = original_total_revenue-after_total_revenue

                # if we assume the after-curtailment %ages are the same, how much
                # revenue needs to come from fixed vs volume?

                fixed_revenue_recovery = revenue_recovery*(1-variable_percentages_after.sum())
                variable_revenue_recovery = revenue_recovery*variable_percentages_after

                # calculate the fixed charge increase
                household_cost = fixed_revenue_recovery / city.counts.sum()/12
                ut.set_fixed_charge(base_charge+ build_monthly_cost/city.counts.sum() + household_cost)
                outputs.loc[m:,'surchargeIncrease'] = household_cost

                # calculate the tier increases
                tier_increase = np.divide(variable_revenue_recovery,tier_use_reduced)
                tier_increase = np.nan_to_num(tier_increase)

                #make the adjustment
                prices = ut.prices
                prices = prices+ tier_increase

                ut.set_tier_prices(prices)
                outputs.loc[outputs.index==m,'tieredPrices']=str(prices)


                # same as above, but also write some code which calculates the %ages
                # from the given rates

            else:
                print("unrecgonized fee passthrough parameter on parameter list line", p)

        # here, we do
        # regardless of what we've done, record the bill structure
        outputs.loc[outputs.index==m,'fixedCharge']=ut.fixed_charge
        #outputs.loc[outputs.index==m,'tieredPrices']=ut.prices

        #TODO: write the tiers. Requires making 3 new columns
        outputs.loc[outputs.index==m,'conserveStatus'] = res_reduction

        update_rates = False


        # for each class, calculate the bill difference
        #ped_class_demands = city.get_class_demands(this_baseline)
        #perc_change_price = []
        #for this_demand in ped_class_demands:

        #    this_bill_change = ut.get_bill(this_demand/748)-ut.get_baseline_bill(this_demand/748)
        #    perc_change_price.append(this_bill_change/ut.get_baseline_bill(this_demand/748))

        # first compute baseline price


        #ped_class_demands = city.get_total_household_demands(this_baseline)
        #mean_demand = ped_class_demands.mean()

       # bill_difference = ut.get_bill(mean_demand/748)-ut.get_baseline_bill(mean_demand/748)

       # percentage_change_price = bill_difference/ut.get_baseline_bill(mean_demand/748)


        #percentage_change_quantity = np.multiply(PED,perc_change_price)

        # now make adjustments based on conservation, and price changes
        adjusted_baseline = this_baseline*(1-res_reduction)#*(1-percentage_change_quantity)
        percentage_change_demand = (city.get_utility_demand_no_ped(adjusted_baseline)-city.get_utility_demand(adjusted_baseline, ut))/city.get_utility_demand_no_ped(adjusted_baseline) *100
        print("perc change price: ",percentage_change_demand)

        # calculate my other demand
        adjusted_other = this_other*(1-nonres_reduction)

        outputs.loc[outputs.index==m,'pedReduction'] = percentage_change_demand
        ut.demand = city.get_utility_demand(adjusted_baseline,ut)/1000000 + adjusted_other
        # this is setting the utility demand in MG

        # write the demand
        outputs.loc[outputs.index==m,'totalDemand'] = ut.demand
        outputs.loc[outputs.index==m,'residentialDemand'] = city.get_utility_demand(this_baseline,ut)/1000000
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

                if this_fee_passthrough=="zero_threshold" :
                    # increase the bills also
                    ut.set_fixed_charge(base_charge+ build_monthly_cost/city.counts.sum() + (market_buy*market_cost)/city.counts.sum())

                    print("buying: ",market_buy," mg of water on the market at a cost of $",market_buy*market_cost)
                    outputs.loc[outputs.index==m,'fixedCharge']= ut.fixed_charge
                    #outputs.loc[m,'monthlyCost']= outputs.loc[m ,'monthlyCost']+ market_buy*market_cost
                    outputs.loc[m:,'surchargeIncrease'] =  (market_buy*market_cost)/city.counts.sum()
                elif this_fee_passthrough=="high_threshold":

                    tier_use = city.get_tier_demands(this_baseline,ut)/748
                    tier_use_reduced = city.get_tier_demands(this_baseline*(1-res_reduction),ut)/748

                    # so right now, we know how much is being used by each tier
                    old_tier=ut.prices[1]

                    new_tier = (market_buy*market_cost-ut.prices[2]*tier_use_reduced[2]+ut.prices[2]*tier_use[2]+ut.prices[1]*tier_use[1])/tier_use_reduced[1]
                    #new_tier = (market_buy*market_cost+(tier_use[1]*old_tier))/(tier_use_reduced[1])


                    ut.set_tier_prices([10.03,new_tier,13.78,16.74])
                    outputs.loc[outputs.index==m,'tieredPrices']=new_tier


        # first write a abseline bill, this is assuming res_reduction = 0 and the bill is normalo

        this_bills = ut.calculate_hh_bills(this_baseline,res_reduction,percentage_change_quantity,city)
        hh_bills.loc[m] = this_bills

        # do the same for demands. Order is final,raw,mand,ped
        this_demands = ut.calculate_hh_demand(this_baseline,res_reduction,percentage_change_quantity,city)

        hh_demand.loc[m] = this_demands

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
    revstring = str(parameter_list.index[p]) + "_rev_contribs.csv"

    outputs.to_csv(repo_home / 'outputs'/'santa_cruz'/ 'experiments'/ 'review_responses2'/ outstring)
    hh_demand.to_csv(repo_home / 'outputs'/'santa_cruz'/ 'experiments'/ 'review_responses2'/hhdstring)
    hh_bills.to_csv(repo_home / 'outputs'/'santa_cruz'/ 'experiments'/ 'review_responses2' /hhbstring)
    rev_contrib.to_csv(repo_home / 'outputs'/'santa_cruz'/ 'experiments'/ 'review_responses2' /revstring)
# now multiprocess it
Parallel(n_jobs=num_cores)(delayed(sim_function)(i) for i in tqdm(range(0,max(parameter_list.index)+1)))
