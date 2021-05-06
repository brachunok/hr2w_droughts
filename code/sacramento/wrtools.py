import numpy as np
import pandas as pd

class City:
    def __init__(self, demand,income_elasticity):
        self.demand = demand # current demand for the city
        #self.inflow = inflow # current inflow
        self.income_elasticity = income_elasticity
    def set_restriction(self,restriction):
        self.restriction = restriction

    def set_bins(self, counts,incomes,household_sizes,leaks):
        # counts are the number of each class, income are the incomes
        # of each class
        self.counts = counts # number of each household
        self.income = incomes
        self.household_sizes = household_sizes
        self.i_bar = np.mean(incomes)
        self.income_percentages = np.divide(self.income-self.i_bar,self.i_bar)
        # ^^ percentage change from MHI
        self.leakage_volumes = leaks

    def set_dummy_demands(self,demands):
        self.demands = demands
        # IN THE FUTURE, this will be

    def set_demands(self,demands):
       # in the future, this will take in
       # the current billing structure and the price
       # elasticity of water deman
       self.demands = demands

    def get_class_demands(self, monthly_baseline):

        # baseline that comes in is average R-GPCD

        # now calculate the adjusted quantity based on the
        #income elasticity and the percentage changes in income

        baseline_adjusted = monthly_baseline + monthly_baseline*self.income_percentages*self.income_elasticity

        d = 30.4*baseline_adjusted
        return(d)
        # this is the monthly demand for one person from each class in GALLONS

    def get_total_household_demands(self,baseline):

        c = self.get_class_demands(baseline) + self.leakage_volumes
        return(np.multiply(c,self.household_sizes))
        # this is the monthly household demand for one household from
        # each class

    def get_total_demand_by_class(self,baseline):
        d = self.get_total_household_demands(baseline)
        return(np.multiply(d,self.counts))
        # this is the total monthly demand broken down by class

    def get_utility_demand(self,baseline):
        d = self.get_total_demand_by_class(baseline)

        return(sum(d))
    # this is a monthyly demand in gallons for everyone

class Reservoir:
    def __init__(self, capacity,env_release):
        self.capacity = capacity
        self.env = env_release


    def set_volume(self,volume):
        self.volume = volume
        # this is only for setting the initial volume

    def make_fixed_release_by_volume(self,inflows,demands,thresholds,releases):
        # here, we make releases based on a givn volume in the reservoir
        # ie, if we have x amount release y, above x amount, release more etc..
        print("b")

    def make_fixed_environmental_release(self,inflows,demands):
        # here, we make releases based on a givn volume in the reservoir
        # ie, if we have x amount release y, above x amount, release more etc..
        self.volume=self.volume - self.env + np.sum(inflows) - np.sum(demands)

    def make_fixed_release_by_date(self,inflows,demands,wkindex,indexset,releases):
        # this makes a fixed release based on the week of the year.
        # wkindex is the current week's index e.g. 15
        # index set are the week cutoffs. e.g. 0,13,45,53
        # releases are the releases you make below the respective index cutoff
        # e.g. 357,490,357

        # in this case we are releaseing 357 between weeks 0 and 13 and 45-53
        # and 490 in 13-45
        # this also uses inflows and demands to update volumes too.
        inflow = np.sum(inflows)
        demand = np.sum(demands)
        release = 0
        n = -1

        # get my weekindex down to a number between 0 and 52
        div = wkindex/52
        div = math.floor(div)

        wkindex = wkindex - (div* 52)

        for w in indexset:
            if wkindex > w:
                release = releases[n+1]
                n=n+1

#        # now do the rules
#        if (self.volume + inflow) < (release):
#            release = self.volume + inflow
#
#        elif (self.volume + inflow) < (self.capacity + demand):
#            release = release
#
#        else:
#            release = self.volume + inflow - self.capacity
#
#        # update volume based on total released
        self.volume = self.volume - release + inflow
#
#        # if we have multiple demands, allocate the release proportionally
#        # to what they asked for
#        if (demand>0):
#            release_values = release * (demands/demand)
#        else:
#            release_values = release
        release_values = release * (demands/demand)
        return(release_values)

    def make_sop(self, inflows, demands):
        # inflows are arrays of each inflow
        # demands are arrays of each demand.
        # release is split up in the same order as the demand
        # what we do here, is the ``standard operating policy``
        # here page 10: https://apps.dtic.mil/dtic/tr/fulltext/u2/a315845.pdf
        # if my storage + my sum of inflows < sum of demands
        #      then I release storage + inflows
        # elif my storage + my sum of inflows < capacity + demand
        #      then release = demand
        # else
        #      release = volume now + inflows - capacity. AKA release to get down to capacity
        #
        # storage gets updated to storage now - release
        # then release gets divided up proportionally based on the demands

        # sum up all my inflows and demands
        inflow = np.sum(inflows)
        demand = np.sum(demands)
        release = 0

        # now do the rules
        if (self.volume + inflow) < (demand):
            release = self.volume + inflow

        elif (self.volume + inflow) < (self.capacity + demand):
            release = demand

        else:
            release = self.volume + inflow - self.capacity

        # update volume based on total released
        self.volume = self.volume - release + inflow

        # if we have multiple demands, allocate the release proportionally
        # to what they asked for
        #if (demand>0):
        #    release_values = release * (demands/demand)
        #else:
        #    release_values = release
        release_values = release
        return(release_values)

    def check_sop(self, inflows, demands):
        # this is the same as the above method
        # except this one doesn't change anything, it only
        # sees how much is available
        #
        # sum up all my inflows and demands
        inflow = np.sum(inflows)
        demand = np.sum(demands)
        release = 0

        # now do the rules
        if (self.volume + inflow) < (demand):
            release = self.volume + inflow

        elif (self.volume + inflow) < (self.capacity + demand):
            release = demand

        else:
            release = self.volume + inflow - self.capacity

        # if we have multiple demands, allocate the release proportionally
        # to what they asked for
        release_values = release * (demands/demand)
        return(release_values)

    def check_excess(self, inflows, demands):
        # this is the same as the above method
        # except this one doesn't change anything, and
        # sees how much is left after we did a hypothetical release
        #
        # sum up all my inflows and demands
        inflow = np.sum(inflows)
        demand = np.sum(demands)
        release = 0
        excess = 0
        # now do the rules
        if (self.volume + inflow) < (demand):
            # this is where we go if we don't have enough coming in
            # and on hand to deal with demand
            release = self.volume + inflow
            excess = 0

        elif (self.volume + inflow) < (self.capacity + demand):
            # this is the 'we just have enough on hand but aren't'
            # overflowing. part of code
            release = demand
            excess = self.volume + inflow - demand

        else:
            # this is the 'we have way too much for the given amount of'
            #
            release = self.volume + inflow - self.capacity

            # in this case, the excess (I think) is just how much is available
            # minus the demand
            excess = self.volume + inflow - demand
        # if we have multiple demands, allocate the release proportionally
        # to what they asked for
        if (demand>0):
            excess = excess * (demands/demand)
        else:
            excess = excess
        return(excess)

class AllocatedReservoir(Reservoir):
    def __init__(self,capacity,allocations):
        super().__init__(capacity)
        self.allocations = allocations

    # allocated reservoir is a subclass of reservoir
    # which makes decisiosn and checks decisions based on the allocations.
    # allocations are going to be an array
    # each entry is the amount the ith demand has access to
    # the order needs to be the same throughout.
    # e.g. [.1,.9.] or [.1,.1,.8]. The array needs to sum to 1


    def make_sop(self, inflows, demands):
        # inflows are arrays of each inflow
        # demands are arrays of each demand.
        # release is split up in the same order as the demand
        # what we do here, is the ``standard operating policy``
        # here page 10: https://apps.dtic.mil/dtic/tr/fulltext/u2/a315845.pdf
        # if my storage + my sum of inflows < sum of demands
        #      then I release storage + inflows
        # elif my storage + my sum of inflows < capacity + demand
        #      then release = demand
        # else
        #      release = volume now + inflows - capacity. AKA release to get down to capacity
        #
        # storage gets updated to storage now - release
        # then release gets divided up proportionally based on the demands

        # sum up all my inflows and demands
        inflow = np.sum(inflows)
        demand = np.sum(demands)
        release = 0

        # now do the rules
        if (self.volume + inflow) < (demand):
            release = self.volume + inflow

        elif (self.volume + inflow) < (self.capacity + demand):
            release = demand

        else:
            release = self.volume + inflow - self.capacity

        # update volume based on total released
        self.volume = self.volume - release + inflow

        # if we have multiple demands, allocate the release proportionally
        # to what they asked for
        if (demand>0):
            release_values = release * (demands/demand)
        else:
            release_values = release

        return(release_values)

    def check_sop(self, inflows, demands):
        # this is the same as the above method
        # except this one doesn't change anything, it only
        # sees how much is available
        #
        # sum up all my inflows and demands
        inflow = np.sum(inflows)
        demand = np.sum(demands)
        release = 0

        # now do the rules
        if (self.volume + inflow) < (demand):
            release = self.volume + inflow

        elif (self.volume + inflow) < (self.capacity + demand):
            release = demand

        else:
            release = self.volume + inflow - self.capacity

        # if we have multiple demands, allocate the release proportionally
        # to what they asked for
        release_values = release * (demands/demand)
        return(release_values)

    def check_excess(self, inflows, demands):
        # this is the same as the above method
        # except this one doesn't change anything, and
        # sees how much is left after we did a hypothetical release
        #
        # sum up all my inflows and demands
        inflow = np.sum(inflows)
        demand = np.sum(demands)
        release = 0
        excess = 0
        # now do the rules
        if (self.volume + inflow) < (demand):
            # this is where we go if we don't have enough coming in
            # and on hand to deal with demand
            release = self.volume + inflow
            excess = 0

        elif (self.volume + inflow) < (self.capacity + demand):
            # this is the 'we just have enough on hand but aren't'
            # overflowing. part of code
            release = demand
            excess = self.volume + inflow - demand

        else:
            # this is the 'we have way too much for the given amount of'
            #
            release = self.volume + inflow - self.capacity

            # in this case, the excess (I think) is just how much is available
            # minus the demand
            excess = self.volume + inflow - demand
        # if we have multiple demands, allocate the release proportionally
        # to what they asked for
        if (demand>0):
            excess = excess * (demands/demand)
        else:
            excess = excess
        return(excess)

class Inflow:
    def __init__(self, flow):
        self.flow = flow

class GroundWater:
    def __init__(self,flow):
        self.flow = flow

class Utility:
    def __init__(self, name,irr):
        self.name = name
        self.irr = irr
        self.monthly_liability =0

    # also initialize the optins dataframe
        self.options = pd.DataFrame(columns=['name','capacity','buildtime','pbp','capex'] )
        # if i change the above, it also has to be changed in the add_options method

    # also initialize the pending dataframe
        self.pending = pd.DataFrame(columns=['name','capacity','buildtime','pbp','capex','start','end'])

    def add_option(self,name,capacity,buildtime,pbp,capex):
        data = [{'name':name,'capacity':capacity,'buildtime':buildtime,'pbp':pbp,'capex':capex}]
        new_df = pd.DataFrame(data)
        self.options = self.options.append(new_df,sort = False)
        self.monthly_costs = self.calculate_monthly_cost()
        self.options = self.options.reset_index(drop=True)

    def choose_option(self,i,time):
        # build option takes the ith element of the options and moves it to
        # the pending dataframe.
        this_row = self.options.iloc[i]
        this_row['start']=time
        this_row['end']=time+this_row['buildtime']
        self.pending = self.pending.append(this_row,ignore_index=True)
        self.options.drop(i,inplace=True)
        self.options = self.options.reset_index(drop=True)

    def check_pending(self,n):
        # at a given time period, check to see if we have anything which
        # should be built and
        # either (1) update the demand reduction or
        # (2) update the reservoir amount
        # I'M HERE

        # if our end time is les than our current time, remove it from pending and
        # return the additional capacity
        return_val = False
        end_list = self.pending['end'] <=n

        if end_list.any():
            # this means at least one is done
            return_val = self.pending.loc[end_list]

            #remove it from pending
            self.pending = self.pending.drop(self.pending.loc[end_list].index)

        return(return_val)


    def calculate_monthly_cost(self):
        # take in the dataframe of options
        # calculate monthly costs
        # store them in the montly_costs attribute
        costs = list()
        for i in range(self.options.shape[0]):
            a = self.a_given_p(self.options['capex'].iloc[i],n= self.options['pbp'].iloc[i])
            costs.append(a/12)
        return(costs)

    def get_cheapest_monthly(self,capacity):

        # make a mask of which options are meeting capacity
        # then use the mask to select the associated costs
        # and finally pick the minimum cost of those
        minimum = float("inf")
        row = -1

        for i in range(self.options.shape[0]):
            if (minimum >= self.monthly_costs[i])&(self.options['capacity'].iloc[i]>capacity):
                minimum = self.monthly_costs[i]
                row = i
        return(row)

    def calculate_cost_of_conservation_EVEN_by_household(self,capacity_needed,city,baseline):
        # take the amount we need, assume that amout is distributed EVENLY
        # across all individuals
        #
        # capacity needed divided by total population
        #
        # Then calculate the total revenue given they use the current amount
        # and the total revenue given they need to conserve

        average_monthly_use = baseline['mean'].mean()
        # this is in gallons per person per day

        # get how much each class is using
        current_use = city.get_total_household_demands(average_monthly_use)
        # this is the total for one household of each class given the baseline^
        # in gallons
        current_use_ccf = current_use/748

        current_revenue = list()
        for h in range(len(current_use)):
            current_revenue.append(self.get_bill(current_use_ccf[h])*city.counts[h])

        # current revenue[h] is how much revenue is coming from all houses
        # of class [h]
        # that difference is the cost of conservation
        total_current_revenue = sum(current_revenue)

        # now calculate the reduction
        total_households= sum(city.counts)

        household_reduction_volume = capacity_needed*1000000/total_households
        # multiply the capacity needed by 1000000 to make it gallons instead of
        # MG
        conservation_use = current_use - household_reduction_volume

        conservation_use_ccf = conservation_use/748
        # now calculate the new bills based on the conservation use
        conservation_revenue = list()
        for h in range(len(conservation_use)):
            conservation_revenue.append(self.get_bill(conservation_use_ccf[h])*city.counts[h])

        return(total_current_revenue-sum(conservation_revenue))

    def a_given_p(self,p,n):
        i = self.irr
        a = p *((i*(1+i)**n)/((1+i)**n-1))
        return(a)

    def set_fixed_charge(self, fixed_charge):
        self.fixed_charge=fixed_charge

    def set_tier_prices(self,prices):
        self.prices = np.array(prices)
        # volumetric prices for each tier

    def set_tiers(self,tiers):
        self.tiers = np.array(tiers)
        # the cutoffs for each tier, 0 is included. These are the upper cutoffs

    def get_bill(self,ccf):

        bill = self.fixed_charge
        remaining_volume = ccf
        t = 0

        # while the bill is above 0, we  keep going
        while remaining_volume>0:
            this_volume = min(remaining_volume,self.tiers[t])
            bill = bill + this_volume*self.prices[t]
            remaining_volume = remaining_volume-this_volume
            if (t+1)>(len(self.tiers)-1):
                t = t
            else:
                t = t+1

        return(bill)

