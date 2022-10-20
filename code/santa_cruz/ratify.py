#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""

@author:adam
"""
import numpy as np
import pandas as pd

# NOTE: use py surcharges.py -v in terminal to run all doctests

# function ratify: create bill from water use (num), rate scheme (r), tiered with widths (t) or untiered
def ratify(num, r, t):
    """
    >>> ratify(11,[11.26, 10.6, 11.58, 13.64, 16.83],[6,8,10])
    142.13
    >>> ratify(9,[11.26, 10.6, 11.58, 13.64, 16.83],[6,8,10])
    111.66
    >>> ratify(7,[11.26, 10.6, 11.58, 13.64, 16.83],[6,8,10])
    86.44
    >>> ratify(6,[11.26, 10.6, 11.58, 13.64, 16.83],[6,8,10])
    74.86
    >>> ratify(0,[11.26, 10.6, 11.58, 13.64, 16.83],[6,8,10])
    11.26
    >>> ratify(11,[11.26, 10.6],[6,8,10])
    127.86
    >>> ratify(11,[11.26],[6,8,10])
    11.26
    """
    bill = r[0]
    thres = num - t[0]
    if len(r) > 1:
        if len(r) == 2:
            bill += num*r[1]
        else:
            if thres >= 0:
                bill += t[0]*r[1]
                num = thres
                thres = num - (t[1] - t[0])
                if len(r) > 2:
                    if thres >= 0:
                        bill += (t[1]-t[0])*r[2]
                        num = thres
                        thres = num - (t[2] - t[1])
                        if len(r) > 3:
                            if thres >= 0:
                                bill += (t[2] - t[1]) * r[3]
                                num = thres
                                if num >= 0:
                                    bill += num * r[4]
                            else:
                                if num > 0:
                                    bill += num * r[3]
                    else:
                        if num > 0:
                            bill += num * r[2]
            else:
                if num > 0:
                    bill += num*r[1]
    return bill


# create tiered buckets from water use based upon categorization where water is the water use and t is the widths assuming 4 buckets
def create_buckets(water, t):
    """
    >>> create_buckets([1,4,7,9,11],[6,8,10])
    {1: [1, 4], 2: [7], 3: [9], 4: [11]}
    >>> create_buckets([1,4,7,9,11,6,6,0,15,10,8],[6,8,10])
    {1: [1, 4, 6, 6, 0], 2: [7, 8], 3: [9, 10], 4: [11, 15]}
    >>> create_buckets([1],[6,8,10])
    {1: [1], 2: [], 3: [], 4: []}
    """
    buckets = {}
    buckets[1] = []
    buckets[2] = []
    buckets[3] = []
    buckets[4] = []
    for val in water:
        if val <= t[0]:
            buckets[1].append(val)
        elif val <= t[1]:
            buckets[2].append(val)
        elif val <= t[2]:
            buckets[3].append(val)
        elif val > t[2]:
            buckets[4].append(val)

    return buckets


# create split water use from water use based upon categorization where water is the water use and t is the widths assuming 4 buckets
def split_wateruse(water, t):
    """
    >>> split_wateruse([1,4,7,9,11],[6,8,10])
    ([5, 23, 5, 3, 1], {1: [1, 4, 6, 6, 6], 2: [1, 2, 2], 3: [1, 2], 4: [1]})
    >>> split_wateruse([1,4,7,9,11,6,6,0,15,10,8],[6,8,10])
    ([11, 53, 11, 7, 6], {1: [1, 4, 6, 6, 6, 6, 6, 0, 6, 6, 6], 2: [1, 2, 2, 2, 2, 2], 3: [1, 2, 2, 2], 4: [1, 5]})
    >>> split_wateruse([1],[6,8,10])
    ([1, 1, 0.0, 0.0, 0.0], {1: [1], 2: [], 3: [], 4: []})
    """
    buckets = {}
    buckets[1] = []
    buckets[2] = []
    buckets[3] = []
    buckets[4] = []
    for val in water:
        if val > t[0]:
            buckets[1].append(t[0])
            if val > t[1]:
                buckets[2].append(t[1]-t[0])
                if val > t[2]:
                    buckets[3].append(t[2]-t[1])
                    buckets[4].append(val-t[2])
                else:
                    buckets[3].append(val-t[1])
            else:
                buckets[2].append(val-t[0])
        else:
            buckets[1].append(val)

    wateruse_by_bucket = [len(water)]+[np.sum(buckets[i]) for i in range(1,5)]

    return wateruse_by_bucket, buckets


# calculates the revenue gained from each bucket from r, the rate scheme, buckets a set of split water uses, and n, the number of bills
def ratify_buckets(buckets,r,n):
    """
    >>> ratify_buckets({1: [1, 4, 6, 6, 6, 6, 6, 0, 6, 6, 6], 2: [1, 2, 2, 2, 2, 2], 3: [1, 2, 2, 2], 4: [1, 5]}, [11.26, 10.6, 11.58, 13.64, 16.83], 11)
    ({1: [10.6, 42.4, 63.599999999999994, 63.599999999999994, 63.599999999999994, 63.599999999999994, 63.599999999999994, 0.0, 63.599999999999994, 63.599999999999994, 63.599999999999994], 2: [11.58, 23.16, 23.16, 23.16, 23.16, 23.16], 3: [13.64, 27.28, 27.28, 27.28], 4: [16.83, 84.14999999999999]}, [123.86, 561.8000000000001, 127.38, 95.48, 100.97999999999999])
    """
    buckets2 = buckets
    buckets2[1] = [i * r[1] for i in buckets[1]]
    buckets2[2] = [i * r[2] for i in buckets[2]]
    buckets2[3] = [i * r[3] for i in buckets[3]]
    buckets2[4] = [i * r[4] for i in buckets[4]]

    revenue_by_bucket = [n*r[0]]+[np.sum(buckets2[i]) for i in range(1,5)]

    return buckets2, revenue_by_bucket


#calculate curtailed water use from current water use
def curtail_water_use(water_use, curtailment):
    """
    >>> curtail_water_use([5,6,7], 0.1)
    [4.5, 5.4, 6.3]
    """
    water_use_curtail = [i * (1 - curtailment) for i in water_use]  # create list of curtailed water use
    return water_use_curtail


# main function to calculate tiered, single volumetric, and flat surcharges for tiered rate
# where water_use is the water use, water_use_curtail is curtailed water use, r is the rate scheme, t is the width structure, perc_flat is the constant of fixed costs of total revenue, perc_deficit is the percent of deficit expected
def calc_tiered_surcharges(water_use, water_use_curtail, r, t, perc_flat, perc_deficit):
    buckets = create_buckets(water_use_curtail, t)  # place water use into tiered buckets from curtailed water use
    use_by_bucket, split_use = split_wateruse(water_use_curtail, t)  # split the curtailed water use into tiers

    bills = [ratify(i,r,t) for i in water_use] # bills for the original water use
    bills_curtail = [ratify(i,r,t) for i in water_use_curtail] # bills after curtailment

    revenue = np.sum(bills) # revenue before curtailment
    revenue_curtail = np.sum(bills_curtail) # revenue after curtailment

    deficit = (revenue-revenue_curtail)*perc_deficit # deficit due to curtailment

    # flat rate delta for current r implemented
    flat = deficit / len(bills_curtail)

    # single volumetric delta for current r implemented
    single_volum = [deficit * perc_flat / len(bills_curtail), deficit * (1 - perc_flat) / np.sum(water_use_curtail)]

    # proportional tiered delta for current r implemented
    rated_buckets, revenue_by_bucket = ratify_buckets(split_use,r,len(bills_curtail)) # return revenue garnered with each tiered bucket
    proportion_revenue = [i/revenue_curtail for i in revenue_by_bucket] # calculate the proportion of revenue each tier is responsible for
    split_deficit = [i*deficit for i in proportion_revenue] # calculate the amount of deficit that each tier is responsible for making up
    delta = [i / j for i, j in zip(split_deficit, use_by_bucket)] # divide split deficit by the number of equivalent units in each bucket, the first value of use_by_bucket being the number of bills

    return delta, single_volum, flat, deficit


# main function to calculate single volumetric and flat surcharges for a single volumetric rate
# where water_use is the water use, water_use_curtail is curtailed water use, r is the rate scheme, t is the width structure, perc_flat is the constant of fixed costs of total revenue, and perc_deficit is the percent of deficit expected
def calc_single_volum_surcharge(water_use, water_use_curtail, r, t, perc_flat, perc_deficit):
    bills = [ratify(i, r, t) for i in water_use]  # bills for the original water use
    bills_curtail = [ratify(i, r, t) for i in water_use_curtail]  # bills after curtailment

    revenue = np.sum(bills)  # revenue before curtailment
    revenue_curtail = np.sum(bills_curtail)  # revenue after curtailment

    deficit = (revenue - revenue_curtail)*perc_deficit  # deficit due to curtailment

    # flat rate delta for current r implemented
    flat = deficit / len(bills_curtail)

    # single volumetric
    single_volum = [deficit * perc_flat / len(bills_curtail), deficit * (1 - perc_flat) / np.sum(water_use_curtail)]

    return single_volum, flat, deficit


# main function to calculate flat surcharge for a flat rate
# where water_use is the water use, water_use_curtail is curtailed water use, r is the rate scheme, t is the width structure, and perc_deficit is the percent of deficit expected
def calc_flat_surcharge(water_use, water_use_curtail, r, t, perc_deficit):
    bills = [ratify(i,r,t) for i in water_use] # bills for the original water use
    bills_curtail = [ratify(i,r,t) for i in water_use_curtail] # bills after curtailment

    revenue = np.sum(bills) # revenue before curtailment
    revenue_curtail = np.sum(bills_curtail) # revenue after curtailment

    deficit = (revenue-revenue_curtail)*perc_deficit # deficit due to curtailment

    # flat rate delta
    flat = deficit/len(bills_curtail)

    return flat, deficit


t = [6,8,10] # widths, can be changed
curtailment = 0.30946981 # percent curtail, can be changed
perc_flat = 0.1520296 # percent flat of bills, can be changed (0.1520296 for modeled demand data, 0.12153 for actual SC Data,)
perc_deficit = 1 # percent of deficit expected, can be changed (1 for control)

# load data for assessment

# Modeled Demand Data
water_use_csv = pd.read_excel("~/Documents/__college/reseach_stuff/hr2w_droughts/data/santa_cruz_billing/estimated_household_demand.xlsx", sheet_name="Sheet1")
water_use_csv['HH Count'] = [2365, 1648, 1456, 1285, 1424, 1027, 1018, 1077, 874, 1931, 2352, 4064, 3110, 2091, 3263, 4895] # make sure these are ints, .astype('int')

water_use = []
tot = 0
for index, row in water_use_csv.iterrows():
    water_use += list(row[1:])*water_use_csv['HH Count'][index]
    tot += water_use_csv['HH Count'][index]

'''
#Actual Santa Cruz Data
water_use_csv = pd.read_csv('wateruse_SC_sample2.csv') # load water use from csv
water_use = water_use_csv['Water Use'] # create list of all water use from csv
'''

#calculate curtailed water use
water_use_curtail = curtail_water_use(water_use, curtailment)

# possible surcharges for tiered rate structure
# rate structures to test, uncomment tiered rate scheme you want to represent r
r_i = [11.26, 10.6, 11.58, 13.64, 16.74] # increasing tiered (same for both datasets)[10.6,11.58,13.64,16.74]
r_d = [11.26, 11.38, 10.40, 8.34, 5.15] # decreasing tiered ([11.26, 11.38, 10.40, 8.34, 5.15] for modeled demand, [11.26, 12.89, 11.91, 9.85, 6.66] for SC 2010 sample)
print(" Increasing Tiered Surcharge ($) , Single Volumetric Surcharge ($) & Flat Surcharges ($):\n", calc_tiered_surcharges(water_use, water_use_curtail, r_i, t, perc_flat, perc_deficit))


print(" Decreasing Tiered Rate, Decreasing Tiered, Single Volumetric & Flat Surcharges:\n", calc_tiered_surcharges(water_use, water_use_curtail, r_d, t, perc_flat, perc_deficit))

# now for single volumetric delta and flat for single volumetric rate
r_f = [11.26, 10.99] # [11.26, 10.99] for modeled demand, [11.26, 11.74] for SC 2010 Data,
print(" Single Volumetric Rate, Single Volumetric & Flat Surcharges:\n", calc_single_volum_surcharge(water_use,water_use_curtail,r_f,t,perc_flat,perc_deficit))

# now for flat delta for flat rate
#r = [74.06] # 74.06 for modeled demand, 92.65 for SC 2010 Data
#print(" Flat Rate, Flat Surcharge:\n", calc_flat_surcharge(water_use,water_use_curtail,r,t,perc_deficit), "\n") # NOTE: makes sense there is no deficit here as a constant flat rate should account for drought periods long term as costs are divided evenly anyways

#print("Should Return (perc_deficit = 1, curtailment = 0.05, Modeled SC Demand Data): \n Increasing Tiered Rate, Increasing Tiered, Single Volumetric & Flat Surcharges: \n ([0.5711254332790036, 0.5376491645432889, 0.5873563514538955, 0.6918428872047608, 0.8536448527607131], [0.5435552492745868, 0.5584749928550595], 3.575325129281316, 1453584.1845606118) \n Decreasing Tiered Rate, Decreasing Tiered, Single Volumetric & Flat Surcharges: \n ([0.42683396743722135, 0.43138281966568215, 0.39423385979992037, 0.3161452298780129, 0.1952215748047682], [0.41120590920519484, 0.42249286988178547], 2.7047753148412865, 1099653.4520018734) \n Single Volumetric Rate, Single Volumetric & Flat Surcharges:\n ([0.4773805792398873, 0.49048393136841895], 3.1400502220612783, 1276618.8182812333) \n Flat Rate, Flat Surcharge:\n (0.0, 0.0)")

if __name__ == '__main__':
    import doctest
    doctest.testmod()