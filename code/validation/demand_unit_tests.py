#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Feb 12 12:10:32 2021

@author: brachuno
"""

# packages
import os, sys
sys.path.append("/Users/brachuno/Documents/__college/reseach_stuff/water-equity/code")

import datetime
import math
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from wrtools import *
# test the demand functions

dem = DemandFunction(0.3)

dem.set_bins([1,1],[10,20],[1,1])

dem.set_dummy_demands([1])
dem.get_utility_demand()

dem.get_total_class_demands()


# price unit tests
pp = RateStructure(22.6)

pp.set_tier_prices([6.66,9.18])

pp.set_tiers([6,999999])# set an upper limit

pp.get_bill(0) # should be 22.6
pp.get_bill(1) #29.26
pp.get_bill(5) #55.9
pp.get_bill(6) #62.56
pp.get_bill(7) #71.74
pp.get_bill(20)  #191.08
