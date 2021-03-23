#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Feb 24 10:34:25 2021

@author: brachuno
"""

# Sacramento

## Demand
UWMP: https://www.cityofsacramento.org/~/media/Corporate/Files/DOU/2015%20UWMP%20June%202016Appendices.pdf
Water district is basically just the city limits(according to UWMP)

Serves: 135,830 *connection* as of 2015
Population: 480,000
GPCD: 158 (Table 5-3 in UWMP)
158gpcd -> .211 CCF/day

93% of city's customers are residential, 6% commercial/industrial, 1% irrigation (pg 3-8)

Demand by sector in 2014:
Table 4-1. Retail: Historical Drinking Water Demand by Water Use Sector, AF
single family 40554
multifamily 15105
other(commercia/industrial) 18146
institutional 4598
landscap 3678
other 189
losses 12953
total: 95222

## Demographics
MHI:

# Supply

How much is groundwater vs surface water?

Surface Water Supply capacity
160MGD from Sacramento River Water Treament Plant

160MGD from Fairbairn Water Treament Plant

Groundwater is 25MGD

Total Storage for Sacramento is 52MG

Actual uses
Groundwater: 13,479 AF in  for retail use


Table 6-16 is the winner
84832 total AF supplied for retail use in 2015

made up of  (Acre Feet)
39,511 surface (sacramento)
30,956 surface (AR)
13605 (ground)
659 (mutual aid)

Based on these numbers we have
1913 MG/month of supply from Surface water

and

369.4 MG/month of supply from Groundwater

They estimate that "It is expected that the distribution of water demand by sector type will not change significantly in
the future. " Pg 4-3. and 10% losses

and in 2015, the breakdown is
36,024 for single fmaily
14,657 for multifamily
17,054 commercialindustrial
3,938 for institutional/governmental
3,481 for landscape
102 other
9,639 losses

which works out to:

59.7 % single/multifamily
28.9 %institutional/commercia/landscape etc...
11.3 % loss

So then make demand of 17054+2928+3481+102+9639 = 33204 / 12 = 2767 for the dummy
demand in acre feet
2767 --> 901 MG/month




# Triggers
Table 8-1
stage 1 is a 20% supply reduction
stage 2 is a  30%
3 is 40%
4 is 50%

# Conservation
Stage 1 triggers 10-20% reduction

2 triggers a 30% reduction
3 40%
4 50%

# Revenues
"Most of hte city's water revenue is generated frm hard costs and not volumetric usage'





# Valiation notes
in 2015, the monthly for residential multifamily and single family was 1377MG/month
from the 2015 UWMP table 4-2.

If you run this on the 'sacramento processed drought 2015' baseline data, you end
up with a monthly average water use of 1457 which is pretty damn close!?