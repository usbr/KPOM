
The official version to use for testing the KROM is the ForTesting_20200909 which is the same as the version 28_DN_20200909.

Version 28_DN 9/9/2020 and ForTesting
------------------------
Imported new data for Ady Canal, Ady to Refuge, and Ady to Ag from the version 29 where BM and DF fixed these values to match the backup values. 

Version 28_MF 8/25/2020
------------------------
Fixed additional leap year issues as described in the PACalcEdits.doc and copied here: 

Sheet: Accretion Forecasts
Column: Lost River, F/FF, & Keno to IG Accretion
•	These columns use the values from the WY Day Number column to look up values from the corresponding, right adjacent tables. The part of the cell formula that performs this lookup is the HLOOKUP function. In this HLOOKUP function, the WY Day Number must add one to its value since the first row is the header. Currently, it does not add one and references the value from the prior day. See the HLOOKUP function setup in the Lake Ewuana Accretion column formulas (i.e. needs to be HLOOKUP(lookup val, table array, row index + 1,)).
Column: Lost River Accretion
•	After September 30, 2020, the cell formulas stop using the AVERAGE function. This should be added back in since all the other accretion columns use it past that date. 

Sheet: Inflow Processing
Column: Day Number
•	On September 30, 2020, the Day Number is 1. Instead, it should be 365 since it is the last day of the water year. October 1, 2020 should have its Day Number as 1. 

Sheet: InflowMaxMin
•	The maximum and minimum historical inflows are hard input and repeat each year (i.e. Max on Feb 1, 2019 is equal to Max on Feb 1, 2020). Thus, instead of having a table that spans the run period, have a table with 365 values that are row indexed by the day number. The day number is the value that Inflow Processing sheet uses to reference this table. Additionally, with this new table setup, the columns on the Inflow Processing sheet always reference the same table array instead of having issues switching between table arrays each new water year.  


Version 28_JC 8/19/2020
------------------------ Fixed some leap year issues and a few other things as identified by MF. This file was provided by JC to the team.

Version 28_Original	7/22/2020
------------------------
This was the original v28, which: Added Sukraw Well offset logic to following tabs > columns: UKL Mass Balance > Z; IGD Calc > AY; Ag Demand > X; USBR Daily > AG, AH, AJ, AK, AL, AO, AP, AX, AY, AZ




