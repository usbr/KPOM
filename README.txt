# 2019-02-07 - MM Model Comments and Updates
-Removed Drain No 2 reach
	Affected initialization rules 25 and 27
-Renamed A1 Northeastern SW Returns as Area 1 Return Flows
	Affected initialization rules 25 and 27
-Renamed Refuge Returns as Refuge Return Flows
	Affected Rule 31
	Affected initialization rule 25
	



# 2019-02-05 - DB Model Comments and Updates
- Lots of slots on UKL object that appear to be left over from Basin Study model -> remove what isn't needed
- Added input slots to objects in an 'Inputs' slot group
	Convention is Inpt[InputType: Obs or Fcst] [SlotType: e.g. Inflow, Outflow]
	1) UKL
		InptFcst Net Inflow - Daily net inflow forecast timeseries (e.g. CNRFC deterministic forecast or ensemble forecast)
		InptFcst Outflow - Daily outflow forecast timeseries
		InptFcst Seasonal Net Inflow - Seasonal volume forecast 
		InptObs Net Inflow - Daily net inflow timeseries - from calcs
		InptObs Outflow - Daily outflow timeseries
		InptObs Pool Elevation - Daily pool elevation
	2) 11501000 Sprague nr Chiloquin
		InptObs Gage Inflow
		InptFcst Gage Inflow
	3) 11502500 Williamson River nr Chiloquin
		InptObs Gage Inflow
		InptFcst Gage Inflow
	4) 11509500 Klamath at Keno
		InptObs Gage Inflow
		InptFcst Gage Inflow
	5) 11510700 Klamath bl JC Boyle Pwrplant
		InptObs Gage Inflow
		InptFcst Gage Inflow
	6) 11516530 Klamath bl Iron Gate
		InptObs Gage Inflow
		InptFcst Gage Inflow
	7) 11520500 Klamath nr Seiad Valley
		InptObs Gage Inflow
		InptFcst Gage Inflow
	8) 11523000 Klamath at Orleans
		InptObs Gage Inflow
		InptFcst Gage Inflow
	9) 11530500 Klamath nr Klamath
		InptObs Gage Inflow
		InptFcst Gage Inflow
	10) 11517500 Shasta nr Yreka
		InptObs Gage Inflow
		InptFcst Gage Inflow
	11) 11519500 Scott nr Fort Jones
		InptObs Gage Inflow
		InptFcst Gage Inflow
	12) 11521500 Indian Creek nr Happy Camp
		InptObs Gage Inflow
		InptFcst Gage Inflow
	13) 11522500 Salmon at Somes Bar
		InptObs Gage Inflow
		InptFcst Gage Inflow
	14) 11525500 Trintiy River at Lewiston
		InptObs Gage Inflow
		InptFcst Gage Inflow
	15) 11530000 Trinity at Hoopa
		InptObs Gage Inflow
		InptFcst Gage Inflow	
	16) A Canal
		InptObs Diversion
		InptFcst Diversion
	17) North Canal
		InptObs Diversion
		InptFcst Diversion	
	18) Ady Canal
		InptObs Diversion
		InptFcst Diversion	
	19) Div To LRDC
		InptObs Diversion
		InptFcst Diversion	
	20) Lost To LRDC
		InptObs Diversion
		InptFcst Diversion		
	21) Miller Hill Pump
		InptObs Diversion
		InptFcst Diversion	
	22) Station 48
		InptObs Diversion
		InptFcst Diversion	
	23) Pumping Plant D
		InptObs Diversion
		InptFcst Diversion	
	24) Area 1 North of LRDC
		InptObs Diversion
		InptFcst Diversion		
	25) Area 1 South of LRDC
		InptObs Diversion
		InptFcst Diversion	
	26) Area K
		InptObs Diversion
		InptFcst Diversion	
	27) Area2
		InptObs Diversion
		InptFcst Diversion	
	28) Lost River Irrigation Depletions
		InptObs Diversion
		InptFcst Diversion	
	29) Williamson To UKL Gain
		InptObs Local Inflow
		InptFcst Local Inflow
	30) Lake Ewuana Gain
		InptObs Local Inflow
		InptFcst Local Inflow	
	31) Keno to Boyle Gain
		InptObs Local Inflow
		InptFcst Local Inflow
	32) Boyle Gage to Copco Gain
		InptObs Local Inflow
		InptFcst Local Inflow
	33)Copco to Iron Gate Gain
		InptObs Local Inflow
		InptFcst Local Inflow
	34) Cottonwood Willow and Bogus Gains
		InptObs Local Inflow
		InptFcst Local Inflow
	35) Horse Dona Beaver and Humbug Gains
		InptObs Local Inflow
		InptFcst Local Inflow
	36) Klamath Elk and Clear Gain
		InptObs Local Inflow
		InptFcst Local Inflow
	37) Klamath Bluff and Red Cap Gain
		InptObs Local Inflow
		InptFcst Local Inflow
	38) Klamath Blue Gain
		InptObs Local Inflow
		InptFcst Local Inflow
	39) Lewiston to Hoopa Gain
		InptObs Local Inflow
		InptFcst Local Inflow
	40) Lost River Gains
		InptObs Local Inflow
		InptFcst Local Inflow
	41) Gerber
		InptFcst Net Inflow - Daily net inflow forecast timeseries (e.g. CNRFC deterministic forecast or ensemble forecast)
		InptFcst Outflow - Daily outflow forecast timeseries
		InptObs Net Inflow - Daily net inflow timeseries - from calcs
		InptObs Outflow - Daily outflow timeseries
		InptObs Pool Elevation - Daily pool elevation
	42) Clear
		InptFcst Net Inflow - Daily net inflow forecast timeseries (e.g. CNRFC deterministic forecast or ensemble forecast)
		InptFcst Outflow - Daily outflow forecast timeseries
		InptObs Net Inflow - Daily net inflow timeseries - from calcs
		InptObs Outflow - Daily outflow timeseries
		InptObs Pool Elevation - Daily pool elevation
	43) E and EE Pump
		InptObs Outflow
		InptFcst Outflow
	43) F and FF Pump
		InptObs Outflow
		InptFcst Outflow

- UKLShortTermForecast object still exists - needed?
- Area2 name - include a space to match Area 1 and Area K?
- Gerber Reservoir and Clear Lake - inflow or net inflow. If inflow, how to account for evap, precip?
- Other slots left over in Gerber and Clear objects from basin study
- Data objects left over from SRO and basin study
- Where to put reference data? E.g. historical demands? On objects?