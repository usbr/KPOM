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


2019-03-26 - DB Model Comments and Update 

Importing a table slot with DateTime column - why can't the YYYY-MM-DD format be used?
	It can be, but has to be in MM/DD/YYYY format and RW midnight format - e.g. 3/1/2019 would be 2/29/2019 23:00 (though displayed as 2/28/2019 in Excel)
	For MonthYear date formats, MMM DD format can be used (e.g. Apr 01) - same RW midnight format applies 

In setting up Pisces RW DMI, the file path to the pdb file must use \ not /
In the Pisces DMI control file, tabs are not allowed, only spaces
The Pisces DMI doesn't allow for specifying which data are DMI'd in - defaults to StartTime EndTime, but works even if data are missing
	This behavior should work, but we should keep an eye on it for any issues this may cause
	
##-----------------------------
--Data from flowcalc.xlsx--
UKL.Misc Flows (cfs)							- What are these?
Williamson River Inflow.CNRFC Short Term (cfs)	- Do we want to keep the forecast data here or on the Stream Gage object?
Ag Sheet.Pacificorp Accretions (cfs)			- IGD Calculator 'USBR Daily' Sheet 'Accretions for PacifiCorp (cfs)' Column -> new Import Observations PacifiCorp imports data to Accretion data object
Ady Canal Remain.Diversion Request (cfs)
IGD.Outflow (cfs)
Keno to Boyle Gain.Local Inflow (cfs)			- IGD Calculator 'USBR Daily' Sheet 'Keno to IGD Accretions (BOR est.)' Column -> calculated using an initilization rule from USGS IGD - USGS Keno flows*
Keno.Outflow (cfs)								- USGS Gauge (11509500) - imported through Pisces DMI
F and FF Pump.Outflow (cfs)						- Klamath Hydromet (FPPO) - imported through Pisces DMI
Ady Canal.Diversion Request (cfs)				- USGS Gauge (11509200) - imported through Pisces DMI -> don't quite match Ady Canal values in IGD Calculator -> what data procesing happens??
North Canal.Diversion Request (cfs)				- USGS Gauge (11509105) - imported through Pisces DMI -> don't quite match North Canal values in IGD Calculator -> what data procesing happens??
Lake Ewuana Gain.Local Inflow (cfs)				-
Miller Hill Pump.Diversion Request (cfs)		-
Station 48.Diversion Request (cfs)				-
A Canal.Diversion Request (cfs)					- Klamath Hydromet (ACHO) - imported through Pisces DMI -> matches IGD values
Lost River Diversion Channel.Inflow (cfs)		- 
UKL.Pool Elevation (ft)							- USGS Gage (
UKL.Inflow (cfs)								-
Williamson River Inflow.Inflow (cfs)			-

* This is calculated as (Iron Gate Releases - Keno Releases) (from the 'USBR Daily' Sheet in the IGD Calculator)
(USGS IGD - USGS Keno) are close, but don't match exactly -> check why and determine what to use for calc
(see section on new Accretion data object below)

##-----------------------------
--Data in IGD Calculator--
-USBR Daily Sheet-
Keno Releases CFS is similar to, but not exactly matching USGS Klamath River nr Keno gage flow
Iron Gate Releases CFS is similar to, but not exactly matching USGS Klamath River blw IGD gage flow

##-----------------------------
New DMIs were created to import observed and forecast data. 
DMI control files, if required, are in the $KLAMATH_OPS/Control/ dir
DMI data files are in the $KLAMATH_OPS/Database/Ingest/ dir
Pisces database file is located in the $KLAMATH_OPS/Database/ dir
Pisces RW DMI executable from Jon Rocha is located in the $KLAMATH_OPS/ dir

Dates required to import data:
		Deterministic Forecast End Date - 5 days after Operations Start Date
		Accretions End Date - 7 days after Operations Start Date
	Created two global functions to retrieve these dates from Dashboard Controls scalar slots
	Created two scalar slots on the Dashboard Controls data object to hold these dates -> currently not set, will need to write Init rules to set based on script inputs
	Check that the date ajustments are correct (5 / 7 days)

New DMIs and associated data files created:

DMI										Type							Associated Files
Import Forecasts CNRFC Deterministic	Excel Database DMI				$KLAMATH_OPS/Database/Ingest/CNRFC_Import.xlsx			
Import Forecasts NRCS WSF				Excel Database DMI				$KLAMATH_OPS/Database/Ingest/NRCS_WSF_Import.xlsx
Import Observations KBAO				Excel Database DMI				$KLAMATH_OPS/Database/Ingest/KBAO_Observations_Import.xlsx
Import Observations PacifiCorp			Excel Database DMI				$KLAMATH_OPS/Database/Ingest/PacifiCorp_Observations_Import.xlsx
Import Observations Pisces				Control File Executable DMI		$KLAMATH_OPS/PiscesRiverWareDMI   (Pisces - RW DMI executable)
																		$KLAMATH_OPS/Database/klamath.pdb (Pisces database file)
																		$KLAMATH_OPS/Control/Pisces_Import_Control.txt  (RW DMI control file)       
Import Forecasts CNRFC HEFS				Excel Database DMI				$KLAMATH_OPS/Database/Ingest/CNRFC_Import.xlsx

##-----------------------------
Accretion data object created to hold accretions data
	May want to move these slots to objects, rename, etc, but it's a start
	Holds imported PacifiCorp Keno to IGD accretions
	Calculates Keno to IGD accretions from USGS gauge flows
		Done using a new initilization rule -> Calculate Accretions from Observations
		Currently using InpObs slots, need to change to actual Outflow slots once Init rules for combining Obs and Fcst data are written
		
##-----------------------------
Model Layout
Added:
	'LRSC Lost below Clear Lake Dam' Stream Gage
	'Clear to Malone Gains' Reach
	'MAL Malone Reservoir' Stream Gage
	'HRPO Lost River at Harpold Dam' Stream Gage
	'Anderson Rose Dam' Stream Gage
	'Tule Lake below ARD Returns' Reach
	'Miller Diversion Dam'
		Holds Langell Valley North Canal diversions -> maybe move to diversion object?
Updated link groups
	'Tule Lake below ARD Returns' <> 'Tule Lake' link added to Outflow group and doesn't seem right
		Couldn't figure out how to remove a link from a group
	
##-----------------------------
KBAO Hydromet
Currently not using:
		CHRO Cherry Creek near Fort Klamath -> flow available
		KELO Lost River at Keller Bridge -> stage record is available, all flow records are missing
		LRD LRDC at C-G Crossing -> flow available
		LRVO LRDC at Tingley Lane -> flow available

##-----------------------------
Run times - how are slots resized and set to the correct run times?
	With input data, to handle missing values, I want to pull in data from ~10 days prior to run start date
		The Pisces import defaults to the slots sizes existing in model -> need to make sure slot timesteps exist when data are pulled in
		
##-----------------------------
Initialization Rules
New rule written to calculate accretions -> see above
New rule written to set initial pool elevations
	-> should this also set known pool elevations? From Start Date to Day of Last Obs?
	-> no error checking right now -> see comment about slot sizes above
		check for missing values, and grab value from last 10 days if missing, or another approach that KBAO uses
		
		
##-----------------------------
Clear Max Release

OLD TABLE - MAY WANT TO PUT BACK IN
4515.6				0
4521				459.999999999294
4521.99999999999	480.000000001413
4525.99999999999	575
4529.99999999999	665.000000000706
4533.99999999999	730.000000001413
4536.4				780.000000001413
4537.99999999999	809.999999999294
4541.99999999999	869.999999998588
4545.99999999999	919.999999998588
4547.2				919.999999998588

Clear Releases, if the LRSC gauge is believed, can be higher than what is specified in this table
	on a test run, releases hit 1010.12 on Apr 7 2018, with a corresponding pool elevation of 4530.85
	Are there local inflows between the reservoir outlet and the gauge, are the gauge numbers suspect, or is the max release table incorrect?
	
##-----------------------------
Reservoir Inflow Method

Hydrologic Inflow Method -> set to 'Solve Hydrologic Inflow' for reservoirs where we know inflows/outflows and pool elevations
	-> how is this method different from the 'Hydrologic Inflow and Loss'?
	-> should we also account for evap losses somehow to close the water budget?
	When using this method, for forecasts, or projections of conditions when pool elevation is not known, both the Inflow and Hydrologic Inflow slots must be set
	