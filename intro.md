# Insect Spring Phenology

Insect life cycles provide the basis for our understanding of the developmental process of countless species. Here is a typical insect life cycle:   
![Insect life cycle](http://www.co.stevens.wa.us/weedboard/htm_bio/lifecycle%20diagram.jpg)

While many insects' life cycles vary greatly from one another, a wholistic interpretation of this life cycle allows for intriguing developmental comparisons to be made between insect species. This visualization will interpret the insect developmental process, specifically **spring phenology**, as a period of time from the insect's egg stage to its emergence as an adult, encompassing other, distinct intermediate stages. 

**Growing degree days (GDD)** are one metric ecologists can use to understand spring phenology in insects. In other words, insects can be thought of as reaching adulthood after they have accumulated a sufficient amount of thermal energy for their emergence to occur. At first glance, this method may seem overly general, as it is well understood that thermal tolerances vary greatly between species. However, incorporating [previously collected data](https://github.com/lbuckley/ICBseasonality/tree/master/CodeForICBPaper) on individual species' developmental temperature thresholds allows for the calculation of GDDs for a target species. The [TrenchR](https://github.com/trenchproject/TrenchR) package's degree day function combines species-specific baseline developmental temperatures with daily temperature minima (**T_min**) and maxima (**T_max**) to calculate the GDDs accumulated in a 24 hour period:

    TrenchR::degree_days(T_min, T_max, LDT = NA, UDT = NA, method = "single.sine")

 - **BDT** (&deg;C) - The *baseline developmental threshold* is the minimum temperature required for development to occur, above which an
   organism is likely to progress forward in development. 
  - **UDT** (&deg;C) - The *upper developmental threshold* is the upper temperature above which an organism will not be able to develop. In
   practice, this value is likely greater than any temperature the
   species will encounter during spring development in the United
   States. Therefore, in this visualization the simplifying assumption
   that organisms will not actually encounter this temperature was made.
   - **method** - The approximation technique used to calculate temperature throughout day. The *single.sine* method assumes the temperature curve is normally distributed around the maximum temperature. 

The threshold amount of degree days that must be accumulated for an insect egg to reach adulthood is defined as the **egg to adult degree days Celsius (EADDC)**.  

Understanding these key concepts in thermal ecology makes visualizing springtime insect development from the egg stage to adult emergence possible for any species with a quantified BDT and UDT. The following visualization combines the simplicity of the degree_days function with current and historical weather data sourced from [NOAA's Global Historical Climatology Network](https://www.ncdc.noaa.gov/ghcnd-data-access) (R Package: [RNOAA](https://docs.ropensci.org/rnoaa/)) and [The Climatology Lab's gridMET dataset](http://www.climatologylab.org/gridmet.html) (R Package: [climateR](https://github.com/mikejohnson51/climateR)). The resulting functionality of the visualization is twofold: 

 - *Phenopause heatmap*- A colorized map displaying insect development across the United States for the selected species
	 - User controls:
		 - *Select a species*: Change the species visualized on the heatmap
		 - *Change layer date*: Change the heatmap viewing date (From first day of this year to two days ago)
		 - *Heatmap resolution*: Change the level of accuracy of the heatmap
			 - *Week* (default): Display's previously calculated heatmap for the week that contains the selected date.
			 - *Day of Week*: Can take several minutes to compute date specific cumulative degree days across the United States. **Use with caution!** 
 - *Observations* - An optional map layer of circle markers displaying where various insect species' were observed to determine thermal tolerances. When an observation is selected, a plot of accumulated degree days for the current date range will be displayed. 
	 - User controls:
		 - *Select species*: Select (or deselect) species' whose observations will be displayed
		 - *Date range*: The period of time to plot accumulated degree days for, starting from 0 at the start date. (Default: first day of this year to two days ago)


<!--stackedit_data:
eyJoaXN0b3J5IjpbLTE5MDY4MDQxODEsMTExNDMzNzQxNCwyMj
UxNjIzNDAsNTEwNDY4MzMyLDM5NDI1NzQ1OV19
-->