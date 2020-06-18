Understanding these key concepts in thermal ecology makes visualizing springtime insect development from the egg stage to adult emergence possible for any species with a quantified BDT and EADDC. The following visualization combines the simplicity of the degree_days function with current and historical weather data sourced from [NOAA's Global Historical Climatology Network](https://www.ncdc.noaa.gov/ghcnd-data-access) (R Package: [RNOAA](https://docs.ropensci.org/rnoaa/)) and [The Climatology Lab's gridMET dataset](http://www.climatologylab.org/gridmet.html) (R Package: [climateR](https://github.com/mikejohnson51/climateR)). The resulting functionality of the visualization is twofold: 

 - *Phenopause heatmap*- A colorized map displaying insect development across the United States for the selected species. The accumulated degree days to which the colors correspond are scaled based on each species' EADDC (displayed in the map legend).
	 - User controls:
		 - *Select a species*: Change the species visualized on the heatmap
		 - *Change heatmap date*: Change the heatmap viewing date (From first day of this year to two days ago)
		 - *Heatmap resolution*: Change the level of accuracy of the heatmap
			 - *Week* (default): Display's previously calculated heatmap for the week that contains the selected date.
			 - *Day of Week*: Can take several minutes to compute date specific cumulative degree days across the United States. **Use with caution!** 
 - *Observations* - An optional map layer of circle markers displaying where various insect species' were observed to determine thermal tolerances. When an observation is selected, a plot of accumulated degree days for the current date range will be displayed. 
	 - User controls:
		 - *Select species*: Select (or deselect) species' whose observations will be displayed
		 - *Date range*: The period of time to plot accumulated degree days for, starting from 0 at the start date. (Default: first day of this year to yesterday)

*Note concerning observation plots*: The simplifying assumption that offspring are immediatly produced upon the emergence of new adults was made, allowing for multiple generations per year. When accumulated degree days equals EADDC, one generation reaches adulthood and the accumulated degree days are reset to 0. This marks the beginning of a new generation. This is not the case for the phenopause heatmap, which displays phenology for one generation each year. 
