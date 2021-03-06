Development rates can be reasonably approximated by how many heat units an insect accumulates. The rate of development each day is determined by the number of degrees by which temperatures exceed T<sub>0</sub>, which is referred to as **degree days**. The insect reaches adulthood once it acquires a sufficient number of degree day heat units, which is referred to as the **growing degree days required** (G, °C).

Developmental traits (T<sub>0</sub> and G) have been estimated for many insect populations by raising them in laboratories at a range of temperatures. Here we use a compilation of development traits for 1493 observations of 682 species to predict phenological timing. We provide two ways of exploring insect phenology:

 - *Phenophase heatmap*- A colorized map displaying insect development across the United States for the selected species. The accumulated degree days to which the colors correspond are scaled based on each species' G (displayed in the map legend).
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


*Note concerning observation plots:* The simplifying assumption that offspring are immediately produced upon the emergence of new adults was made, allowing for multiple generations per year. When accumulated degree days equals G, one generation reaches adulthood and the accumulated degree days are reset to 0. This marks the beginning of a new generation. A custom generational lag can be specified below the plot. This is not the case for the phenophase heatmap, which displays phenology for one generation each year.

*Note concerning pieris rapae heatmap:* The [USA National Phenology Network](https://www.usanpn.org/usa-national-phenology-network) Nature's Notebook Program features [observational data](https://www.usanpn.org/data/observational) of many plant and insect populations' pheology. The NPN database, features  *P. rapae* as an insect species with many historical phenophase obervations around the USA. Observations within 30 days of the current layer date are presented for *P. rapae* as an additional (optional) layer of colored, clickable markers, and displayed when *P. rapae* is selected for visualization on the heatmap.
 
### Data sources and methods:
**Insect Data:** Sourced from [Jarosík et al. 2011](https://pubmed.ncbi.nlm.nih.gov/22299347/),  available on [GitHub](https://github.com/lbuckley/ICBseasonality/tree/master/CodeForICBPaper)

**Weather data:** We use daily weather data sourced from the two weather stations nearest to an observaton within [NOAA's Global Historical Climatology Network](https://www.ncdc.noaa.gov/ghcnd-data-access) (R Package: [RNOAA](https://docs.ropensci.org/rnoaa/)) to create phenology plots and [The Climatology Lab's gridMET dataset](http://www.climatologylab.org/gridmet.html) (R Package: [climateR](https://github.com/mikejohnson51/climateR)), which features daily, gridded weather data at 4km spatial resolution to produce phenology heatmaps.

**Degree day calculation:** We use a function for calculating degree days available from the TrenchR package.  The [TrenchR](https://github.com/trenchproject/TrenchR) package's [degree day function](https://github.com/trenchproject/TrenchR/blob/master/man/degree_days.Rd) combines species-specific baseline developmental temperatures with daily temperature minima (T_min) and maxima (T_max) to calculate the GDDs accumulated in a 24 hour period:

    TrenchR::degree_days(T_min, T_max, T_0 = NA, T_upper = NA, method = "single.sine")

-   **T<sub>0**</sub> (°C) - The lower developmental threshold is the minimum temperature required for development to occur, above which an organism is likely to progress forward in development.
    
-   **T<sub>upper**</sub> (°C) - The upper developmental threshold is the upper temperature above which an organism will not be able to develop. In practice, this value is likely greater than any temperature the species will encounter during spring development in the United States. Therefore, in this visualization the simplifying assumption that organisms will not actually encounter this temperature was made.
    
-   **method** - The approximation technique used to calculate temperature throughout the day. The single.sine method assumes the temperature curve is normally distributed around the maximum temperature.
    
The threshold amount of degree days that must be accumulated for an insect egg to reach adulthood is defined as the **growing degree days required** (G).

<!--stackedit_data:
eyJoaXN0b3J5IjpbLTIwNDkzNTQ5ODhdfQ==
-->