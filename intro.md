### Visualizing the Insect Developmental Progress 

While many insects' life cycles vary greatly, a wholistic interpretation of this life cycle allows for intriguing developmental comparisons to be made between insect species. This visualization will interpret the insect developmental process, specifically **spring phenology**— the time from an insect's egg stage to its emergence as an adult, which encompasses numerous, varying intermediate stages. 

**Growing degree days (GDD)** are one metric ecologists can use to understand spring phenology in insects. In other words, insects can be thought of as reaching adulthood after they have accumulated a sufficient amount of thermal energy for their emergence to occur. At first glance, this method may seem overly general, as it is well understood that thermal tolerances vary greatly between species. However, incorporating [previously collected data](https://github.com/lbuckley/ICBseasonality/tree/master/CodeForICBPaper) on individual species' developmental temperature thresholds allows for the calculation of GDDs for a target species. The [TrenchR](https://github.com/trenchproject/TrenchR) package's [degree day function](https://github.com/trenchproject/TrenchR/blob/master/man/degree_days.Rd) combines species-specific baseline developmental temperatures with daily temperature minima (**T_min**) and maxima (**T_max**) to calculate the GDDs accumulated in a 24 hour period:

    TrenchR::degree_days(T_min, T_max, LDT = NA, UDT = NA, method = "single.sine")

- **T<sub>0</sub>** (&deg;C) - The *baseline developmental threshold* is the minimum temperature required for development to occur, above which an
   organism is likely to progress forward in development. 
- **G** (&deg;C) - The *upper developmental threshold* is the upper temperature above which an organism will not be able to develop. In
   practice, this value is likely greater than any temperature the
   species will encounter during spring development in the United
   States. Therefore, in this visualization the simplifying assumption
   that organisms will not actually encounter this temperature was made.
- **method** - The approximation technique used to calculate temperature throughout day. The *single.sine* method assumes the temperature curve is normally distributed around the maximum temperature. 

The threshold amount of degree days that must be accumulated for an insect egg to reach adulthood is defined as the **egg to adult degree days Celsius (EADDC)**.  




<!--stackedit_data:
eyJoaXN0b3J5IjpbLTEwMzU2OTIxNzQsOTU3NzQ4MTI1LC0xMj
gwOTg2MTI3LDE2OTQ3MTY2MTAsNTQ1MDI5MzgyLDEzNDQ0MjU1
NzcsMTcxODEzODM4MCwtODQxMTQzNjkwLDExMTQzMzc0MTQsMj
I1MTYyMzQwLDUxMDQ2ODMzMiwzOTQyNTc0NTldfQ==
-->