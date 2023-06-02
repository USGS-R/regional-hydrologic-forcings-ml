Active development moved to https://code.usgs.gov/wma/wp/regional-hydrologic-forcings-ml

# regional-hydrologic-forcings-ml
The repository contains the code pipeline used to develop machine learning models for regional prediction of hydrologic forcing functions, specifically for flood flow metrics. The FY22 focus is a rainfall-dominated flood region within and surrounding the Delaware River Basin (DRB), and a snowmelt-dominated region within and surrounding the Upper Colorado River Basin (UCOL). The steps of the pipeline are as follows: 

1. Fetch gage data and site characteristics for minimally altered catchments in the Conterminous United States (CONUS) from the National Water Information System (NWIS), and download watershed attributes for those sites from ScienceBase. Process the gage data and site characteristics to prepare them for use in modeling.
2. Compute daily flow quantile-based flood flow metrics: magnitude, duration, frequency, volume, and maximum daily flow. Computed for period of record and seasonally.
3. Use hierarchical agglomerative cluster analysis of seasonal flood metrics to estimate regions with similar flood response characteristics across CONUS.
4. Predict flood flow metrics using machine learning models.

## Dependencies not on CRAN
The EflowStats package can be installed from github using R:
remotes::install_github("USGS-R/EflowStats")

## Running in Parallel
This pipeline is built to run in parallel. Use the `tar_make_clustermq` function with your desired number of workers (cores) to run in parallel. Please check that the available RAM on your computing resources is at least 3 GB / worker.

## Notes 
- The GAGES2.1 database is currently (as of 2-28-22) being stored in a private repo until it is released to the public.
