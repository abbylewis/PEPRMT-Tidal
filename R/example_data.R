#' Example data
#'
#' Data sourced from Oikawa et al."A new coupled biogeochemical modeling approach provides accurate predictions of methane and carbon dioxide fluxes across diverse tidal wetlands." 
#' Journal of Geophysical Research: Biogeosciences 129.10 (2024): e2023JG007943.
#' Eddy covariance data are from Ameriflux sites US-Srr, US-Edn, US-LA1, US-PLM, US-Stj
#' See Ameriflux website for site location and information: https://ameriflux.lbl.gov/sites/site-search/
#' All sites are North American tidal marshes
#'
#' @format ## `example_data`
#' A data frame with 7,240 rows and 60 columns:
#' \describe{
#'   \item{site}{Ameriflux site ID}
#'   \item{Year}{Year}
#'   \item{DOY_disc}{Discontinuous day of year (starts over at 1 at the beginning of each year)}
#'   \item{DOY}{Day of year (continuously increases over time)}
#'   \item{TA_C}{Air temperature (degrees C)}
#'   \item{WTD_cm}{Water table depth (cm)}
#'   \item{PAR_umol_m2_day}{Average Photosynthetically active radiation (umol m-2 d-1)}
#'   \item{LAI}{Leaf Area Index (not used in this dataset so defaulted to NaN)}
#'   \item{EVI}{daily Enhanced Vegetation Index from Landsat}
#'   \item{FPAR}{FPAR flag for GPP module. Set to 0 if using EVI. Set to 1 if using LAI}
#'   \item{LUE}{Light Use Efficiency. Computed by taking the average of daily measured GPP divided by daily average PAR for each site}
#'   \item{Wetland_age_years}{Age of wetland in years}
#'   \item{Salinity_daily_ave_ppt}{Daily average salinity (ppt)}
#'   \item{NO3_mg_L}{Daily average NO3 (mg/L)}
#'   \item{SOM_MEM_gC_m3}{Available soil organic matter (gC m-3) in the top 1 m of soil predicted by the Cohort Marsh Equilibrium (CMEM) model}
#'   \item{CO2_gC_m2_day}{Measured Net Ecosystem Exchange (NEE) of CO2 (gC-CO2 m-2 day-1)}
#'   \item{GPP_gC_m2_day}{Gross Primary Productivity (GPP) partitioned from NEE (gC-CO2 m-2 day-1)}
#'   \item{Reco_gC_m2_day}{Ecosystem Respiration (Reco) partitioned from NEE (gC-CO2 m-2 day-1)}
#'   \item{CH4_gC_m2_day}{Measured Net Ecosystem Exchange of CH4 (gC-CH4 m-2 day-1)}
#'   ...
#' }
#' @source Oikawa et al. 2024
"example_data"
