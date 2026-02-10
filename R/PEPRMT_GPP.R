#' Gross Primary Productivity (GPP)
#'
#' Runs the PEPRMT gross primary productivity module for freshwater peatlands or
#' tidal wetlands at a daily time step.
#'
#' @param theta Numeric vector of length 4 containing calibrated parameter
#'   values. Default values were determined via MCMC Bayesian fitting
#'   (Oikawa et al. 2023).
#' @param data Data frame containing 15 required columns used as model inputs.
#'   See **Details** for expected column structure.
#'
#' @description
#' Gross Primary Productivity (GPP) module of the PEPRMT model (v1.0).
#'
#' @details
#' The PEPRMT model was originally parameterized for restored freshwater
#' wetlands in the Sacramento–San Joaquin River Delta, California, USA
#' (Oikawa et al. 2017) and later updated for tidal wetlands (Oikawa et al. 2023).
#'
#' Modules are intended to be run sequentially:
#' PEPRMT_GPP, then PEPRMT_Reco, then PEPRMT_CH4.
#'
#' All variables are expected at a daily time step.
#' 
#' This model predicts GPP using a light use efficiency equation GPP can be 
#' predicted using leaf area index (LAI) or a greenness index from Phenocam data 
#' or remote sensing such as EVI or NDVI PEPRMT-Tidal applied in Oikawa et al. 
#' 2023 uses EVI from Landsat
#'
#' **Expected data column order:**
#' 1. Continuous day of year
#' 2. Discontinuous day of year
#' 3. Year
#' 4. Air temperature (°C)
#' 5. Water table depth (cm)
#' 6. PAR (µmol m^-2 d^-1)
#' 7. Leaf Area Index
#' 8. Greenness Index
#' 9. FPAR flag
#' 10. Light Use Efficiency
#' 11. Wetland age (years)
#' 12. Salinity (ppt)
#' 13. NO3 (mg L^-1)
#' 14. Soil organic matter (g C m^-3)
#' 15. Site identifier
#'
#' @returns Updated dataframe containing:
#' \describe{
#'   \item{GPP}{gross primary productivity
#'     (g C CO2 m^-2 day^-1)}
#'   \item{APAR}{absorbed photosynthetically active radiation
#'     (umol m-2 d-1)}
#' }
#'
#' @references
#' Oikawa, P. Y., Jenerette, G. D., Knox, S. H., Sturtevant, C., Verfaillie, 
#' J., Dronova, I., Poindexter, C. M., Eichelmann, E., & Baldocchi, D. D. 
#' (2017). Evaluation of a hierarchy of models reveals importance of substrate 
#' limitation for predicting carbon dioxide and methane exchange in restored 
#' wetlands. Journal of Geophysical Research: Biogeosciences, 122(1), 145–167. 
#' https://doi.org/10.1002/2016JG003438
#'
#' Oikawa, P. Y., Sihi, D., Forbrich, I., Fluet-Chouinard, E., Najarro, M., 
#' Thomas, O., Shahan, J., Arias-Ortiz, A., Russell, S., Knox, S. H., McNicol, 
#' G., Wolfe, J., Windham-Myers, L., Stuart-Haentjens, E., Bridgham, S. D., 
#' Needelman, B., Vargas, R., Schäfer, K., Ward, E. J., Megonigal, P., & 
#' Holmquist, J. (2024). A New Coupled Biogeochemical Modeling Approach Provides 
#' Accurate Predictions of Methane and Carbon Dioxide Fluxes Across Diverse 
#' Tidal Wetlands. Journal of Geophysical Research: Biogeosciences, 129(10), 
#' e2023JG007943. https://doi.org/10.1029/2023JG007943
#'
#' @export
#'
#' @examples
#' # Example
#' # data(example_dataset)
#' # theta <- c(0.7479271, 1.0497113, 149.4681710, 94.4532674)
#' # out <- PEPRMT_GPP(theta, example_dataset, wetland_type = 2)
PEPRMT_GPP <- function(theta, 
                       data) {
  #---CREATE A SPACE TO COLLECT ITERATIVE RESULTS---#
  q <- unique(as.integer(data$site))
  outcome_lst <- vector("list", length(q))

  #---ITERATIVE LOOP TO RUN THE MODEL ACROSS DIFFERENT SITES---#
  for (i in 1:length(q)) {
    #  subset your data here, then create the exogenous variables here
    d <- subset(data, site == i)

    # Exogenous Variables
    Time_2 <- d[, 1] # day of year (1-infinite # of days)
    DOY_disc_2 <- d[, 2] # discontinuous day of year that starts over every year (1-365 or 366)
    Year_2 <- d[, 3] # year
    TA_2 <- d[, 4] # Air temperature (C)
    WT_2 <- d[, 5] # water table depth (cm) equals 0 when water table at soil surface
    PAR_2 <- d[, 6] # photosynthetically active radiation (umol m-2 d-1)
    LAI_2 <- d[, 7] # Leaf area index (if not using can be 0s or NaN)
    GI_2 <- d[, 8] # greeness index from Phenocam (GCC) or Landsat EVI etc (unitless)
    FPAR <- d[, 9] # If using LAI data, set FPAR variable to 1's, if using a greenness index set FPAR to 0's
    LUE <- d[, 10] # growing season LUE computed for each site using measured GPP in gC per umol
    wetland_age_2 <- d[, 11] # Age of wetland in years
    Sal <- d[, 12] # Salinity (ppt)
    NO3 <- d[, 13] # Dissolved NO3 (mg/L)
    # Season_drop_2 <- d[,13] #not used in PEPRMT-Tidal (was used in original PEPRMT model in peatlands)
    # Season variable that is set to 1 in winter (DOY 1-88, 336-365), 2 pre-spring (DOY 89-175), 3 spring (DOY 176-205), 4 summer (DOY 206-265), 5 fall (DOY 266-335)
    SOM_2 <- d[, 14] # Decomposed Organic matter : all the decomposed soil organic matter in top meter of soil informed buy MEM inclusive of current year
    site_2 <- d[, 15] # Site: if running more than 1 site, have 1s in this column for first site, 2s for 2nd site and so on


    ########## COMPUTE GPP################################
    # PARAMETERS
    Ha <- theta[3] + 30 # default=30;#activation energy for general crop plant (KJ mol-1)
    Hd <- theta[4] + 100 # default=100;(KJ mol-1)
    # CONSTANTS
    vcopt <- 1.0
    R_t <- 0.00831 # KJ mol-1 K-1
    T_opt <- 25 + 274.15 # (K); our Temp opt for Ps is 25C

    # EQUATIONS
    # Decide how to compute fPAR
    # If Fpar = 1 then calculate FPAR using LAI if FPAR =0 then calculate using GI
    if (sum(FPAR) > 0) {
      k <- 0.8 # ranges between 0-1
      fPAR_2 <- 0.95 * (1 - exp(-k * LAI_2)) # for an LAI=4.9, fpar=0.87--Yuan 2007
      fPAR_2 <- (fPAR_2 / 2) * 10^4
    } else {
      fPAR_2 <- theta[1] + theta[2] * GI_2
    }


    APAR_2 <- fPAR_2 * PAR_2 # umol m-2 daily average

    AirT_K <- TA_2 + 274.15 # C to Kelvin


    vct <- vector("numeric", length(Time_2))
    NPP_FPAR_T <- vector("numeric", length(Time_2))

    for (t in 1:length(Time_2)) {
      exponent1 <- (Ha * (AirT_K[t] - T_opt)) / (AirT_K[t] * R_t * T_opt)
      exponent2 <- (Hd * (AirT_K[t] - T_opt)) / (AirT_K[t] * R_t * T_opt)
      top <- Hd * exp(exponent1)
      bottom <- Hd - (Ha * (1 - exp(exponent2)))
      vct[t] <- vcopt * (top / bottom)

      NPP_FPAR_T[t] <- ((vct[t] * (APAR_2[t] * LUE[t]))) # umol m-2 d-1* gC/umol == g C m-2 d-1
    }

    GPP <- (NPP_FPAR_T) * -1 # stay as g C m-2 d-1 where negative values= uptake

    w <- cbind(GPP, APAR_2, Time_2) |>
      as.data.frame()
    # store d in a vector
    outcome_lst[[i]] <- (w)
  }

  # combine iterations of loop and return all results
  GPP_output <- do.call("rbind", outcome_lst) |>
    as.data.frame()

  return(GPP_output)
}

