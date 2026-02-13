#' Ecosystem Respiration (Reco)
#'
#' Runs the PEPRMT ecosystem respiration module for freshwater peatlands or
#' tidal wetlands at a daily time step.
#'
#' @param theta Numeric vector of length 4 containing calibrated parameter
#'   values. Default values were determined via MCMC Bayesian fitting
#'   (Oikawa et al. 2023).
#' @param data Data frame containing 16 required columns used as model inputs.
#'   See **Details** for expected column structure.
#' @param wetland_type Integer indicating wetland class: 1 = Freshwater
#' peatland, 2 = Tidal wetland.
#'
#' @description
#' Ecosystem respiration (Reco) module of the PEPRMT model (v1.0).
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
#' 16. Modeled GPP (g C m^-2 day^-1)
#'
#' @returns Updated dataframe containing:
#' \describe{
#'   \item{Reco_mod}{Total ecosystem respiration
#'     (g C CO2 m^-2 day^-1)}
#'   \item{NEE_mod}{Net ecosystem exchange of CO2
#'     (g C CO2 m^-2 day^-1)}
#'   \item{S1}{Labile soil carbon pool
#'     (g C m^-3, top meter of soil)}
#'   \item{S2}{Soil organic carbon pool
#'     (g C m^-3, top meter of soil)}
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
#' # theta <- c(18.4, 1487.6, 11.6, 61.3)
#' # out <- PEPRMT_Reco(theta, example_dataset, wetland_type = 2)
PEPRMT_Reco <- function(theta = c(18.41329, 1487.65701, 11.65972, 61.29611),
                        data,
                        wetland_type) {
  # -------------------------
  # Check data structure
  # -------------------------

  data <- data.frame(data)
  expected_colnames <- c(
    "DOY", "DOY_disc", "Year", "TA_C", "WTD_cm",
    "PAR_umol_m2_day", "LAI", "EVI", "FPAR", "LUE", "Wetland_age_years",
    "Salinity_daily_ave_ppt", "NO3_mg_L",
    "SOM_MEM_gC_m3", "site", "GPP_mod"
  )

  if (!all(expected_colnames %in% colnames(data))) {
    stop(paste0(
      "Missing required inputs.\nThe following columns were not found in data:\n",
      paste(expected_colnames[!expected_colnames %in% colnames(data)],
        collapse = ", "
      )
    ))
  }

  # -------------------------
  # Check parameters
  # -------------------------

  if (!is.numeric(theta) || length(theta) != 4) {
    stop("Reco_theta must be a numeric vector of length 4.", call. = FALSE)
  }

  # -------------------------
  # Check wetland_type
  # -------------------------

  if (!is.numeric(wetland_type) || length(wetland_type) != 1 ||
    !wetland_type %in% c(1, 2)) {
    stop("wetland_type must be a single numeric value: 1 (Freshwater peatland) or 2 (Tidal wetland).",
      call. = FALSE
    )
  }

  # SET UP Reco
  # SOM
  alpha1 <- 3e3 # g C m-2 d-1;--SET AS CONSTANT;
  ea1 <- theta[1] * 1000 # J mol-1
  km1 <- theta[2] # g C m-3

  # LABILE
  alpha2 <- 3e3 # g C m-2 d-1 --SET AS CONSTANT
  ea2 <- theta[3] * 1000 # J mol-1
  km2 <- theta[4] # g C m-3

  # initialize labile C pool-set to 0 initially
  C2_init <- 0 # in g C m-3

  # Empirical parameters used to comput inhibition of Reco when WTD is high
  a1 <- 0.00033
  a2 <- 0.0014
  a3 <- 0.75


  #---CREATE A SPACE TO COLLECT ITERATIVE RESULTS---#
  q <- unique(data$site)
  outcome_lst <- vector("list", length(q))

  #---ITERATIVE LOOP TO RUN THE MODEL ACROSS DIFFERENT SITES---#
  for (i in 1:length(q)) {
    #  subset your data here, then create the exogenous variables here
    d <- subset(data, site == q[[i]])

    # Exogenous Variables
    DOY <- d$DOY # day of year (1-infinite # of days)
    DOY_disc_2 <- d$DOY_disc # discontinuous day of year that starts over every year (1-365 or 366)
    Year_2 <- d$Year # year
    TA_2 <- d$TA_C # Air temperature (C)
    WT_2 <- d$WTD_cm # water table depth (cm) equals 0 when water table at soil surface
    PAR_2 <- d$PAR_umol_m2_day # photosynthetically active radiation (umol m-2 d-1)
    LAI_2 <- d$LAI # Leaf area index (if not using can be 0s or NaN)
    GI_2 <- d$EVI # greeness index from Phenocam (GCC) or Landsat EVI etc (unitless)
    FPAR <- d$FPAR # If using LAI data, set FPAR variable to 1's, if using a greenness index set FPAR to 0's
    LUE <- d$LUE # growing season LUE computed for each site using measured GPP in gC per umol
    wetland_age_2 <- d$Wetland_age_years # Age of wetland in years
    Sal <- d$Salinity_daily_ave_ppt # Salinity (ppt)
    NO3 <- d$NO3_mg_L # Dissolved NO3 (mg/L)
    # Season_drop_2 <- d[,13] #not used in PEPRMT-Tidal (was used in original PEPRMT model in peatlands)
    # Season variable that is set to 1 in winter (DOY 1-88, 336-365), 2 pre-spring (DOY 89-175), 3 spring (DOY 176-205), 4 summer (DOY 206-265), 5 fall (DOY 266-335)
    SOM_2 <- d$SOM_MEM_gC_m3 # Decomposed Organic matter : all the decomposed soil organic matter in top meter of soil informed buy MEM inclusive of current year
    site <- d$site # Site: if running more than 1 site, have 1s in this column for first site, 2s for 2nd site and so on
    GPP_2 <- d$GPP_mod # Modeled GPP - use output from PEPRMT-GPP (gC m-2 day-1) where negative fluxes = uptake


    # #Static C allocation theme
    NPPsum_avail_2 <- (c(GPP_2) * -1) # g C m-2 day-1 change to + numbers & give Reco access to GPP

    # Time Invariant
    R <- 8.314 # J K-1 mol-1
    RT <- R * (TA_2 + 274.15) # T in Kelvin-all units cancel out
    Vmax1 <- alpha1 * exp(-ea1 / RT) # g C m-2 d-1 SOM
    Vmax2 <- alpha2 * exp(-ea2 / RT) # g C m-2 d-1 labile


    # preallocating space
    S1sol <- vector("numeric", length(DOY))
    S2sol <- vector("numeric", length(DOY))
    R1 <- vector("numeric", length(DOY))
    R2 <- vector("numeric", length(DOY))
    S1 <- vector("numeric", length(DOY))
    S2 <- vector("numeric", length(DOY))
    percent_reduction <- vector("numeric", length(DOY))
    percent_enhancement <- vector("numeric", length(DOY))
    Reco_1 <- vector("numeric", length(DOY))
    Reco_mod <- vector("numeric", length(DOY))
    Ps <- vector("numeric", length(DOY))
    C2in <- vector("numeric", length(DOY))
    C1_init <- vector("numeric", length(DOY))
    percent_available <- vector("numeric", length(DOY))

    for (t in 1:length(DOY)) {
      # C allocation
      # only 50% of GPP is available
      C2in[t] <- NPPsum_avail_2[t] * 0.5 # gC m-2 d-1

      C1_init[t] <- SOM_2[t] # "decomposed" Organic matter all the soil organic matter in top meter from MEM inclusive of current year

      # if (t == 1 | site_change[t]>0 #if beginning of model or switch sites, start C1 pool over
      if (t == 1) {
        S1[t] <- C1_init[t] # substrate avail NOT affected by water avail-- SOM pool
        S2[t] <- C2_init + C2in[t] # Ps C pool-- some initial Ps C lingering in soil + day 1 GPPavail
      } else {
        S1[t] <- S1sol[t - 1] + C1_init[t]
        S2[t] <- S2sol[t - 1] + C2in[t] # substrate availability based on Ps on time step previous
      }

      # Empirical factor for increased availability of SOC during the first 3 yrs following restoration
      if (wetland_age_2[t] < 1) {
        percent_available[t] <- 0.6
      } else {
        percent_available[t] <- 1 # for peatlands was 20% now 100% is available
      }

      S1[t] <- S1[t] * percent_available[t] # SOM pool


      # following Davidson and using multiple eq for diff substrate pools
      R1[t] <- Vmax1[t] * S1[t] / (km1 + S1[t]) # g C m2 d-1 Reaction velocity
      R2[t] <- Vmax2[t] * S2[t] / (km2 + S2[t]) # g C m2 d-1
      if (R1[t] < 0) {
        R1[t] <- 0
      }

      if (R2[t] < 0) {
        R2[t] <- 0
      }


      # Reco is reduced by 25% when WT is at or above soil surface
      #--McNicol Silver 2015
      #   a1 <- 0.00033
      #   a2 <- 0.0014
      #   a3 <- 0.75
      #   WT_ex <- c(-30, -20, -10, 0)
      #   percent_red_ex <- c(1, 0.85, 0.77, 0.75)

      #   plot(percent_red_ex ~ WT_ex)

      # insert logic tree here for freshwater peatland or tidal wetland switch
      # Reduce Reco when WTD is high--not applied to Tidal sites
      if (wetland_type == "1") {
        percent_reduction[t] <- (a1 * WT_2[t]^2) - (a2 * WT_2[t]) + a3
        if (WT_2[t] > 5) {
          percent_reduction[t] <- 0.75
        }
        if (percent_reduction[t] > 1.25) {
          percent_reduction[t] <- 1.25
        }
        if (percent_reduction[t] < 0.75) {
          percent_reduction[t] <- 0.75
        }

        R1[t] <- R1[t] * percent_reduction[t] # g C m2 d-1  Reaction velocity
        R2[t] <- R2[t] * percent_reduction[t] # g C m2 d-1
      } else {}

      # Empirical factor for elevated Reco during the first 3 yrs following restoration
      if (wetland_age_2[t] < 4) {
        percent_enhancement[t] <- 1.2
      } else {
        percent_enhancement[t] <- 1
      }

      R1[t] <- R1[t] * percent_enhancement[t] # umol m2 sec Reaction velocity
      R2[t] <- R2[t] * percent_enhancement[t] # umol m2 sec

      if (t == 1) {
        S1sol[t] <- C1_init[t] - (R1[t]) # accounts for depletion of C sources in soil due to Reco and methane production
        S2sol[t] <- (C2_init + C2in[t]) - (R2[t])
      } else {
        S1sol[t] <- S1[t] - R1[t]
        S2sol[t] <- S2[t] - R2[t]
      }

      if (S1sol[t] < 0) {
        S1sol[t] <- 0
      }
      if (S2sol[t] < 0) {
        S2sol[t] <- 0
      }

      ########### EDITED OUT IN CURRENT VERSION############
      # in autumn time or season 6, labile PS C pool empties into SOM
      # if (Season_drop_2[t]>5) {
      #   S1sol[t] = S1sol[t]+(0.2*S2sol[t]) #move part of labile C into SOM pool--mimicing plant matter dying
      #   S2sol[t] = S2sol[t]-(0.2*S2sol[t])
      # }
      #
      # #in winter time or season 1, labile PS C pool empties into SOM
      # if (Season_drop_2[t]<2) {
      #   S1sol[t] = S1sol[t]+(S2sol[t]) #move entire labile C into SOM pool--mimicing plant matter dying
      #   S2sol[t] = 0
      # }
      ########################################

      Reco_1[t] <- R1[t] + R2[t]
      Reco_mod[t] <- (R1[t]) + (R2[t]) # umol m2 d-1
    }

    NEE_mod <- GPP_2 + Reco_1 # umol m-2 d-1

    w <- data.frame(Reco_mod, NEE_mod, S1, S2, DOY, site)
    # store d in a vector
    outcome_lst[[i]] <- (w)
  }

  # combine iterations of loop and return all results
  Reco_output <- do.call("rbind", outcome_lst) |>
    as.data.frame()

  return(Reco_output)
}
