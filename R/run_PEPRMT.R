#' run_PEPRMT
#' 
#' Runs all PEPRMT functions and returns an output dataframe with modeled GPP, Reco, and CH4
#'
#' @param data Data frame containing 15 required columns used as model inputs.
#'   See **Details** for expected column structure.
#'   
#' @description
#' Wrapper function to run all steps of the PEPRMT model (v1.0).
#' 
#' @details
#' The PEPRMT model was originally parameterized for restored freshwater
#' wetlands in the Sacramento–San Joaquin River Delta, California, USA
#' (Oikawa et al. 2017) and later updated for tidal wetlands with inhibition
#' of methane production in response to salinity and nitrate
#' (Oikawa et al. 2024).
#'
#' Modules are run sequentially:
#' PEPRMT_GPP, then PEPRMT_Reco, then PEPRMT_CH4.
#'
#' All variables are expected at a daily time step.
#'
#' All PEPRMT modules use the same input structure, although not all variables
#' are used in every module.
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
#'   \item{Reco_full}{Total ecosystem respiration
#'     (g C CO2 m^-2 day^-1)}
#'   \item{NEE_mod}{Net ecosystem exchange of CO2
#'     (g C CO2 m^-2 day^-1)}
#'   \item{S1}{Labile soil carbon pool
#'     (g C m^-3, top meter of soil)}
#'   \item{S2}{Soil organic carbon pool
#'     (g C m^-3, top meter of soil)}
#'   \item{pulse_emission_total}{total methane emitted
#'     (g C CH4 m^-2 day^-1)}
#'   \item{Plant_flux_net}{net methane flux via plant-mediated transport
#'     (g C CH4 m^-2 day^-1)}
#'   \item{Hydro_flux}{net diffusive methane flux from water to atmosphere
#'     (g C CH4 m^-2 day^-1)}
#'   \item{M1}{methane pool produced from labile soil carbon
#'     (g C CH4 m^-3, top meter of soil and water)}
#'   \item{M2}{methane pool produced from soil organic carbon
#'     (g C CH4 m^-3, top meter of soil and water)}
#'   \item{trans2}{fraction of methane released via plant-mediated transport
#'     (unitless)}
#' }
#' @export
#'
run_PEPRMT <- function(data) {
  data <- data.frame(data)
  #First run GPP Module
  GPP_theta <- c(0.7479271, 1.0497113, 149.4681710, 94.4532674)
  GPP_mod_data <- PEPRMT_GPP(theta = GPP_theta, data = data)
  
  #Create a new dataset that included model results
  data_results <- data |>
    dplyr::left_join(GPP_mod_data |>
                dplyr::rename(GPP_mod = GPP, 
                              DOY = Time_2,
                              site = site_2),
              by = c("DOY", "site"))
  
  #Second run Reco Module
  #Add modeled GPP into data before running Reco module (16th column)
  data$GPP_mod <- data_results$GPP_mod
  
  Reco_theta <- c(18.41329, 1487.65701, 11.65972, 61.29611)
  Reco_mod_data <- PEPRMT_Reco(Reco_theta, 
                                 data = data, 
                                 wetland_type = 2)
  
  #Create a new dataset that included model results
  data_results <- data_results |>
    dplyr::left_join(Reco_mod_data |>
                dplyr::rename(DOY = Time_2, 
                              Reco_mod = Reco_full,
                              site = site_2),
              by = c("DOY", "site"))
  
  #Last, run CH4 module
  #Add modeled S1, S2 into data before running CH4 module (17th & 18th columns)
  data$SOM_total <- data_results$S1
  data$SOM_labile <- data_results$S2
  
  CH4_theta <- c(
    #Ea_CH4_SOC kJ mol-1
    14.9025078 + 67.1,
    #kM_CH4_SOC
    0.4644174 + 17,
    #Ea_CH4_labile kJ mol-1- used to be 16.7845002 + 71.1
    86.7,
    #kM_Ch4labile
    0.4359649 + 23,
    #Ea_CH4_oxi
    15.8857612 + 75.4,
    #kM_CH4oxi
    0.5120464 + 23,
    #486.4106939, #kI_SO4
    100,
    #0.1020278) #kI_NO3
    0.2
  )
  
  CH4_mod_data <- PEPRMT_CH4(theta = CH4_theta,
                                     data = data,
                                     wetland_type = 2)
  
  #Create a new dataset that includes model results
  data_results <- data_results |>
    dplyr::left_join(
      CH4_mod_data |>
        dplyr::rename(DOY = Time_2, 
                      CH4_mod = pulse_emission_total,
                      DOY = Time_2,
                      site = site_2),
      by = c("DOY", "site")
    )
  
  return(data_results)
}
