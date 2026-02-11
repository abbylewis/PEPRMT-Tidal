#' run_PEPRMT
#'
#' @param target target dataframe
#'
#' @returns filled dataframe with model results
#' @export
#'
run_PEPRMT <- function(target) {
  target <- data.frame(target)
  #First run GPP Module
  GPP_theta <- c(0.7479271, 1.0497113, 149.4681710, 94.4532674)
  GPP_mod_target <- PEPRMT_GPP(theta = GPP_theta, data = target)
  
  #Create a new dataset that included model results
  target_results <- target |>
    dplyr::left_join(GPP_mod_target |>
                dplyr::rename(GPP_mod = GPP, 
                              DOY = Time_2,
                              site = site_2),
              by = c("DOY", "site"))
  
  #Second run Reco Module
  #Add modeled GPP into data before running Reco module (16th column)
  target$GPP_mod <- target_results$GPP_mod
  
  Reco_theta <- c(18.41329, 1487.65701, 11.65972, 61.29611)
  Reco_mod_target <- PEPRMT_Reco(Reco_theta, 
                                 data = target, 
                                 wetland_type = 2)
  
  #Create a new dataset that included model results
  target_results <- target_results |>
    dplyr::left_join(Reco_mod_target |>
                dplyr::rename(DOY = Time_2, 
                              Reco_mod = Reco_full,
                              site = site_2),
              by = c("DOY", "site"))
  
  #Last, run CH4 module
  #Add modeled S1, S2 into data before running CH4 module (17th & 18th columns)
  target$SOM_total <- target_results$S1
  target$SOM_labile <- target_results$S2
  
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
  
  CH4_mod_target <- PEPRMT_CH4(theta = CH4_theta,
                                     data = target,
                                     wetland_type = 2)
  
  #Create a new dataset that includes model results
  target_results <- target_results |>
    dplyr::left_join(
      CH4_mod_target |>
        dplyr::rename(DOY = Time_2, 
                      CH4_mod = pulse_emission_total,
                      DOY = Time_2,
                      site = site_2),
      by = c("DOY", "site")
    )
  
  return(target_results)
}
