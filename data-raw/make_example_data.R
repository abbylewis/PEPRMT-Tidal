all_sites <- read.csv("data-raw/All_sites_master.csv")

example_data <- all_sites[c(
  "DOY", "DOY_disc", "Year", "TA_C", "WTD_cm",
  "PAR_umol_m2_day", "LAI", "EVI", "FPAR", "LUE",
  "Wetland_age_years", "Salinity_daily_ave_ppt",
  "NO3_mg_L", "SOM_MEM_gC_m3", "site", "site_char",
  "CO2_gC_m2_day", "GPP_gC_m2_day", "Reco_gC_m2_day",
  "CH4_gC_m2_day"
)]

usethis::use_data(example_data, overwrite = TRUE)
