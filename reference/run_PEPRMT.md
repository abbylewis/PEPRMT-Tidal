# run_PEPRMT

Wrapper function to run all steps of the PEPRMT model (v1.0).

## Usage

``` r
run_PEPRMT(
  data,
  wetland_type,
  GPP_theta = c(0.7479271, 1.0497113, 149.468171, 94.4532674),
  Reco_theta = c(18.41329, 1487.65701, 11.65972, 61.29611),
  CH4_theta = c(14.9025078 + 67.1, 0.4644174 + 17, 16.7845002 + 71.1, 0.4359649 + 23,
    15.8857612 + 75.4, 0.5120464 + 23, 486.4106939, 0.1020278),
  k_plant_oxi = 0.35,
  T_opt_GPP = 25 + 274.15
)
```

## Arguments

- data:

  Data frame containing 15 required columns used as model inputs. See
  **Details** for expected column structure.

- wetland_type:

  Integer indicating wetland class: 1 = Freshwater peatland, 2 = Tidal
  wetland.

- GPP_theta:

  Numeric vector of length 4 containing calibrated GPP parameter values.
  Default values were determined via MCMC Bayesian fitting (Oikawa et
  al. 2023).

- Reco_theta:

  Numeric vector of length 4 containing calibrated Reco parameter
  values. Default values were determined via MCMC Bayesian fitting
  (Oikawa et al. 2023).

- CH4_theta:

  Numeric vector of length 8 containing calibrated CH4 parameter values.
  Default values were determined via MCMC Bayesian fitting (Oikawa et
  al. 2023).

- k_plant_oxi:

  Fraction of CH4 oxidized during transport

- T_opt_GPP:

  Optimum temperature for GPP

## Value

Updated dataframe containing:

- GPP:

  gross primary productivity (g C CO2 m^-2 day^-1)

- APAR:

  absorbed photosynthetically active radiation (umol m-2 d-1)

- Reco_full:

  Total ecosystem respiration (g C CO2 m^-2 day^-1)

- NEE_mod:

  Net ecosystem exchange of CO2 (g C CO2 m^-2 day^-1)

- S1:

  Labile soil carbon pool (g C m^-3, top meter of soil)

- S2:

  Soil organic carbon pool (g C m^-3, top meter of soil)

- pulse_emission_total:

  total methane emitted (g C CH4 m^-2 day^-1)

- Plant_flux_net:

  net methane flux via plant-mediated transport (g C CH4 m^-2 day^-1)

- Hydro_flux:

  net diffusive methane flux from water to atmosphere (g C CH4 m^-2
  day^-1)

- M1:

  methane pool produced from labile soil carbon (g C CH4 m^-3, top meter
  of soil and water)

- M2:

  methane pool produced from soil organic carbon (g C CH4 m^-3, top
  meter of soil and water)

- trans2:

  fraction of methane released via plant-mediated transport (unitless)

## Details

Runs all PEPRMT functions and returns an output dataframe with modeled
GPP, Reco, and CH4

The PEPRMT model was originally parameterized for restored freshwater
wetlands in the Sacramento–San Joaquin River Delta, California, USA
(Oikawa et al. 2017) and later updated for tidal wetlands with
inhibition of methane production in response to salinity and nitrate
(Oikawa et al. 2024).

Modules are run sequentially: PEPRMT_GPP, then PEPRMT_Reco, then
PEPRMT_CH4.

All variables are expected at a daily time step.

All PEPRMT modules use the same input structure, although not all
variables are used in every module.

**Expected data column order:**

1.  Continuous day of year

2.  Discontinuous day of year

3.  Year

4.  Air temperature (°C)

5.  Water table depth (cm)

6.  PAR (µmol m^-2 d^-1)

7.  Leaf Area Index

8.  Greenness Index

9.  FPAR flag

10. Light Use Efficiency

11. Wetland age (years)

12. Salinity (ppt)

13. NO3 (mg L^-1)

14. Soil organic matter (g C m^-3)

15. Site identifier
