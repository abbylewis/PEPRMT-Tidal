# run_PEPRMT

Wrapper function to run all steps of the PEPRMT model (v1.0).

## Usage

``` r
run_PEPRMT(
  data,
  wetland_type,
  a0 = 0.7479271,
  a1 = 1.0497113,
  Ha = 149.468171 + 30,
  Hd = 94.4532674 + 100,
  T_opt_GPP = 25 + 274.15,
  Ea_SOM = 18.41329,
  kM_SOM = 1487.65701,
  Ea_labile = 11.65972,
  kM_labile = 61.29611,
  Ea_SOM_CH4 = 14.9025078 + 67.1,
  kM_SOM_CH4 = 0.4644174 + 17,
  Ea_labile_CH4 = 16.7845002 + 71.1,
  kM_labile_CH4 = 0.4359649 + 23,
  Ea_oxi_CH4 = 15.8857612 + 75.4,
  kM_oxi_CH4 = 0.5120464 + 23,
  kI_SO4 = 486.4106939,
  kI_NO3 = 0.1020278,
  k_plant_oxi = 0.35
)
```

## Arguments

- data:

  Data frame containing 15 required columns used as model inputs. See
  **Details** for expected column structure.

- wetland_type:

  Integer indicating wetland class: 1 = Freshwater peatland, 2 = Tidal
  wetland.

- a0:

  Empirical intercept parameter for the fPAR scaling function
  (unitless). Used in GPP module.

- a1:

  Empirical slope parameter for the fPAR scaling function (unitless).
  Used in GPP module.

- Ha:

  Activation energy governing the temperature response of photosynthesis
  for general crop-type vegetation (kJ mol^-1). Controls the rate of
  increase in GPP with temperature below the thermal optimum. Used in
  GPP module.

- Hd:

  Deactivation energy controlling the decline in photosynthesis above
  the thermal optimum (kJ mol^-1). Determines the rate of decrease in
  GPP at high temperatures. Used in GPP module.

- T_opt_GPP:

  Temperature optimum for GPP. Used in GPP module.

- Ea_SOM:

  – Activation energy controlling the temperature sensitivity of
  decomposition from the soil organic matter (SOM) pool (kJ mol^-1).
  Used in Reco module.

- kM_SOM:

  – Half-saturation constant for microbial decomposition of the SOM pool
  (g C m^-3 soil). Determines substrate limitation strength for SOM
  respiration. Used in Reco module.

- Ea_labile:

  Activation energy controlling the temperature sensitivity of
  decomposition from the labile carbon pool (kJ mol^-1). Used in Reco
  module.

- kM_labile:

  Half-saturation constant for microbial decomposition of the labile
  carbon pool (g C m^-3 soil). Determines substrate limitation strength
  for labile respiration. Used in Reco module.

- Ea_SOM_CH4:

  Activation energy for methane production from soil organic matter (kJ
  mol^-1). Used in CH4 module.

- kM_SOM_CH4:

  Half-saturation constant for SOM methane production (g C m^-3 soil).
  Used in CH4 module.

- Ea_labile_CH4:

  Activation energy for methane production from labile carbon (kJ
  mol^-1). Used in CH4 module.

- kM_labile_CH4:

  Half-saturation constant for labile methane production (g C m^-3
  soil). Used in CH4 module.

- Ea_oxi_CH4:

  Activation energy for methane oxidation (kJ mol^-1). Used in CH4
  module.

- kM_oxi_CH4:

  Half-saturation constant for methane oxidation (g C m^-3 soil). Used
  in CH4 module.

- kI_SO4:

  Sulfate inhibition constant (mg L^-1). Used in CH4 module.

- kI_NO3:

  Nitrate inhibition constant (mg L^-1). Used in CH4 module.

- k_plant_oxi:

  Fraction of CH4 oxidized during transport. Used in CH4 module.

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
