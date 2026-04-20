# Methane Exchange (CH4)

Methane production and transport module of the PEPRMT model (v1.0).

## Usage

``` r
PEPRMT_CH4(
  data,
  wetland_type,
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

  Data frame containing 18 required columns used as model inputs. See
  **Details** for expected column names.

- wetland_type:

  Integer indicating wetland class: 1 = Freshwater peatland, 2 = Tidal
  wetland.

- Ea_SOM_CH4:

  Activation energy for methane production from soil organic matter (kJ
  mol^-1)

- kM_SOM_CH4:

  Half-saturation constant for SOM methane production (g C m^-3 soil)

- Ea_labile_CH4:

  Activation energy for methane production from labile carbon (kJ
  mol^-1)

- kM_labile_CH4:

  Half-saturation constant for labile methane production (g C m^-3 soil)

- Ea_oxi_CH4:

  Activation energy for methane oxidation (kJ mol^-1)

- kM_oxi_CH4:

  Half-saturation constant for methane oxidation (g C m^-3 soil)

- kI_SO4:

  – Sulfate inhibition constant (mg L^-1)

- kI_NO3:

  – Nitrate inhibition constant (mg L^-1)

- k_plant_oxi:

  Fraction of CH4 oxidized during transport

## Value

Updated dataframe containing:

- CH4_mod:

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

Runs the PEPRMT methane production and transport module for freshwater
peatlands or tidal wetlands at a daily time step. Default parameter
values were determined via MCMC Bayesian fitting (Oikawa et al. 2024).

The PEPRMT model was originally parameterized for restored freshwater
wetlands in the Sacramento–San Joaquin River Delta, California, USA
(Oikawa et al. 2017) and later updated for tidal wetlands with
inhibition of methane production in response to salinity and nitrate
(Oikawa et al. 2024).

Modules are intended to be run sequentially: PEPRMT_GPP, then
PEPRMT_Reco, then PEPRMT_CH4.

All variables are expected at a daily time step.

All PEPRMT modules use the same input structure, although not all
variables are used in every module.

**Required data columns:**

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

16. Modeled GPP (g C m^-2 day^-1)

17. Modeled Reco (g C m^-2 day^-1)

18. Net ecosystem exchange (g C m^-2 day^-1)

## References

Oikawa, P. Y., Jenerette, G. D., Knox, S. H., Sturtevant, C.,
Verfaillie, J., Dronova, I., Poindexter, C. M., Eichelmann, E., &
Baldocchi, D. D. (2017). Evaluation of a hierarchy of models reveals
importance of substrate limitation for predicting carbon dioxide and
methane exchange in restored wetlands. Journal of Geophysical Research:
Biogeosciences, 122(1), 145–167. https://doi.org/10.1002/2016JG003438

Oikawa, P. Y., Sihi, D., Forbrich, I., Fluet-Chouinard, E., Najarro, M.,
Thomas, O., Shahan, J., Arias-Ortiz, A., Russell, S., Knox, S. H.,
McNicol, G., Wolfe, J., Windham-Myers, L., Stuart-Haentjens, E.,
Bridgham, S. D., Needelman, B., Vargas, R., Schäfer, K., Ward, E. J.,
Megonigal, P., & Holmquist, J. (2024). A New Coupled Biogeochemical
Modeling Approach Provides Accurate Predictions of Methane and Carbon
Dioxide Fluxes Across Diverse Tidal Wetlands. Journal of Geophysical
Research: Biogeosciences, 129(10), e2023JG007943.
https://doi.org/10.1029/2023JG007943

## Examples

``` r
# Example
# data(example_dataset)
# theta <- c(14.9025078, 0.4644174, 16.7845002, 0.4359649, 15.8857612, 
# 0.5120464, 486.4106939, 0.1020278)
# out <- PEPRMT_CH4(theta, example_dataset, wetland_type = 2)
```
