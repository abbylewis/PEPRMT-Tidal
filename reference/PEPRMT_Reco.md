# Ecosystem Respiration (Reco)

Ecosystem respiration (Reco) module of the PEPRMT model (v1.0).

## Usage

``` r
PEPRMT_Reco(
  data,
  wetland_type,
  Ea_SOM = 18.41329,
  kM_SOM = 1487.65701,
  Ea_labile = 11.65972,
  kM_labile = 61.29611
)
```

## Arguments

- data:

  Data frame containing 16 required columns used as model inputs. See
  **Details** for expected column structure.

- wetland_type:

  Integer indicating wetland class: 1 = Freshwater peatland, 2 = Tidal
  wetland.

- Ea_SOM:

  – Activation energy controlling the temperature sensitivity of
  decomposition from the soil organic matter (SOM) pool (kJ mol^-1).

- kM_SOM:

  – Half-saturation constant for microbial decomposition of the SOM pool
  (g C m^-3 soil). Determines substrate limitation strength for SOM
  respiration.

- Ea_labile:

  – Activation energy controlling the temperature sensitivity of
  decomposition from the labile carbon pool (kJ mol^-1).

- kM_labile:

  – Half-saturation constant for microbial decomposition of the labile
  carbon pool (g C m^-3 soil). Determines substrate limitation strength
  for labile respiration.

## Value

Updated dataframe containing:

- Reco_mod:

  Total ecosystem respiration (g C CO2 m^-2 day^-1)

- NEE_mod:

  Net ecosystem exchange of CO2 (g C CO2 m^-2 day^-1)

- S1:

  Labile soil carbon pool (g C m^-3, top meter of soil)

- S2:

  Soil organic carbon pool (g C m^-3, top meter of soil)

## Details

Runs the PEPRMT ecosystem respiration module for freshwater peatlands or
tidal wetlands at a daily time step.

The PEPRMT model was originally parameterized for restored freshwater
wetlands in the Sacramento–San Joaquin River Delta, California, USA
(Oikawa et al. 2017) and later updated for tidal wetlands (Oikawa et al.
2023).

Modules are intended to be run sequentially: PEPRMT_GPP, then
PEPRMT_Reco, then PEPRMT_CH4.

All variables are expected at a daily time step.

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
# theta <- c(18.4, 1487.6, 11.6, 61.3)
# out <- PEPRMT_Reco(theta, example_dataset, wetland_type = 2)
```
