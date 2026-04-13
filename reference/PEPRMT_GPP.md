# Gross Primary Productivity (GPP)

Gross Primary Productivity (GPP) module of the PEPRMT model (v1.0).

## Usage

``` r
PEPRMT_GPP(
  data,
  theta = c(0.7479271, 1.0497113, 149.468171, 94.4532674),
  T_opt_GPP = 25 + 274.15
)
```

## Arguments

- data:

  Data frame containing 15 required columns used as model inputs. See
  **Details** for expected column structure.

- theta:

  Numeric vector of length 4 containing calibrated parameter values.
  Default values were determined via MCMC Bayesian fitting (Oikawa et
  al. 2023). a0: Empirical intercept parameter for the fPAR scaling
  function (unitless). a1: Empirical slope parameter for the fPAR
  scaling function (unitless). Ha: Activation energy governing the
  temperature response of photosynthesis for general crop-type
  vegetation (kJ mol^-1). Controls the rate of increase in GPP with
  temperature below the thermal optimum. Hd: Deactivation energy
  controlling the decline in photosynthesis above the thermal optimum
  (kJ mol^-1). Determines the rate of decrease in GPP at high
  temperatures.

- T_opt_GPP:

  Temperature optimum for GPP

## Value

Updated dataframe containing:

- GPP:

  gross primary productivity (g C CO2 m^-2 d^-1)

- APAR:

  absorbed photosynthetically active radiation (umol m^-2 d^-1)

## Details

Runs the PEPRMT gross primary productivity module for freshwater
peatlands or tidal wetlands at a daily time step.

The PEPRMT model was originally parameterized for restored freshwater
wetlands in the Sacramento–San Joaquin River Delta, California, USA
(Oikawa et al. 2017) and later updated for tidal wetlands (Oikawa et al.
2023).

Modules are intended to be run sequentially: PEPRMT_GPP, then
PEPRMT_Reco, then PEPRMT_CH4.

All variables are expected at a daily time step.

This model predicts GPP using a light use efficiency equation GPP can be
predicted using leaf area index (LAI) or a greenness index from Phenocam
data or remote sensing such as EVI or NDVI PEPRMT-Tidal applied in
Oikawa et al. 2023 uses EVI from Landsat

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
# out <- PEPRMT_GPP(theta, example_dataset, wetland_type = 2)
```
