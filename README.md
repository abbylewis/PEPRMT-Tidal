---
editor_options: 
  markdown: 
    wrap: 72
---

# PEPRMT-Tidal

PEPRMT-Tidal is a one-dimensional process-based model that predicts
gross primary productivity, ecosystem respiration and methane exchange
in tidal wetlands at the daily time step. If you plan to use the model
code or data provided in this repository please cite Oikawa et al.
(2024).

This package contains all the files needed to run PEPRMT-Tidal, an
updated version of the PEPRMT model. This model is described in detail
in Oikawa et al. (2024).

# About the model: 

Originally PEPRMT (the Peatland Ecosystem Photosynthesis and Methane
Transport) model was parameterized for restored freshwater wetlands in
the Sacramento-San Joaquin River Delta, CA, USA (Oikawa et al. 2017).
Presented here is an updated version that works for tidal wetlands
(Oikawa et al. 2024).

All PEPRMT modules use the same input structure, and all variables are
at the daily time step. Modules are run in succession, first GPP, then
R~eco~ and last CH~4~.

## GPP

The GPP module using a light use efficiency equation to predict GPP. GPP
can be predicted using leaf area index (LAI) or a greenness index from
Phenocam data, or remote sensing such as EVI or NDVI. PEPRMT-Tidal
applied in Oikawa et al. (2024) uses EVI from Landsat.

## R~eco~

The R~eco~ module uses a Dual Arrhenius Michaelis-Menten (DAMM) approach
to predict R~eco~ from two carbon pools.

## CH~4~

The CH~4~ module also uses the DAMM approach and includes inhibition
factors to decrease CH~4~ production in presence of NO~3~ and SO~4~

# About the data: 

We have included here an example dataset used in Oikawa et al. (2024),
which sources data from Ameriflux eddy covariance towers across the
United States. These data have been filtered and gapfilled following
methods outlined in Oikawa et al. 2023. Ancillary data from local tidal
streams included continuous NO~3~ or salinity measurements are also
included.

# About the vignette: 

We have also included a vignette that loads in the dataset, runs the
PEPRMT Tidal modules in sequence, and plots results. We hope this helps
future users to better understand how the data need to be organized and
how the modules run in sequence and feed into each other.

If you have any questions, please contact Patty Oikawa
[patty.oikawa\@gmail.com](mailto:patty.oikawa@gmail.com){.email} Thank
you!

# References

Oikawa, P. Y., Jenerette, G. D., Knox, S. H., Sturtevant, C.,
Verfaillie, J., Dronova, I., Poindexter, C. M., Eichelmann, E., &
Baldocchi, D. D. (2017). Evaluation of a hierarchy of models reveals
importance of substrate limitation for predicting carbon dioxide and
methane exchange in restored wetlands. Journal of Geophysical Research:
Biogeosciences, 122(1), 145–167. <https://doi.org/10.1002/2016JG003438>

Oikawa, P. Y., Sihi, D., Forbrich, I., Fluet-Chouinard, E., Najarro, M.,
Thomas, O., Shahan, J., Arias-Ortiz, A., Russell, S., Knox, S. H.,
McNicol, G., Wolfe, J., Windham-Myers, L., Stuart-Haentjens, E.,
Bridgham, S. D., Needelman, B., Vargas, R., Schäfer, K., Ward, E. J.,
Megonigal, P., & Holmquist, J. (2024). A New Coupled Biogeochemical
Modeling Approach Provides Accurate Predictions of Methane and Carbon
Dioxide Fluxes Across Diverse Tidal Wetlands. Journal of Geophysical
Research: Biogeosciences, 129(10), e2023JG007943.
<https://doi.org/10.1029/2023JG007943>
