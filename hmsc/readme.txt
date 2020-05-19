This folder contains all the scripts and data used for Hmsc modelling specifically.

Anything on the cluster regarding Hmsc modelling should be initiated from this as a root dir.

pa_models and abundance_models contain my first attempt at models that are incorrectly specified.
These models lacked "Season" as a fixed effect variable.
They do define 4 distinct model types. Switching in and out spatial and environmental covariates.
Abundance models also struggle to converge using "lognormal poisson" distribution with large RHat values

pa_2 models attempt to improve on previous modelling errors and experiment with including predators as both response or predictor variables.

