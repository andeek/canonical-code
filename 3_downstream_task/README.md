# Canonicalization

After record linkage has been performed and the posterior draws of the linkage have been formatted, canonicalization can be performed.

This folder contains 2 scripts --

1. caswell_voters_canonical.R - Perform and store all methods of canonicalization for the caswell voters results.
2. geco_pp_weights.R - Estimate and store distributions of PC weights for each record in the geco data set.

Note that canonicalization for the GeCO data is performed synchronously with the downstream task and can be found in 3_downstream_task/geco_reg_script.R and 3_downstream_task/geco_coverage_script.R.