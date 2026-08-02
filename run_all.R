# coppock_2020/run_all.R
# Runs the whole reproduction in order: fetch and verify the deposited archive,
# then the published figures, then the data generator and the in-text quantities.
# Every script is self-contained and can also be run on its own.

library(here)
here::i_am("run_all.R")

# Deposited archive ----
# Downloads from Dataverse on a fresh clone; verifies checksums either way.
source(here::here("download_original.R"))

# Figures ----
source(here::here("maintained", "figure_1_two_arm_trial.R"))
source(here::here("maintained", "figure_2_blocked_experiment.R"))
source(here::here("maintained", "figure_4_clustered_experiment.R"))
source(here::here("maintained", "figure_5_covariate_adjustment.R"))
source(here::here("maintained", "figure_6_interaction_continuous.R"))
source(here::here("maintained", "figure_7_noncompliance.R"))
source(here::here("maintained", "figure_8_attrition.R"))

# In-text quantities ----
source(here::here("maintained", "text_design_parameters.R"))
source(here::here("maintained", "text_stata_equivalent.R"))
# Reads the figure scripts' output, so it comes after them.
source(here::here("maintained", "text_archive_agreement.R"))

# Data generator ----
# The figures read the deposited data, not the data this regenerates, so it runs
# last. It is the only script that draws random numbers.
source(here::here("maintained", "make_datasets.R"))

# Ground truth ----
# Rebuilds the comparison table from the outputs above, so it cannot go stale.
source(here::here("ground_truth", "build_ground_truth.R"))

# Deposited archive, again ----
# The check at the top of this file is a precondition: it says original/ was intact
# before anything ran. Nothing above writes to original/, and this second pass is what
# demonstrates it rather than assuming it. Nothing is downloaded; the files are already
# present and are re-checked against the manifest on checksum, byte size and membership.
source(here::here("download_original.R"))
