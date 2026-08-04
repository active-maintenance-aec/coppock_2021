# coppock_2021/maintained/text_descriptive_claims.R
# Output: output/text_descriptive_claims.csv
# Depends on: original/replication_archive/*_simulated_data.csv, helpers.R
# Description: The chapter states three bounds on its simulated outcomes: a
#   400 to 1600 point scale for the clustered example, a seven-point Likert scale
#   for the attrition example, and a seven-point Likert raw scale on the vertical
#   axis of the covariate figure. A bound cannot be checked without the bound, so
#   this is the one script in the rewrite that holds numbers taken from the
#   chapter. They are comparison targets only: nothing here estimates anything,
#   and no figure or estimate anywhere in maintained/ reads this file.

source(here::here("maintained", "helpers.R"))

clustered <- read_csv(here::here("original", "replication_archive", "clustered_simulated_data.csv"), show_col_types = FALSE)
covariate <- read_csv(here::here("original", "replication_archive", "covariate_simulated_data.csv"), show_col_types = FALSE)
attrition <- read_csv(here::here("original", "replication_archive", "attrition_simulated_data.csv"), show_col_types = FALSE)

attrition_observed <- attrition$Y[!is.na(attrition$Y)]

descriptive_claims <- tibble(
  figure = c(
    rep("Figure 17.4", 3),
    rep("Figure 17.5", 2),
    rep("Figure 17.8", 2)
  ),
  quantity = c(
    "Students with an outcome inside the stated 400 to 1600 scale",
    "Students with an outcome outside the stated 400 to 1600 scale",
    "Students in total",
    "Units with an outcome outside the 1 to 7 range the axis label implies",
    "Units in total",
    "Observed outcomes outside the stated 1 to 7 Likert scale",
    "Distinct Likert categories the draws use, of the stated seven"
  ),
  target = c("441", "0", "441", "0", "100", "0", "7"),
  value = c(
    as.character(sum(clustered$Y >= 400 & clustered$Y <= 1600)),
    as.character(sum(clustered$Y < 400 | clustered$Y > 1600)),
    as.character(nrow(clustered)),
    as.character(sum(covariate$Y < 1 | covariate$Y > 7)),
    as.character(nrow(covariate)),
    as.character(sum(attrition_observed < 1 | attrition_observed > 7)),
    as.character(n_distinct(attrition_observed))
  )
)

write_csv(descriptive_claims,
          here::here("maintained", "output", "text_descriptive_claims.csv"))

print(descriptive_claims, n = nrow(descriptive_claims))
