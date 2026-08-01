# coppock_2020/maintained/text_design_parameters.R
# Output: output/text_design_parameters.csv
# Depends on: original/replication_archive/*_simulated_data.csv, helpers.R
# Description: The design facts the chapter states in prose (sample sizes, numbers
#   treated, block and cluster counts, response scales), read off the deposited data
#   so that the ground truth compares the text against the files rather than against
#   a description of them.

source(here::here("maintained", "helpers.R"))

two_arm <- read_csv(here::here("original", "replication_archive", "two_arm_simulated_data.csv"), show_col_types = FALSE)
blocked <- read_csv(here::here("original", "replication_archive", "blocked_simulated_data.csv"), show_col_types = FALSE)
clustered <- read_csv(here::here("original", "replication_archive", "clustered_simulated_data.csv"), show_col_types = FALSE)
covariate <- read_csv(here::here("original", "replication_archive", "covariate_simulated_data.csv"), show_col_types = FALSE)
interaction <- read_csv(here::here("original", "replication_archive", "interaction_simulated_data.csv"), show_col_types = FALSE)
noncompliance <- read_csv(here::here("original", "replication_archive", "noncompliance_simulated_data.csv"), show_col_types = FALSE)
attrition <- read_csv(here::here("original", "replication_archive", "attrition_simulated_data.csv"), show_col_types = FALSE)

blocked_counts <- blocked |> count(neighborhood, name = "n_residents")
blocked_treated <- blocked |> filter(Z == 1) |> count(neighborhood, name = "n_treated")
clustered_sizes <- clustered |> distinct(class, n_per_class)

design_parameters <- tibble(
  figure = c(
    rep("Figure 17.1", 2),
    rep("Figures 17.2 and 17.3", 5),
    rep("Figure 17.4", 5),
    rep("Figure 17.5", 2),
    rep("Figure 17.6", 1),
    rep("Figure 17.7", 3),
    rep("Figure 17.8", 3)
  ),
  quantity = c(
    "N", "N treated",
    "N", "Number of neighborhoods", "N in neighborhood 1", "N in neighborhood 2",
    "N treated per neighborhood",
    "N students", "Number of classes", "Smallest class", "Largest class",
    "Outcome range",
    "N", "Number of distinct outcome values",
    "N",
    "N", "N assigned to treatment", "N assigned to control",
    "N", "N missing an outcome", "Outcome range"
  ),
  value = c(
    as.character(nrow(two_arm)),
    as.character(sum(two_arm$Z == 1)),
    as.character(nrow(blocked)),
    as.character(n_distinct(blocked$neighborhood)),
    as.character(blocked_counts$n_residents[blocked_counts$neighborhood == 1]),
    as.character(blocked_counts$n_residents[blocked_counts$neighborhood == 2]),
    paste(unique(blocked_treated$n_treated), collapse = ", "),
    as.character(nrow(clustered)),
    as.character(n_distinct(clustered$class)),
    as.character(min(clustered_sizes$n_per_class)),
    as.character(max(clustered_sizes$n_per_class)),
    paste(round(range(clustered$Y)), collapse = " to "),
    as.character(nrow(covariate)),
    as.character(n_distinct(covariate$Y)),
    as.character(nrow(interaction)),
    as.character(nrow(noncompliance)),
    as.character(sum(noncompliance$Z == "Treatment")),
    as.character(sum(noncompliance$Z == "Control")),
    as.character(nrow(attrition)),
    as.character(sum(is.na(attrition$Y))),
    paste(range(attrition$Y, na.rm = TRUE), collapse = " to ")
  )
)

write_csv(design_parameters,
          here::here("maintained", "output", "text_design_parameters.csv"))

print(design_parameters, n = nrow(design_parameters))
