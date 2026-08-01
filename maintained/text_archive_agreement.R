# coppock_2020/maintained/text_archive_agreement.R
# Output: output/text_archive_agreement.csv
# Depends on: original/replication_archive/*_simulated_data.csv, all seven figure
#   scripts, helpers.R
# Description: Re-derives every estimate behind the eight published figures straight
#   from the deposited data, using the model specifications the deposited scripts
#   use, and compares the result against what the figure scripts wrote to output/.
#   The chapter prints no estimates, so this is what makes the ground truth's
#   "archive" column mean something: without it, a claim that the rewrite reproduces
#   the deposit would rest on reading the two sets of scripts side by side.
#   The reshaping and labelling differ from the deposit on purpose, since that is
#   where a faithful-looking port can go wrong; the model calls do not.

source(here::here("maintained", "helpers.R"))

read_deposit <- function(f) {
  read_csv(here::here("original", "replication_archive", f), show_col_types = FALSE)
}

read_output <- function(f) {
  read_csv(here::here("maintained", "output", f), show_col_types = FALSE)
}

# Figure 17.1 ----
two_arm <- read_deposit("two_arm_simulated_data.csv")

two_arm_archive <- two_arm |>
  group_by(condition) |>
  reframe(tidy(lm_robust(Y ~ 1, data = pick(everything()))))

# Figure 17.2 ----
blocked <- read_deposit("blocked_simulated_data.csv")

blocked_archive <- bind_rows(
  blocked |>
    group_by(condition) |>
    reframe(tidy(lm_robust(Y ~ 1, weights = 1 / Z_cond_prob, data = pick(everything())))),
  blocked |>
    group_by(condition) |>
    reframe(tidy(lm_robust(Y ~ 1, data = pick(everything()))))
)

# Figure 17.3 ----
blocked_facets_archive <- bind_rows(
  blocked |>
    group_by(condition, neighborhood) |>
    reframe(tidy(lm_robust(Y ~ 1, data = pick(everything())))),
  blocked |>
    group_by(condition, neighborhood) |>
    reframe(tidy(lm_robust(Y ~ 1, data = pick(everything()))))
)

# Figure 17.4 ----
clustered <- read_deposit("clustered_simulated_data.csv")

clustered_archive <- bind_rows(
  clustered |>
    group_by(condition) |>
    reframe(tidy(lm_robust(Y ~ 1, clusters = class, data = pick(everything())))),
  clustered |>
    group_by(condition) |>
    reframe(tidy(lm_robust(Y ~ 1, data = pick(everything()))))
)

# Figure 17.5 ----
covariate <- read_deposit("covariate_simulated_data.csv") |>
  mutate(X_c = X - mean(X))

covariate <- covariate |>
  mutate(
    Y_Adjusted = residuals(lm(Y ~ X_c + X_c:Z, data = covariate)),
    Z_Adjusted = residuals(lm(Z ~ X_c + X_c:Z, data = covariate))
  )

covariate_archive <- bind_rows(
  tidy(lm_lin(Y ~ Z, covariates = ~X, data = covariate)),
  tidy(lm_robust(Y ~ Z + X_c + X_c:Z, data = covariate)),
  tidy(lm_robust(Y_Adjusted ~ Z_Adjusted, data = covariate))
) |>
  filter(term %in% c("Z", "Z_Adjusted"))

# Figure 17.6 ----
interaction <- read_deposit("interaction_simulated_data.csv")

interaction_archive <- lm_robust(Y ~ condition * X, data = interaction) |>
  margins(at = list(X = seq(-2, 2, by = 0.25))) |>
  summary() |>
  as_tibble() |>
  filter(factor == "conditionTreatment")

# Figure 17.7 ----
noncompliance <- read_deposit("noncompliance_simulated_data.csv")

noncompliance_archive <- noncompliance |>
  pivot_longer(cols = c(`Treatment Receipt`, Turnout), names_to = "dv", values_to = "value") |>
  group_by(Z, dv) |>
  reframe(tidy(lm_robust(value ~ 1, data = pick(everything()))))

# Figure 17.8 ----
attrition <- read_deposit("attrition_simulated_data.csv") |>
  mutate(
    `Lower Bound` = case_when(Z == 1 & is.na(Y) ~ 1, Z == 0 & is.na(Y) ~ 7, .default = Y),
    `Upper Bound` = case_when(Z == 1 & is.na(Y) ~ 7, Z == 0 & is.na(Y) ~ 1, .default = Y)
  ) |>
  pivot_longer(cols = c(`Lower Bound`, `Upper Bound`), names_to = "bound", values_to = "value")

attrition_archive <- attrition |>
  group_by(Z, bound) |>
  reframe(tidy(lm_robust(value ~ 1, data = pick(everything()))))

# Compare ----
compare <- function(figure, quantity, archive, rewrite) {
  columns <- intersect(c("estimate", "std.error", "conf.low", "conf.high"), names(archive))
  tibble(
    figure = figure,
    quantity = quantity,
    n_values = length(columns) * nrow(archive),
    max_abs_difference = max(abs(
      as.matrix(archive[order(archive$estimate), columns]) -
        as.matrix(rewrite[order(rewrite$estimate), columns])
    ))
  )
}

agreement <- bind_rows(
  compare("Figure 17.1", "Group means and intervals", two_arm_archive,
          read_output("figure_1_two_arm_estimates.csv")),
  compare("Figure 17.2", "Weighted and unweighted group means", blocked_archive,
          read_output("figure_2_blocked_estimates.csv")),
  compare("Figure 17.3", "Group means within each facet", blocked_facets_archive,
          read_output("figure_3_blocked_facets_estimates.csv")),
  compare("Figure 17.4", "Clustered and unclustered group means", clustered_archive,
          read_output("figure_4_clustered_estimates.csv")),
  compare("Figure 17.5", "Three covariate adjustment estimators", covariate_archive,
          read_output("figure_5_covariate_adjustment_estimators.csv")),
  compare("Figure 17.7", "Group means by assigned group", noncompliance_archive,
          read_output("figure_7_noncompliance_estimates.csv") |>
            filter(panel == "By assigned group")),
  compare("Figure 17.8", "Bounded group means", attrition_archive,
          read_output("figure_8_attrition_estimates.csv"))
)

# margins names its columns differently, so Figure 17.6 is compared on its own terms.
interaction_rewrite <- read_output("figure_6_interaction_cates.csv")
agreement <- bind_rows(
  agreement,
  tibble(
    figure = "Figure 17.6",
    quantity = "CATE across the covariate grid",
    n_values = 3 * nrow(interaction_archive),
    max_abs_difference = max(abs(
      as.matrix(interaction_archive[, c("AME", "lower", "upper")]) -
        as.matrix(interaction_rewrite[, c("AME", "lower", "upper")])
    ))
  )
) |>
  arrange(figure)

write_csv(agreement, here::here("maintained", "output", "text_archive_agreement.csv"))

print(agreement)
