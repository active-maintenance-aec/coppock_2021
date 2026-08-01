# coppock_2020/maintained/make_datasets.R
# Output: output/simulated_data/*.csv, output/make_datasets_deposit_comparison.csv,
#         output/make_datasets_blocked_diagnostics.csv
# Depends on: original/replication_archive/*_simulated_data.csv, helpers.R
# Description: Ports the archive's data generator to the DeclareDesign 1.x API and
#   compares each regenerated dataset against the deposited one, column by column.
#   The figure scripts read the deposited data, not these; this script exists to
#   establish whether the generator still generates what was deposited.
#   Reveal steps use declare_reveal() rather than declare_measurement() with
#   reveal_outcomes(): the two are equivalent here, but the second draws two extra
#   random numbers per design and would move every design after the first off the
#   archive's random number stream, which is the thing this comparison measures.

source(here::here("maintained", "helpers.R"))

dir.create(here::here("maintained", "output", "simulated_data"),
           showWarnings = FALSE, recursive = TRUE)

set.seed(343)

# Two arm trial ----
two_arm_design <-
  declare_model(N = 500, U = rnorm(N)) +
  declare_potential_outcomes(Y ~ draw_binary(latent = 0.5 * Z + U, link = "probit")) +
  declare_assignment(
    Z = complete_ra(N, m = 100),
    Z_cond_prob = obtain_condition_probabilities(declare_ra(N = N, m = 100), Z)
  ) +
  declare_reveal()

two_arm <- draw_data(two_arm_design) |>
  mutate(condition = if_else(Z == 1, "Treatment", "Control"))

# Blocked ----
blocked_design <-
  declare_model(
    neighborhood = add_level(N = 2, lambda = c(10, 5)),
    resident = add_level(N = c(50, 100))
  ) +
  declare_potential_outcomes(Y ~ 0 * Z + -4 * Z * (neighborhood == 1) + rpois(N, lambda)) +
  declare_assignment(
    Z = block_ra(blocks = neighborhood, m = 25),
    Z_cond_prob = obtain_condition_probabilities(declare_ra(blocks = neighborhood, m = 25), Z)
  ) +
  declare_reveal()

blocked <- draw_data(blocked_design) |>
  mutate(condition = if_else(Z == 1, "Treatment", "Control"))

# Blocked diagnostics ----
# The archive's generator prints these two regressions before moving on, and the
# weighted one draws two random numbers on its way through estimatr. Those draws
# sit in the stream between the blocked design and every design declared after
# it, so dropping the pair as dead diagnostic code would change all five
# remaining datasets. They are kept, and written out instead of printed.
blocked_diagnostics <- bind_rows(
  tidy(lm_robust(Y ~ Z, data = blocked)) |>
    mutate(estimator = "Unweighted"),
  tidy(lm_robust(Y ~ Z, weights = 1 / Z_cond_prob, data = blocked)) |>
    mutate(estimator = "Inverse probability weighted")
)

# Clustered ----
clustered_design <-
  declare_model(
    class = add_level(
      N = 30,
      n_per_class = sample(10:20, size = N, replace = TRUE),
      class_shock = rnorm(N, mean = 1000, sd = 100)
    ),
    student = add_level(N = n_per_class, student_shock = rnorm(N, sd = 175))
  ) +
  declare_potential_outcomes(Y ~ 100 * Z + class_shock + student_shock) +
  declare_assignment(
    Z = cluster_ra(clusters = class),
    Z_cond_prob = obtain_condition_probabilities(declare_ra(clusters = class), Z)
  ) +
  declare_reveal()

clustered <- draw_data(clustered_design) |>
  mutate(condition = if_else(Z == 1, "Treatment", "Control"))

# Covariate adjustment ----
covariate_design <-
  declare_model(N = 100, U = rnorm(N, sd = 0.5), X = rnorm(N, mean = 3, sd = 1)) +
  declare_potential_outcomes(Y ~ 0.5 * Z + 1.0 * X + 0.5 * Z * X + U) +
  declare_assignment(
    Z = complete_ra(N),
    Z_cond_prob = obtain_condition_probabilities(declare_ra(N = N), Z)
  ) +
  declare_reveal()

covariate <- draw_data(covariate_design) |>
  mutate(condition = if_else(Z == 1, "Treatment", "Control"))

# Noncompliance ----
# Fully deterministic: the archive writes these counts out by hand rather than
# drawing them.
noncompliance <- tibble(
  Z = rep(c("Treatment", "Control"), each = 300),
  `Treatment Receipt` = rep(c(1, 0, 1, 0), c(200, 100, 50, 250)),
  Turnout = rep(c(1, 0, 1, 0, 1, 0, 1, 0), c(170, 30, 20, 80, 25, 50, 125, 100))
)

# Attrition ----
# fabricatr's draw_likert() no longer supplies default cut points, so the seven
# categories the archive relied on are written out here. The breaks are recovered
# from the deposited file, whose potential outcomes this reproduces exactly.
attrition_design <-
  declare_model(N = 200, U = rnorm(N)) +
  declare_potential_outcomes(
    Y ~ as.numeric(draw_likert(
      0.5 * Z + U,
      breaks = c(-Inf, -2.5, -1.5, -0.5, 0.5, 1.5, 2.5, Inf)
    ))
  ) +
  declare_potential_outcomes(R ~ draw_binary(latent = -0.3 * Z + U + 2, link = "probit")) +
  declare_assignment(
    Z = complete_ra(N),
    Z_cond_prob = obtain_condition_probabilities(declare_ra(N = N), Z)
  ) +
  declare_reveal(outcome_variables = "R") +
  declare_reveal(outcome_variables = "Y", attrition_variables = "R")

attrition <- draw_data(attrition_design)

# Interaction with a continuous covariate ----
interaction_design <-
  declare_model(N = 2500, noise = rnorm(N), X = rnorm(N)) +
  declare_potential_outcomes(
    Y_Z_1 = 2 * X^2 + 0.50 * X + noise,
    Y_Z_0 = 1.00 * X + noise
  ) +
  declare_sampling(
    S_inclusion_prob = pnorm(X),
    S = simple_rs(N, prob_unit = S_inclusion_prob),
    filter = S == 1
  ) +
  declare_assignment(
    Z = complete_ra(N),
    Z_cond_prob = obtain_condition_probabilities(declare_ra(N = N), Z)
  ) +
  declare_reveal()

interaction <- draw_data(interaction_design) |>
  select(-S) |>
  mutate(condition = if_else(Z == 1, "Treatment", "Control"))

# Write ----
regenerated <- list(
  two_arm = two_arm,
  blocked = blocked,
  clustered = clustered,
  covariate = covariate,
  noncompliance = noncompliance,
  attrition = attrition,
  interaction = interaction
)

iwalk(
  regenerated,
  function(d, nm) {
    write_csv(d, here::here("maintained", "output", "simulated_data",
                            paste0(nm, "_simulated_data.csv")))
  }
)

write_csv(blocked_diagnostics,
          here::here("maintained", "output", "make_datasets_blocked_diagnostics.csv"))

# Compare against the deposit ----
# A column agrees if every value matches to within 1e-10, missingness included.
compare_column <- function(x, y) {
  if (is.numeric(x) && is.numeric(y)) {
    all(is.na(x) == is.na(y)) && max(abs(x - y), na.rm = TRUE) < 1e-10
  } else {
    identical(as.character(x), as.character(y))
  }
}

comparison <- imap(
  regenerated,
  function(d, nm) {
    deposited <- read_csv(
      here::here("original", "replication_archive", paste0(nm, "_simulated_data.csv")),
      show_col_types = FALSE
    )
    shared <- intersect(names(d), names(deposited))
    tibble(
      dataset = nm,
      column = shared,
      agrees = map_lgl(shared, function(col) compare_column(d[[col]], deposited[[col]])),
      n_rows_regenerated = nrow(d),
      n_rows_deposited = nrow(deposited)
    )
  }
) |>
  list_rbind()

write_csv(comparison, here::here("maintained", "output",
                                 "make_datasets_deposit_comparison.csv"))

print(comparison |> count(dataset, agrees), n = 30)
