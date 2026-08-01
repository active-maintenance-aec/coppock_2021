# coppock_2020/maintained/text_stata_equivalent.R
# Output: output/text_stata_equivalent.csv
# Depends on: original/replication_archive/two_arm_simulated_data.csv, helpers.R
# Description: The deposit's README says its Stata file "produces something similar
#   to Figure 1". That is a claim about output nobody here can run, so the .do file's
#   six lines of arithmetic are reproduced in R and compared against the intervals
#   the R script draws. Stata computes a normal-approximation interval from the
#   unpooled group standard deviation; the R script draws an HC2 interval on the
#   t distribution. The two are close but not the same, which is what "similar" gets
#   to mean here.

source(here::here("maintained", "helpers.R"))

dat <- read_csv(
  here::here("original", "replication_archive", "two_arm_simulated_data.csv"),
  show_col_types = FALSE
)

stata_intervals <- dat |>
  group_by(condition) |>
  summarise(
    estimate = mean(Y),
    std.error = sd(Y) / sqrt(n()),
    .groups = "drop"
  ) |>
  mutate(
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    method = "Stata .do file: sd / sqrt(N), normal approximation"
  )

r_intervals <- dat |>
  group_by(condition) |>
  reframe(tidy(lm_robust(Y ~ 1, data = pick(everything())))) |>
  transmute(condition, estimate, std.error, conf.low, conf.high,
            method = "R script: lm_robust HC2, t distribution")

comparison <- bind_rows(stata_intervals, r_intervals) |>
  arrange(condition, method)

write_csv(comparison,
          here::here("maintained", "output", "text_stata_equivalent.csv"))

print(comparison)
