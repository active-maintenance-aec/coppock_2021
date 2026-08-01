# coppock_2020/maintained/figure_5_covariate_adjustment.R
# Output: output/figure_5_covariate_adjustment_good.pdf/.png,
#         output/figure_5_covariate_adjustment_estimators.csv,
#         output/figure_5_covariate_adjustment_panel_ranges.csv
# Depends on: original/replication_archive/covariate_simulated_data.csv, helpers.R
# Description: Figure 17.5, the residual-residual plot for covariate adjustment, and
#   the three estimators the chapter's code compares behind it.

source(here::here("maintained", "helpers.R"))

dat <- read_csv(
  here::here("original", "replication_archive", "covariate_simulated_data.csv"),
  show_col_types = FALSE
) |>
  mutate(X_c = X - mean(X))

fit_y <- lm(Y ~ X_c + X_c:Z, data = dat)
fit_z <- lm(Z ~ X_c + X_c:Z, data = dat)

dat <- dat |>
  mutate(
    Y_Adjusted = residuals(fit_y),
    Z_Adjusted = residuals(fit_z),
    Y_Unadjusted = Y,
    Z_Unadjusted = Z
  )

# The archive prints three estimators here under the comment "standard errors
# v. slightly off; point estimates OK". Writing them out puts that claim on the
# record instead of leaving it in a console that no longer exists.
estimators <- bind_rows(
  tidy(lm_lin(Y ~ Z, covariates = ~X, data = dat)) |>
    mutate(estimator = "lm_lin"),
  tidy(lm_robust(Y ~ Z + X_c + X_c:Z, data = dat)) |>
    mutate(estimator = "Interacted, centred covariate"),
  tidy(lm_robust(Y_Adjusted ~ Z_Adjusted, data = dat)) |>
    mutate(estimator = "Residual on residual")
) |>
  filter(term %in% c("Z", "Z_Adjusted"))

gg_df <- dat |>
  select(ID, Y_Unadjusted, Y_Adjusted, Z_Unadjusted, Z_Adjusted) |>
  pivot_longer(cols = -ID, names_to = "key", values_to = "value") |>
  separate(key, into = c("variable", "estimation"), sep = "_") |>
  pivot_wider(id_cols = c(ID, estimation), names_from = variable, values_from = value) |>
  mutate(estimation = factor(estimation, levels = c("Unadjusted", "Adjusted")))

# The chapter's panels are drawn on fixed ranges; these four invisible points set
# them without clipping any data.
blank_df <- tibble(
  Z = 0,
  estimation = factor(c("Unadjusted", "Unadjusted", "Adjusted", "Adjusted"),
                      levels = c("Unadjusted", "Adjusted")),
  Y = c(0, 10, -5, 5)
)

good <-
  ggplot(gg_df, aes(Z, Y)) +
  geom_point(alpha = 0.4, stroke = 0) +
  geom_blank(data = blank_df) +
  stat_smooth(method = lm_robust, formula = y ~ x, color = "grey", alpha = 0.5) +
  scale_y_continuous(breaks = -6:12) +
  theme_bw() +
  theme(strip.background = element_blank()) +
  ylab("Outcome variable (raw scale is 7-point Likert)") +
  xlab("Randomly assigned treatment") +
  facet_wrap(~estimation, scales = "free")

write_csv(estimators,
          here::here("maintained", "output",
                     "figure_5_covariate_adjustment_estimators.csv"))

# The chapter's claim about this figure is that the two facets share a vertical
# span without sharing a range, so the spans are written out and compared.
write_csv(
  blank_df |>
    group_by(estimation) |>
    summarise(y_min = min(Y), y_max = max(Y), y_span = max(Y) - min(Y), .groups = "drop"),
  here::here("maintained", "output",
             "figure_5_covariate_adjustment_panel_ranges.csv")
)

ggsave(here::here("maintained", "output", "figure_5_covariate_adjustment_good.pdf"),
       good, width = 4, height = 4)
ggsave(here::here("maintained", "output", "figure_5_covariate_adjustment_good.png"),
       good, width = 4, height = 4, dpi = 300)
