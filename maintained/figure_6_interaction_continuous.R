# coppock_2021/maintained/figure_6_interaction_continuous.R
# Output: output/figure_6_interactions_good.pdf/.png, output/figure_6_interactions_bad.pdf/.png,
#         output/figure_6_interaction_fit.csv, output/figure_6_interaction_cates.csv,
#         output/figure_6_interaction_binned_residuals.csv
# Depends on: original/replication_archive/interaction_simulated_data.csv, helpers.R
# Description: Figure 17.6, an interaction with a continuous covariate drawn well
#   (two fitted lines in data space) and badly (a ladder of marginal effects).

source(here::here("maintained", "helpers.R"))

dat <- read_csv(
  here::here("original", "replication_archive", "interaction_simulated_data.csv"),
  show_col_types = FALSE
)

fit <- lm_robust(Y ~ condition * X, data = dat)

# The chapter draws a short segment at X = 1 in each arm to anchor the two slope
# labels.
anchor_df <- tibble(X = c(1, 1), condition = c("Control", "Treatment"))
anchor_df <- anchor_df |> mutate(Y = predict(fit, newdata = anchor_df))

label_df <- tibble(
  X = c(1, 0.5),
  Y = c(-3.5, 7),
  condition = c("Control", "Treatment"),
  label = c("Slope for control units", "Slope for treated units")
)

lines_df <- bind_rows(anchor_df, label_df)

good <-
  ggplot(dat, aes(X, Y, shape = condition, group = condition)) +
  geom_point(alpha = 0.2, stroke = 0) +
  stat_smooth(method = lm_robust, formula = y ~ x, fullrange = TRUE, color = "black") +
  geom_line(data = lines_df) +
  geom_label(data = label_df, aes(label = label)) +
  coord_cartesian(xlim = c(-2, 2), ylim = c(-5, 10)) +
  xlab("Continuous pre-treatment covariate") +
  ylab("Outcome") +
  theme_bw() +
  theme(legend.position = "none")

# The bad panel evaluates the marginal effect of treatment across a grid of the
# covariate. The grid runs past the observed range of X at its lower end, which
# margins warns about and the chapter's point survives.
cates <- fit |>
  margins(at = list(X = seq(-2, 2, by = 0.25))) |>
  summary() |>
  as_tibble() |>
  filter(factor == "conditionTreatment")

bad <-
  ggplot(cates, aes(X, AME)) +
  geom_point() +
  coord_cartesian(xlim = c(-2, 2), ylim = c(-5, 10)) +
  geom_linerange(aes(ymin = lower, ymax = upper)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Continuous pre-treatment covariate") +
  ylab("Estimated CATE at each level of the covariate") +
  theme_bw()

# The chapter reads a lack of fit off this figure and says it is worst at low values
# of the covariate, so the mean residual is written out by covariate bin.
binned_residuals <- dat |>
  mutate(
    residual = Y - predict(fit, newdata = dat),
    X_bin = cut(X, breaks = seq(-2, 4, by = 0.5))
  ) |>
  group_by(X_bin) |>
  summarise(n = n(), mean_residual = mean(residual), .groups = "drop")

write_csv(tidy(fit),
          here::here("maintained", "output", "figure_6_interaction_fit.csv"))
write_csv(binned_residuals,
          here::here("maintained", "output", "figure_6_interaction_binned_residuals.csv"))
write_csv(cates,
          here::here("maintained", "output", "figure_6_interaction_cates.csv"))

ggsave(here::here("maintained", "output", "figure_6_interactions_good.pdf"),
       good, width = 4, height = 4)
ggsave(here::here("maintained", "output", "figure_6_interactions_good.png"),
       good, width = 4, height = 4, dpi = 300)
ggsave(here::here("maintained", "output", "figure_6_interactions_bad.pdf"),
       bad, width = 4, height = 4)
ggsave(here::here("maintained", "output", "figure_6_interactions_bad.png"),
       bad, width = 4, height = 4, dpi = 300)
