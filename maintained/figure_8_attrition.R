# coppock_2021/maintained/figure_8_attrition.R
# Output: output/figure_8_attrition_good.pdf/.png, output/figure_8_attrition_estimates.csv
# Depends on: original/replication_archive/attrition_simulated_data.csv, helpers.R
# Description: Figure 17.8, extreme value bounds under attrition, with the imputed
#   observations kept visible.

source(here::here("maintained", "helpers.R"))

dat <- read_csv(
  here::here("original", "replication_archive", "attrition_simulated_data.csv"),
  show_col_types = FALSE
)

gg_df <- dat |>
  mutate(
    Condition = factor(Z, levels = 0:1, labels = c("Control", "Treatment")),
    `Lower Bound` = case_when(
      Z == 1 & is.na(Y) ~ 1,
      Z == 0 & is.na(Y) ~ 7,
      .default = Y
    ),
    `Upper Bound` = case_when(
      Z == 1 & is.na(Y) ~ 7,
      Z == 0 & is.na(Y) ~ 1,
      .default = Y
    ),
    Y_missing = if_else(is.na(Y), "Outcome imputed", "Outcome available")
  ) |>
  pivot_longer(
    cols = c(`Lower Bound`, `Upper Bound`),
    names_to = "bound",
    values_to = "value"
  )

summary_df <- gg_df |>
  group_by(Condition, bound) |>
  reframe(tidy(lm_robust(value ~ 1, data = pick(everything())))) |>
  mutate(value = estimate)

good <-
  ggplot(summary_df, aes(Condition, value)) +
  geom_point(
    data = gg_df,
    aes(color = Y_missing, shape = Y_missing),
    position = position_jitter(width = 0.2, height = 0.1, seed = 42),
    alpha = 0.3
  ) +
  geom_point(size = 3) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high)) +
  scale_y_continuous(
    "Outcome variable \n [1: Strongly Disagree, 7: Strongly Agree]",
    breaks = 1:7
  ) +
  scale_color_manual(values = c("#205C8A", "#C67800")) +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title.x = element_blank()
  ) +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  facet_wrap(~bound)

write_csv(
  summary_df |>
    mutate(n_missing_outcome = sum(is.na(dat$Y)), n_total = nrow(dat)),
  here::here("maintained", "output", "figure_8_attrition_estimates.csv")
)

ggsave(here::here("maintained", "output", "figure_8_attrition_good.pdf"),
       good, width = 4, height = 4)
ggsave(here::here("maintained", "output", "figure_8_attrition_good.png"),
       good, width = 4, height = 4, dpi = 300)
