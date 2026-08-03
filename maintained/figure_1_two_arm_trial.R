# coppock_2021/maintained/figure_1_two_arm_trial.R
# Output: output/figure_1_two_arm_good.pdf/.png, output/figure_1_two_arm_bad.pdf/.png,
#         output/figure_1_two_arm_estimates.csv
# Depends on: original/replication_archive/two_arm_simulated_data.csv, helpers.R
# Description: Figure 17.1, a simulated two-arm trial drawn well and drawn badly.

source(here::here("maintained", "helpers.R"))

dat <- read_csv(
  here::here("original", "replication_archive", "two_arm_simulated_data.csv"),
  show_col_types = FALSE
)

summary_df <- dat |>
  group_by(condition) |>
  reframe(tidy(lm_robust(Y ~ 1, data = pick(everything())))) |>
  mutate(Y = estimate)

good <-
  ggplot(summary_df, aes(condition, Y)) +
  geom_point(
    data = dat,
    position = position_jitter(width = 0.2, height = 0.1, seed = 42),
    alpha = 0.2
  ) +
  geom_point(size = 3) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high)) +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 1, length.out = 5)) +
  coord_cartesian(ylim = c(-0.1, 1.1)) +
  theme(axis.title.x = element_blank()) +
  ylab("Outcome variable [1 = 'Yes', 0 = otherwise]")

bad <-
  ggplot(summary_df, aes(condition, Y)) +
  geom_col() +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 1, length.out = 5)) +
  coord_cartesian(ylim = c(-0.1, 1.1)) +
  theme(axis.title.x = element_blank()) +
  ylab("Outcome variable [1 = 'Yes', 0 = otherwise]")

write_csv(summary_df,
          here::here("maintained", "output", "figure_1_two_arm_estimates.csv"))

ggsave(here::here("maintained", "output", "figure_1_two_arm_good.pdf"),
       good, width = 4, height = 4)
ggsave(here::here("maintained", "output", "figure_1_two_arm_good.png"),
       good, width = 4, height = 4, dpi = 300)
ggsave(here::here("maintained", "output", "figure_1_two_arm_bad.pdf"),
       bad, width = 4, height = 4)
ggsave(here::here("maintained", "output", "figure_1_two_arm_bad.png"),
       bad, width = 4, height = 4, dpi = 300)
