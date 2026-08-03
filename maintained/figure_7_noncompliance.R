# coppock_2021/maintained/figure_7_noncompliance.R
# Output: output/figure_7_noncompliance_good.pdf/.png, output/figure_7_noncompliance_bad.pdf/.png,
#         output/figure_7_noncompliance_estimates.csv
# Depends on: original/replication_archive/noncompliance_simulated_data.csv, helpers.R
# Description: Figure 17.7, a trial with two-sided noncompliance drawn well (turnout
#   and treatment receipt by assigned group) and badly (turnout by realised receipt).

source(here::here("maintained", "helpers.R"))

dat <- read_csv(
  here::here("original", "replication_archive", "noncompliance_simulated_data.csv"),
  show_col_types = FALSE
)

gg_df <- dat |>
  pivot_longer(
    cols = c(`Treatment Receipt`, Turnout),
    names_to = "dv",
    values_to = "value"
  )

summary_good <- gg_df |>
  group_by(Z, dv) |>
  reframe(tidy(lm_robust(value ~ 1, data = pick(everything())))) |>
  mutate(value = estimate)

dat_bad <- dat |>
  mutate(`Treatment Receipt` = factor(
    `Treatment Receipt`,
    levels = c(0, 1),
    labels = c("Did not receive treatment", "Did receive treatment")
  ))

summary_bad <- dat_bad |>
  group_by(Z, `Treatment Receipt`) |>
  reframe(tidy(lm_robust(Turnout ~ 1, data = pick(everything())))) |>
  mutate(Turnout = estimate)

good <-
  ggplot(summary_good, aes(Z, value)) +
  geom_point(
    data = gg_df,
    position = position_jitter(width = 0.2, height = 0.1, seed = 42),
    alpha = 0.1
  ) +
  geom_point(size = 3) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high)) +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 1, length.out = 5)) +
  coord_cartesian(ylim = c(-0.1, 1.1)) +
  theme(strip.background = element_blank()) +
  facet_wrap(~dv) +
  ylab("Outcome variable [1 = 'Yes', 0 = otherwise]") +
  xlab("Randomly assigned group")

bad <-
  ggplot(summary_bad, aes(Z, Turnout)) +
  geom_point(
    data = dat_bad,
    position = position_jitter(width = 0.2, height = 0.1, seed = 42),
    alpha = 0.1
  ) +
  geom_point(size = 3) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high)) +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 1, length.out = 5)) +
  coord_cartesian(ylim = c(-0.1, 1.1)) +
  theme(
    strip.background = element_blank(),
    legend.position = "bottom"
  ) +
  facet_wrap(~`Treatment Receipt`) +
  ylab("Turnout [1 = 'Yes', 0 = otherwise]") +
  xlab("Randomly assigned group")

write_csv(
  bind_rows(
    summary_good |> rename(group = dv) |> mutate(panel = "By assigned group"),
    summary_bad |> rename(group = `Treatment Receipt`) |> mutate(panel = "By realised receipt")
  ),
  here::here("maintained", "output", "figure_7_noncompliance_estimates.csv")
)

ggsave(here::here("maintained", "output", "figure_7_noncompliance_good.pdf"),
       good, width = 4, height = 4)
ggsave(here::here("maintained", "output", "figure_7_noncompliance_good.png"),
       good, width = 4, height = 4, dpi = 300)
ggsave(here::here("maintained", "output", "figure_7_noncompliance_bad.pdf"),
       bad, width = 4, height = 4)
ggsave(here::here("maintained", "output", "figure_7_noncompliance_bad.png"),
       bad, width = 4, height = 4, dpi = 300)
