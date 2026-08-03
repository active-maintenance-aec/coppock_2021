# coppock_2021/maintained/figure_2_blocked_experiment.R
# Output: output/figure_2_blocked_good.pdf/.png, output/figure_2_blocked_bad.pdf/.png,
#         output/figure_3_blocked_facets_good.pdf/.png,
#         output/figure_3_blocked_facets_bad.pdf/.png,
#         output/figure_2_blocked_estimates.csv, output/figure_3_blocked_facets_estimates.csv,
#         output/figure_2_blocked_ates.csv, output/figure_3_blocked_block_ates.csv
# Depends on: original/replication_archive/blocked_simulated_data.csv, helpers.R
# Description: Figures 17.2 and 17.3, a blocked experiment drawn well and drawn badly.
#   The two figures share one dataset and one set of estimates, so they are produced
#   together.

source(here::here("maintained", "helpers.R"))

dat <- read_csv(
  here::here("original", "replication_archive", "blocked_simulated_data.csv"),
  show_col_types = FALSE
) |>
  mutate(
    neighborhood_lab = paste0("Neighborhood ", neighborhood),
    neighborhood_lab2 = paste0("N/hood ", neighborhood)
  )

# Figure 17.2, weighted against unweighted group means ----
summary_good <- dat |>
  group_by(condition) |>
  reframe(tidy(lm_robust(Y ~ 1, weights = 1 / Z_cond_prob, data = pick(everything())))) |>
  mutate(Y = estimate)

summary_bad <- dat |>
  group_by(condition) |>
  reframe(tidy(lm_robust(Y ~ 1, data = pick(everything())))) |>
  mutate(Y = estimate)

# The chapter's claim about this figure is about the difference between the two
# group means, which is the treatment effect each panel implies.
ates <- bind_rows(
  tidy(lm_robust(Y ~ Z, weights = 1 / Z_cond_prob, data = dat)) |>
    mutate(analysis = "Inverse probability weighted (Figure 17.2, left)"),
  tidy(lm_robust(Y ~ Z, data = dat)) |>
    mutate(analysis = "Unweighted (Figure 17.2, right)")
) |>
  filter(term == "Z")

# Figure 17.3a is read in the chapter as a statement about where the effect is, so
# the block-level effects it displays are written out alongside the group means.
block_ates <- dat |>
  group_by(neighborhood) |>
  reframe(tidy(lm_robust(Y ~ Z, data = pick(everything())))) |>
  filter(term == "Z")

assignment_probabilities <- dat |>
  group_by(neighborhood) |>
  summarise(prob_treated = mean(Z_cond_prob[Z == 1]), .groups = "drop")

good <-
  ggplot(dat, aes(condition, Y)) +
  geom_point(
    aes(size = 1 / Z_cond_prob),
    position = position_jitter(width = .25, height = .25, seed = 42),
    alpha = 0.1,
    stroke = 0
  ) +
  geom_point(data = summary_good, size = 4) +
  geom_linerange(data = summary_good, aes(ymin = conf.low, ymax = conf.high)) +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "none"
  ) +
  ylab("Outcome variable: count of some behavior")

bad <-
  ggplot(dat, aes(condition, Y)) +
  geom_point(
    position = position_jitter(width = .25, height = .1, seed = 42),
    alpha = 0.1,
    stroke = 0
  ) +
  geom_point(data = summary_bad, size = 4) +
  geom_linerange(data = summary_bad, aes(ymin = conf.low, ymax = conf.high)) +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "none"
  ) +
  ylab("Outcome variable: count of some behavior")

# Figure 17.3, faceting by block against faceting by condition ----
summary_by_block <- dat |>
  group_by(condition, neighborhood_lab) |>
  reframe(tidy(lm_robust(Y ~ 1, data = pick(everything())))) |>
  mutate(Y = estimate)

summary_by_cond <- dat |>
  group_by(condition, neighborhood_lab2) |>
  reframe(tidy(lm_robust(Y ~ 1, data = pick(everything())))) |>
  mutate(Y = estimate)

good_facets <-
  ggplot(dat, aes(condition, Y)) +
  geom_point(
    position = position_jitter(width = .25, height = .1, seed = 42),
    alpha = 0.1, stroke = 0
  ) +
  facet_wrap(~neighborhood_lab) +
  geom_point(data = summary_by_block, size = 4) +
  geom_linerange(data = summary_by_block, aes(ymin = conf.low, ymax = conf.high)) +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    axis.title.x = element_blank()
  ) +
  ylab("Outcome variable: count of some behavior")

bad_facets <-
  ggplot(dat, aes(neighborhood_lab2, Y)) +
  geom_point(
    position = position_jitter(width = .25, height = .25, seed = 42),
    alpha = 0.1, stroke = 0
  ) +
  facet_wrap(~condition) +
  geom_point(data = summary_by_cond, size = 4) +
  geom_linerange(data = summary_by_cond, aes(ymin = conf.low, ymax = conf.high)) +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    axis.title.x = element_blank()
  ) +
  ylab("Outcome variable: count of some behavior")

write_csv(
  bind_rows(
    summary_good |> mutate(panel = "Inverse probability weighted"),
    summary_bad |> mutate(panel = "Unweighted")
  ),
  here::here("maintained", "output", "figure_2_blocked_estimates.csv")
)

write_csv(ates, here::here("maintained", "output", "figure_2_blocked_ates.csv"))

write_csv(
  block_ates |> left_join(assignment_probabilities, by = "neighborhood"),
  here::here("maintained", "output", "figure_3_blocked_block_ates.csv")
)

write_csv(
  bind_rows(
    summary_by_block |> rename(block = neighborhood_lab) |> mutate(panel = "Faceted by block"),
    summary_by_cond |> rename(block = neighborhood_lab2) |> mutate(panel = "Faceted by condition")
  ),
  here::here("maintained", "output", "figure_3_blocked_facets_estimates.csv")
)

ggsave(here::here("maintained", "output", "figure_2_blocked_good.pdf"),
       good, width = 4, height = 4)
ggsave(here::here("maintained", "output", "figure_2_blocked_good.png"),
       good, width = 4, height = 4, dpi = 300)
ggsave(here::here("maintained", "output", "figure_2_blocked_bad.pdf"),
       bad, width = 4, height = 4)
ggsave(here::here("maintained", "output", "figure_2_blocked_bad.png"),
       bad, width = 4, height = 4, dpi = 300)
ggsave(here::here("maintained", "output", "figure_3_blocked_facets_good.pdf"),
       good_facets, width = 4, height = 4)
ggsave(here::here("maintained", "output", "figure_3_blocked_facets_good.png"),
       good_facets, width = 4, height = 4, dpi = 300)
ggsave(here::here("maintained", "output", "figure_3_blocked_facets_bad.pdf"),
       bad_facets, width = 4, height = 4)
ggsave(here::here("maintained", "output", "figure_3_blocked_facets_bad.png"),
       bad_facets, width = 4, height = 4, dpi = 300)
