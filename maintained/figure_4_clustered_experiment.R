# coppock_2021/maintained/figure_4_clustered_experiment.R
# Output: output/figure_4_clustered_good.pdf/.png, output/figure_4_clustered_bad.pdf/.png,
#         output/figure_4_clustered_estimates.csv, output/figure_4_clustered_ates.csv,
#         output/figure_4_clustered_axis_labels.csv
# Depends on: original/replication_archive/clustered_simulated_data.csv, helpers.R
# Description: Figure 17.4, a cluster-randomized experiment drawn well and drawn badly.

source(here::here("maintained", "helpers.R"))

dat <- read_csv(
  here::here("original", "replication_archive", "clustered_simulated_data.csv"),
  show_col_types = FALSE
)

class_level <- dat |>
  group_by(class, condition, n_per_class) |>
  summarise(Y = mean(Y), .groups = "drop")

summary_good <- dat |>
  group_by(condition) |>
  reframe(tidy(lm_robust(Y ~ 1, clusters = class, data = pick(everything())))) |>
  mutate(Y = estimate)

summary_bad <- dat |>
  group_by(condition) |>
  reframe(tidy(lm_robust(Y ~ 1, data = pick(everything())))) |>
  mutate(Y = estimate)

# The chapter's aside about this design is that a cluster-robust regression on the
# student-level data and a size-weighted regression on the class means agree.
ates <- bind_rows(
  tidy(lm_robust(Y ~ condition, clusters = class, data = dat)) |>
    mutate(analysis = "Student level, cluster robust"),
  tidy(lm_robust(Y ~ condition, weights = n_per_class, data = class_level)) |>
    mutate(analysis = "Class means, weighted by class size")
) |>
  filter(term == "conditionTreatment")

good <-
  ggplot(summary_good, aes(condition, Y)) +
  geom_point() +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high)) +
  geom_point(
    data = class_level,
    aes(size = n_per_class),
    position = position_jitter(width = 0.2, height = 0.1, seed = 42),
    alpha = 0.2, stroke = 0
  ) +
  coord_cartesian(ylim = c(400, 1600)) +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    legend.position = "none"
  ) +
  ylab("Outcome variable: Classroom Average SAT score")

bad <-
  ggplot(summary_bad, aes(condition, Y)) +
  geom_point() +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high)) +
  geom_point(
    data = dat,
    position = position_jitter(width = 0.2, height = 0.1, seed = 42),
    alpha = 0.2, stroke = 0
  ) +
  coord_cartesian(ylim = c(400, 1600)) +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    legend.position = "none"
  ) +
  ylab("Outcome variable: SAT score")

write_csv(
  bind_rows(
    summary_good |> mutate(panel = "Cluster robust"),
    summary_bad |> mutate(panel = "Student level, no clustering")
  ),
  here::here("maintained", "output", "figure_4_clustered_estimates.csv")
)

write_csv(ates, here::here("maintained", "output", "figure_4_clustered_ates.csv"))

# The chapter's two panels carry different vertical axis labels and the deposited
# script gives them the same one, so the labels are written out and compared like
# any other value rather than being read off an image.
write_csv(
  tibble(
    panel = c("(a) Cluster means", "(b) Individuals"),
    y_label = c(good$labels$y, bad$labels$y)
  ),
  here::here("maintained", "output", "figure_4_clustered_axis_labels.csv")
)

ggsave(here::here("maintained", "output", "figure_4_clustered_good.pdf"),
       good, width = 4, height = 4)
ggsave(here::here("maintained", "output", "figure_4_clustered_good.png"),
       good, width = 4, height = 4, dpi = 300)
ggsave(here::here("maintained", "output", "figure_4_clustered_bad.pdf"),
       bad, width = 4, height = 4)
ggsave(here::here("maintained", "output", "figure_4_clustered_bad.png"),
       bad, width = 4, height = 4, dpi = 300)
