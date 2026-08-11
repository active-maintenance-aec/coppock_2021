# coppock_2021/ground_truth/build_ground_truth.R
# Output: ground_truth/coppock_2021_ground_truth.csv
# Depends on: maintained/output/ (run run_all.R first), ground_truth/published_claims.csv,
#   maintained/in_text_claims.R
# Description: Assemble the ground truth table. Every value_paper entry was read off
#   the published chapter and is used only as a comparison target; no published value
#   is an input to any computation here or anywhere in maintained/. Every value_script
#   and value_rewrite entry is read back out of maintained/output/, so the table cannot
#   drift from the pipeline.
#
#   The chapter prints no tables and states no estimates, so most of its checkable
#   content is qualitative: a direction, an ordering, a range, an axis label. A claim
#   about a value carries its verdict in `match`; a claim about shape, sign or
#   ordering has no value to compare and carries its verdict in `holds`. Both are
#   computed here from explicit predicates, never typed.
#
#   value_script and value_rewrite are the same number throughout, and that is a
#   finding rather than a shortcut: text_archive_agreement.R re-derives every estimate
#   behind the eight figures from the deposited data using the deposited model
#   specifications, and the largest disagreement with the rewrite is asserted below.
#
#   The last section is the coverage gate. It runs maintained/in_text_claims.R as a
#   program, counts the claims it prints, and requires that set to equal the set of
#   extraction rows that need a block. Where both instruments arrive at a bare number
#   it compares them at the precision the extraction records for that claim.

library(here)
library(tidyverse)

here::i_am("ground_truth/build_ground_truth.R")

out <- function(f) read_csv(here::here("maintained", "output", f), show_col_types = FALSE)

published_claims <- read_csv(
  here::here("ground_truth", "published_claims.csv"),
  col_types = cols(value_paper = col_character(), .default = col_guess())
)

# Every join below goes through this, so a transcription that finds nothing, or
# finds too much, stops the build instead of producing a silently short table.
join_1to1 <- function(x, y, by) {
  stopifnot(!any(duplicated(x[[by]])), !any(duplicated(y[[by]])))
  joined <- inner_join(x, y, by = by)
  stopifnot(nrow(joined) == nrow(x), nrow(joined) == nrow(y))
  joined
}

design <- out("text_design_parameters.csv")
descriptive <- out("text_descriptive_claims.csv")
two_arm <- out("figure_1_two_arm_estimates.csv")
blocked_ates <- out("figure_2_blocked_ates.csv")
block_ates <- out("figure_3_blocked_block_ates.csv")
facet_estimates <- out("figure_3_blocked_facets_estimates.csv")
clustered <- out("figure_4_clustered_estimates.csv")
clustered_ates <- out("figure_4_clustered_ates.csv")
clustered_labels <- out("figure_4_clustered_axis_labels.csv")
covariate <- out("figure_5_covariate_adjustment_estimators.csv")
covariate_ranges <- out("figure_5_covariate_adjustment_panel_ranges.csv")
cates <- out("figure_6_interaction_cates.csv")
residuals_binned <- out("figure_6_interaction_binned_residuals.csv")
noncompliance <- out("figure_7_noncompliance_estimates.csv")
attrition <- out("figure_8_attrition_estimates.csv")
stata <- out("text_stata_equivalent.csv")
agreement <- out("text_archive_agreement.csv")

# The rewrite is a port of the deposited estimators, not a reanalysis, so the
# archive and rewrite columns below are the same number. That holds only because
# every estimate agrees; if it ever stops holding, this stops the build.
stopifnot(max(agreement$max_abs_difference) < 1e-10)

param <- function(fig, q) design$value[design$figure == fig & design$quantity == q]
target <- function(fig, q) descriptive$value[descriptive$figure == fig & descriptive$quantity == q]

# Values used more than once ----
ate_weighted <- blocked_ates$estimate[str_detect(blocked_ates$analysis, "Inverse")]
ate_unweighted <- blocked_ates$estimate[str_detect(blocked_ates$analysis, "Unweighted")]
p_unweighted <- blocked_ates$p.value[str_detect(blocked_ates$analysis, "Unweighted")]

# The chapter's contrast is between an analysis that finds an effect and one that
# does not, so "large and negative" and "close to zero" are read as whether each
# interval excludes zero rather than against a threshold chosen here.
weighted_excludes_zero <- blocked_ates$conf.high[str_detect(blocked_ates$analysis, "Inverse")] < 0
unweighted_covers_zero <-
  blocked_ates$conf.low[str_detect(blocked_ates$analysis, "Unweighted")] < 0 &
  blocked_ates$conf.high[str_detect(blocked_ates$analysis, "Unweighted")] > 0

ate_block_1 <- block_ates$estimate[block_ates$neighborhood == 1]
ate_block_2 <- block_ates$estimate[block_ates$neighborhood == 2]
prob_block_1 <- block_ates$prob_treated[block_ates$neighborhood == 1]
prob_block_2 <- block_ates$prob_treated[block_ates$neighborhood == 2]

# Figure 17.3b compares whatever the deposited script puts on its horizontal axis.
compared_units <- facet_estimates |>
  filter(panel == "Faceted by condition") |>
  distinct(block) |>
  arrange(block, .locale = "en") |>
  pull(block)

clustered_wide <- clustered |>
  select(condition, panel, estimate, conf.low, conf.high) |>
  pivot_wider(names_from = panel, values_from = c(estimate, conf.low, conf.high))

width_clustered <- clustered_wide$`conf.high_Cluster robust` - clustered_wide$`conf.low_Cluster robust`
width_unclustered <- clustered_wide$`conf.high_Student level, no clustering` -
  clustered_wide$`conf.low_Student level, no clustering`

lin_estimate <- covariate$estimate[covariate$estimator == "lm_lin"]
interacted_estimate <- covariate$estimate[covariate$estimator == "Interacted, centred covariate"]
resres_estimate <- covariate$estimate[covariate$estimator == "Residual on residual"]

cate_low <- cates$AME[cates$X == -2]
cate_high <- cates$AME[cates$X == 2]

receipt_treated <- noncompliance$estimate[
  noncompliance$panel == "By assigned group" &
    noncompliance$group == "Treatment Receipt" & noncompliance$Z == "Treatment"
]
receipt_control <- noncompliance$estimate[
  noncompliance$panel == "By assigned group" &
    noncompliance$group == "Treatment Receipt" & noncompliance$Z == "Control"
]

bound_mean <- function(cond, b) {
  attrition$estimate[attrition$Condition == cond & attrition$bound == b]
}
lower_bound_ate <- bound_mean("Treatment", "Lower Bound") - bound_mean("Control", "Lower Bound")
upper_bound_ate <- bound_mean("Treatment", "Upper Bound") - bound_mean("Control", "Upper Bound")

stata_width <- stata |>
  mutate(width = conf.high - conf.low) |>
  select(condition, method, width) |>
  pivot_wider(names_from = method, values_from = width)
stata_interval_gap <- max(abs(stata_width[[2]] - stata_width[[3]]))

# cut() orders its levels from low to high and group_by keeps that order, so the
# lowest occupied covariate bin is the first row.
lowest_bin_residual <- residuals_binned$mean_residual[1]

fmt <- function(x, digits = 2) formatC(x, format = "f", digits = digits)

# The rows ----
# `match` and `holds` are predicates evaluated here, never typed verdicts. A claim
# about a value is matched at the precision the chapter prints it to and carries a
# `match`; a claim about shape, sign or ordering carries a `holds` and no `match`.
gt <- tribble(
  ~claim_id, ~table_figure, ~claim, ~value_script, ~value_paper, ~match, ~holds, ~defect_locus, ~notes,

  "fig1_n", "Figure 17.1", "N", param("Figure 17.1", "N"), "500",
  as.numeric(param("Figure 17.1", "N") == "500"), NA_real_, "",
  "Chapter: \"imagine a 500-person experiment\"",

  "fig1_n_treated", "Figure 17.1", "N treated", param("Figure 17.1", "N treated"), "100",
  as.numeric(param("Figure 17.1", "N treated") == "100"), NA_real_, "",
  "Chapter: \"exactly 100 units are treated\"",

  NA_character_, "Figure 17.1", "Control group mean",
  fmt(two_arm$estimate[two_arm$condition == "Control"], 3),
  NA_character_, NA_real_, NA_real_, "",
  "The chapter prints no group means; the figure shows the two points without labelling them",

  NA_character_, "Figure 17.1", "Treatment group mean",
  fmt(two_arm$estimate[two_arm$condition == "Treatment"], 3),
  NA_character_, NA_real_, NA_real_, "",
  "As above",

  NA_character_, "Figure 17.1", "The deposited Stata file draws something similar to the R figure",
  paste0("intervals differ in width by at most ", fmt(stata_interval_gap, 4)),
  NA_character_, NA_real_, NA_real_, "",
  paste0("The deposit's README says its .do file \"produces something similar to Figure 1\", ",
         "which the chapter itself does not claim. The .do file builds a normal-approximation ",
         "interval from the group standard deviation where the R script builds an HC2 interval ",
         "on the t distribution. The two group means are identical and the interval widths ",
         "differ by at most ", fmt(stata_interval_gap, 4), " on a 0 to 1 outcome. It also reads ",
         "data/two_arm_simulated_data.csv, a directory the deposit does not contain"),

  "blocked_n_neighborhoods", "Figures 17.2 and 17.3", "Number of neighborhoods",
  param("Figures 17.2 and 17.3", "Number of neighborhoods"), "2",
  as.numeric(param("Figures 17.2 and 17.3", "Number of neighborhoods") == "2"), NA_real_, "",
  "Chapter: \"an experiment conducted in two neighborhoods\"",

  "blocked_n1", "Figures 17.2 and 17.3", "N in neighborhood 1",
  param("Figures 17.2 and 17.3", "N in neighborhood 1"), "50",
  as.numeric(param("Figures 17.2 and 17.3", "N in neighborhood 1") == "50"), NA_real_, "",
  "Chapter: \"The first neighborhood has 50 residents\"",

  "blocked_n2", "Figures 17.2 and 17.3", "N in neighborhood 2",
  param("Figures 17.2 and 17.3", "N in neighborhood 2"), "100",
  as.numeric(param("Figures 17.2 and 17.3", "N in neighborhood 2") == "100"), NA_real_, "",
  "Chapter: \"and the second has 100\"",

  "blocked_n_treated", "Figures 17.2 and 17.3", "N treated per neighborhood",
  param("Figures 17.2 and 17.3", "N treated per neighborhood"), "25",
  as.numeric(param("Figures 17.2 and 17.3", "N treated per neighborhood") == "25"), NA_real_, "",
  "Chapter: \"needs to treat exactly 25 residents in each neighborhood\"",

  NA_character_, "Figures 17.2 and 17.3", "N total", param("Figures 17.2 and 17.3", "N"),
  NA_character_, NA_real_, NA_real_, "",
  "The chapter states the two block sizes but not their sum",

  "blocked_prob_higher_first", "Figure 17.2",
  "Probability of treatment is higher in the first neighborhood",
  paste0(fmt(prob_block_1), " against ", fmt(prob_block_2)), "higher in the first",
  NA_real_, as.numeric(prob_block_1 > prob_block_2), "",
  "Chapter: \"The probability of treatment is higher in the first neighborhood than the second\"",

  "blocked_weighted_ate", "Figure 17.2",
  "Inverse probability weighted ATE is large and negative",
  fmt(ate_weighted, 3), "large and negative",
  NA_real_, as.numeric(weighted_excludes_zero), "",
  "Chapter: \"the correct analysis shows that the ATE estimate is large and negative\"",

  "blocked_unweighted_ate", "Figure 17.2", "Unweighted ATE is close to zero",
  fmt(ate_unweighted, 3), "close to zero",
  NA_real_, as.numeric(unweighted_covers_zero), "",
  paste0("Chapter: \"the incorrect analysis estimates the ATE to be close to zero\". Attenuated ",
         "by ", round(100 * (1 - ate_unweighted / ate_weighted)), " per cent and no longer ",
         "distinguishable from zero (p = ", fmt(p_unweighted, 2), ") rather than literally zero"),

  "fig3_block_effects", "Figure 17.3",
  "Small effect in neighborhood 1 and a large negative effect in neighborhood 2",
  paste0("neighborhood 1 = ", fmt(ate_block_1), "; neighborhood 2 = ", fmt(ate_block_2)),
  "small in 1 and large negative in 2",
  NA_real_, as.numeric(abs(ate_block_1) < abs(ate_block_2)), "paper_internal",
  paste0("The two neighborhoods are transposed in the sentence. The large negative effect is in ",
         "neighborhood 1 (", fmt(ate_block_1), ") and the small one is in neighborhood 2 (",
         fmt(ate_block_2), "). The published figure is right and its facets are labelled ",
         "correctly, and the deposited generator writes the effect as -4 * Z * (neighborhood == 1)"),

  "fig3b_across_schools", "Figure 17.3",
  "Panel b compares across schools",
  paste(compared_units, collapse = ", "), "schools",
  NA_real_, as.numeric(any(str_detect(str_to_lower(compared_units), "school"))), "paper_internal",
  paste0("Chapter: \"in Figure 17.3b, we facet by randomly assigned group, so we compare across ",
         "schools and within treatment group\". The groups compared are the ",
         length(compared_units), " neighborhoods of the blocked design, which the published ",
         "panel labels in full. No school or class variable exists outside the Figure 17.4 ",
         "dataset, and the chapter's classroom example is Figure 17.4"),

  "fig3b_axis_labels", "Figure 17.3", "Panel b horizontal axis labels",
  paste(compared_units, collapse = ", "), "Neighborhood 1",
  as.numeric(compared_units[1] == "Neighborhood 1"), NA_real_,
  if (compared_units[1] == "Neighborhood 1") "" else "archive",
  paste0("The deposited script abbreviated the axis labels of panel (b) to \"N/hood 1\" ",
         "while writing panel (a)'s in full, so running it did not produce the published ",
         "labels; the rewrite now uses one label for both panels and reproduces them. ",
         "Nothing published was ever wrong here: the deposit was what fell short, as it did ",
         "for the Figure 17.4 panel (b) label"),

  NA_character_, "Figure 17.4", "Number of classes", param("Figure 17.4", "Number of classes"),
  NA_character_, NA_real_, NA_real_, "",
  "The chapter describes a classroom-level trial without giving the number of classrooms",

  NA_character_, "Figure 17.4", "N students", param("Figure 17.4", "N students"),
  NA_character_, NA_real_, NA_real_, "", "Not stated in the chapter",

  "clustered_outcome_scale", "Figure 17.4", "Outcome measured on a 400 to 1600 point scale",
  param("Figure 17.4", "Outcome range"), "400 to 1600",
  as.numeric(param("Figure 17.4", "Outcome range") == "400 to 1600"), NA_real_, "paper_internal",
  paste0("Chapter: \"the outcome is measured on a 400-1600-point scale\". The simulated outcomes ",
         "run ", param("Figure 17.4", "Outcome range"), ", and ",
         target("Figure 17.4", "Students with an outcome outside the stated 400 to 1600 scale"),
         " of ", target("Figure 17.4", "Students in total"), " students fall outside the stated ",
         "scale. Both panels clip at 400 and 1600, so the published figure does not show them"),

  "fig4_means_same", "Figure 17.4", "Group means are the same in both panels",
  paste0(fmt(clustered_wide$`estimate_Cluster robust`[1], 1), " and ",
         fmt(clustered_wide$`estimate_Cluster robust`[2], 1), " in both"),
  "the same in both panels",
  NA_real_,
  as.numeric(max(abs(clustered_wide$`estimate_Cluster robust` -
                       clustered_wide$`estimate_Student level, no clustering`)) < 1e-10), "",
  paste0("Chapter: \"While the group means are the same in both panels, the 95% confidence ",
         "intervals in Figure 17.4b were constructed ignoring the clustering\""),

  "fig4_ci_ignore_clustering", "Figure 17.4",
  "Panel b confidence intervals ignore clustering and are narrower",
  paste0("widths ", fmt(width_unclustered[1], 0), " and ", fmt(width_unclustered[2], 0),
         " against ", fmt(width_clustered[1], 0), " and ", fmt(width_clustered[2], 0)),
  "narrower", NA_real_, as.numeric(all(width_unclustered < width_clustered)), "",
  "Same sentence as the row above; the unclustered intervals are about a third as wide",

  "fig4b_axis_label", "Figure 17.4", "Panel b vertical axis label",
  clustered_labels$y_label[clustered_labels$panel == "(b) Individuals"],
  "Outcome variable: SAT score",
  as.numeric(str_to_lower(clustered_labels$y_label[clustered_labels$panel == "(b) Individuals"]) ==
               str_to_lower("Outcome variable: SAT score")),
  NA_real_,
  if (str_to_lower(clustered_labels$y_label[clustered_labels$panel == "(b) Individuals"]) ==
      str_to_lower("Outcome variable: SAT score")) "" else "archive",
  paste0("The deposited script gave both panels the class-mean wording. The book restyled every ",
         "axis label to sentence case, and in panel (b) the restyling also dropped \"classroom ",
         "average\", which is a correction rather than a house style change, since panel (b) ",
         "plots students. Running the deposited script does not produce the published label"),

  "clustered_either_or", "Figure 17.4",
  "Clustering can be handled by clustered standard errors or by weighting class means",
  paste0("both give ", fmt(clustered_ates$estimate[1], 3)),
  "either approach",
  NA_real_, as.numeric(abs(diff(clustered_ates$estimate)) < 1e-10), "",
  paste0("Chapter: \"either by using clustered standard errors or by aggregating outcomes to the ",
         "cluster-level and weighting the difference-in-means estimator by cluster size\". The two ",
         "point estimates agree to ", signif(abs(diff(clustered_ates$estimate)), 2),
         " and the standard errors to ", signif(abs(diff(clustered_ates$std.error)), 2),
         "; only the degrees of freedom differ"),

  NA_character_, "Figure 17.5", "N", param("Figure 17.5", "N"),
  NA_character_, NA_real_, NA_real_, "",
  "Not stated in the chapter",

  "fig5_scale_not_range", "Figure 17.5",
  "The vertical scale of both facets is the same but their range is not",
  paste0(fmt(covariate_ranges$y_span[1], 0), " units in each facet; ",
         fmt(covariate_ranges$y_min[covariate_ranges$estimation == "Unadjusted"], 0), " to ",
         fmt(covariate_ranges$y_max[covariate_ranges$estimation == "Unadjusted"], 0), " against ",
         fmt(covariate_ranges$y_min[covariate_ranges$estimation == "Adjusted"], 0), " to ",
         fmt(covariate_ranges$y_max[covariate_ranges$estimation == "Adjusted"], 0)),
  "the same scale but not the same range",
  NA_real_,
  as.numeric(n_distinct(covariate_ranges$y_span) == 1 &
               n_distinct(covariate_ranges$y_min) > 1), "",
  "Chapter: \"the vertical scale of both facets (but not their range) is set to be the same\"",

  "fig5_lin_separately", "Figure 17.5",
  "lm_lin is equivalent to interacting the covariate with the treatment indicator",
  fmt(lin_estimate, 4), "equivalent",
  NA_real_, as.numeric(abs(lin_estimate - interacted_estimate) < 1e-10), "",
  paste0("Chapter: \"I use the procedure recommended in Lin (2013) to adjust each treatment arm ",
         "separately, equivalent to interacting the covariates with the treatment indicator and ",
         "then predicting the average outcome in each treatment condition\". The two estimates ",
         "agree to ", signif(abs(lin_estimate - interacted_estimate), 2)),

  "resres_equals_mr", "Figure 17.5",
  "The residual on residual slope equals the multiple regression estimate",
  fmt(resres_estimate, 4), "exactly equal",
  NA_real_, as.numeric(abs(lin_estimate - resres_estimate) < 1e-10), "",
  paste0("Chapter: the best linear summary of the residuals \"will be exactly the estimate ",
         "obtained from the multiple regression\". The two agree to ",
         signif(abs(lin_estimate - resres_estimate), 2), ". The deposited script asserts in a ",
         "comment that the standard errors are \"v. slightly off\" while the point estimates are ",
         "fine, and both halves hold: the standard errors are ",
         fmt(covariate$std.error[covariate$estimator == "lm_lin"], 5), " against ",
         fmt(covariate$std.error[covariate$estimator == "Residual on residual"], 5)),

  "fig5_axis_label_likert", "Figure 17.5",
  "Vertical axis label describes the raw scale as a seven point Likert",
  paste0(param("Figure 17.5", "Number of distinct outcome values"), " distinct values over ",
         param("Figure 17.5", "Outcome range")),
  "7-point Likert",
  as.numeric(param("Figure 17.5", "Number of distinct outcome values") == "7"), NA_real_,
  "paper_internal",
  paste0("The vertical axis of Figure 17.5 reads \"Outcome variable (raw scale is 7-point ",
         "Likert)\". The deposited covariate outcome takes ",
         param("Figure 17.5", "Number of distinct outcome values"), " distinct values across ",
         param("Figure 17.5", "N"), " units, running ", param("Figure 17.5", "Outcome range"),
         ", and ",
         target("Figure 17.5", "Units with an outcome outside the 1 to 7 range the axis label implies"),
         " of them fall outside the 1 to 7 range such a scale allows. The generator draws ",
         "Y ~ 0.5 * Z + 1.0 * X + 0.5 * Z * X + U with X normal, so nothing about it is a ",
         "Likert item. The deposited script writes this label, so the chapter printed what its ",
         "own code gave it; the chapter's only Likert outcome is Figure 17.8"),

  NA_character_, "Figure 17.6", "N", param("Figure 17.6", "N"),
  NA_character_, NA_real_, NA_real_, "",
  "Not stated in the chapter; the design samples 2,500 units with probability pnorm(X)",

  "fig6_cate_signs", "Figure 17.6",
  "Negative effects at low covariate values and positive effects at high ones",
  paste0(fmt(cate_low), " at X = -2 and ", fmt(cate_high), " at X = 2"),
  "negative at low and positive at high",
  NA_real_, as.numeric(cate_low < 0 & cate_high > 0), "",
  paste0("Chapter: \"The model estimates negative effects for low values of the covariate and ",
         "positive effects for high values of the covariate\""),

  "fig6_fit_low", "Figure 17.6", "The linear model fits worst at low covariate values",
  paste0("mean residual ", fmt(lowest_bin_residual), " in the lowest bin"),
  "does not fit well, especially at low values",
  NA_real_, as.numeric(which.max(abs(residuals_binned$mean_residual)) == 1), "",
  paste0("Chapter: \"the statistical model does not fit the data particularly well, especially at ",
         "low values of the covariate\". Binning the covariate in half-unit steps, the largest ",
         "mean residual is in the lowest bin. The fit is also poor at the top of the range, where ",
         "the chapter does not say it is"),

  NA_character_, "Figure 17.7", "N", param("Figure 17.7", "N"),
  NA_character_, NA_real_, NA_real_, "", "Not stated in the chapter",

  NA_character_, "Figure 17.7", "N per assigned arm",
  param("Figure 17.7", "N assigned to treatment"),
  NA_character_, NA_real_, NA_real_, "", "Not stated in the chapter",

  "fig7_two_sided", "Figure 17.7", "Noncompliance is two-sided",
  paste0("treatment arm ", fmt(receipt_treated, 3), " and control arm ", fmt(receipt_control, 3)),
  "two-sided", NA_real_, as.numeric(receipt_control > 0 & receipt_treated < 1), "",
  paste0("Caption: \"Simulated experiment encountering two-sided noncompliance\". Some control ",
         "units take the treatment and some assigned units do not, so the label holds"),

  NA_character_, "Figure 17.8", "N", param("Figure 17.8", "N"),
  NA_character_, NA_real_, NA_real_, "", "Not stated in the chapter",

  NA_character_, "Figure 17.8", "N missing an outcome", param("Figure 17.8", "N missing an outcome"),
  NA_character_, NA_real_, NA_real_, "", "Not stated in the chapter",

  "attrition_likert_scale", "Figure 17.8", "Outcome on a seven point Likert scale",
  param("Figure 17.8", "Outcome range"), "seven point Likert",
  as.numeric(target("Figure 17.8", "Observed outcomes outside the stated 1 to 7 Likert scale") == "0"),
  NA_real_, "",
  paste0("Chapter: \"the outcome variable (measured on a seven-point Likert scale)\". The realised ",
         "draws use ",
         target("Figure 17.8", "Distinct Likert categories the draws use, of the stated seven"),
         " of the seven categories, running ", param("Figure 17.8", "Outcome range"),
         ", and none falls outside the stated scale"),

  "fig8_lower_bound", "Figure 17.8",
  "Lower bound group means are very similar and the worst case effect is near zero",
  paste0(fmt(bound_mean("Control", "Lower Bound")), " and ",
         fmt(bound_mean("Treatment", "Lower Bound")), ", a difference of ",
         fmt(lower_bound_ate)),
  "very similar and close to zero", NA_real_, as.numeric(abs(lower_bound_ate) < 0.05), "",
  paste0("Chapter: \"the resulting group means (shown as large black points) are very similar in ",
         "treatment and control, so the implied worst-case treatment effect estimate is very close ",
         "to zero\". They are in fact equal to two decimal places"),

  "fig8_upper_bound", "Figure 17.8", "Upper bound effect is close to a full scale point",
  fmt(upper_bound_ate), "close to a full-scale point",
  NA_real_, as.numeric(abs(upper_bound_ate - 1) < 0.25), "",
  paste0("Chapter: \"The upper-bound treatment effect estimate is close to a full-scale point\". ",
         "The upper bound is ", fmt(upper_bound_ate), ", a little above the full point the ",
         "chapter brackets it at, and the sentence hedges with \"close to\""),

  "fig8_bounds_range", "Figure 17.8", "The ATE lies somewhere between zero and one",
  paste0(fmt(lower_bound_ate), " to ", fmt(upper_bound_ate)), "zero and one",
  NA_real_, NA_real_, "",
  paste0("Chapter: \"the ATE is likely to lie somewhere between zero and one\". The bounds are ",
         fmt(lower_bound_ate), " to ", fmt(upper_bound_ate), ", so the upper end sits ",
         fmt(upper_bound_ate - 1), " above the point the sentence names. No verdict is recorded: ",
         "the sentence hedges with \"likely\", and the chapter says in the same paragraph that ",
         "the bounds are themselves estimates subject to sampling variability")
)

# The rewrite and the archive agree on every estimate, asserted above, so the two
# columns carry the same value and the two match verdicts are the same verdict.
# `holds` is a single verdict for the same reason: the descriptive claims are about
# numbers the archive and the rewrite share.
gt <- gt |>
  mutate(
    paper_id = "coppock_2021",
    value_rewrite = value_script,
    match_rewrite = match
  ) |>
  select(paper_id, claim_id, table_figure, claim, value_script, value_paper, match,
         value_rewrite, match_rewrite, holds, defect_locus, notes)

# The locus rule, three states. An adverse row must name where the fault lies; a
# clean match must not, since a locus there would read as a defect; a row with no
# verdict may, and none here does.
adverse <- (!is.na(gt$match) & gt$match == 0) |
  (!is.na(gt$match_rewrite) & gt$match_rewrite == 0) |
  (!is.na(gt$holds) & gt$holds == 0)
clean <- (!is.na(gt$match) & gt$match == 1) | (!is.na(gt$holds) & gt$holds == 1)
stopifnot(
  all(gt$defect_locus[adverse] != ""),
  all(gt$defect_locus[clean] == ""),
  !any(adverse & clean),
  # A row carries at most one kind of verdict.
  !any(!is.na(gt$match) & !is.na(gt$holds)),
  !any(duplicated(gt$claim_id[!is.na(gt$claim_id)]))
)

write_csv(gt, here::here("ground_truth", "coppock_2021_ground_truth.csv"))

# The errata spine's claim_ids ----
# errata_entries.csv names, for every published entry, the ground-truth claims it corrects.
# Every one of those ids has to exist here: a missing one is a typo or a claim that has since
# been renamed, and a dangling reference inside a document whose whole purpose is correcting
# the record is worse than a failed build.
errata_spine <- here::here("errata_entries.csv")
if (file.exists(errata_spine)) {
  cited_ids <- read_csv(errata_spine, show_col_types = FALSE)$claim_ids |>
    str_split(";") |>
    unlist() |>
    str_trim() |>
    discard(\(x) is.na(x) | x == "")
  dangling <- setdiff(cited_ids, gt$claim_id)
  if (length(dangling) > 0) print(dangling)
  stopifnot(length(dangling) == 0)
}

print(gt |> select(claim_id, table_figure, claim, value_script, value_paper, match, holds),
      n = nrow(gt), width = 200)
print(str_glue("rows: {nrow(gt)}  match=1: {sum(gt$match == 1, na.rm = TRUE)}  ",
               "match=0: {sum(gt$match == 0, na.rm = TRUE)}  ",
               "holds=1: {sum(gt$holds == 1, na.rm = TRUE)}  ",
               "holds=0: {sum(gt$holds == 0, na.rm = TRUE)}  ",
               "no verdict: {sum(is.na(gt$match) & is.na(gt$holds))}"))

# Coverage gate ----
# in_text_claims.R is read as a program, not as text: a block that errors at runtime
# or prints nothing satisfies a textual check completely. It goes into its own
# environment because the two files necessarily name the same outputs, and a bare
# source() would replace this script's `out`, `param` and `published_claims` with the
# claims file's before the assertions below ever ran.
claims_stdout <- capture.output(
  source(here::here("maintained", "in_text_claims.R"), local = new.env())
)

printed <- str_match(claims_stdout, "^CLAIM (\\S+) = (.*) \\|\\| (.*)$")
printed <- tibble(claim_id = printed[, 2], value = printed[, 3]) |>
  filter(!is.na(claim_id))

required <- published_claims |> filter(needs_block)

stopifnot(
  nrow(printed) == nrow(required),
  !any(duplicated(printed$claim_id)),
  setequal(printed$claim_id, required$claim_id)
)

# The two hand transcriptions of the same pages, compared. Backfilling an extraction
# onto a finished ground truth otherwise leaves them free to drift apart.
reconciled <- join_1to1(
  gt |> filter(!is.na(claim_id)) |> select(claim_id, gt_paper = value_paper),
  published_claims |> filter(claim_id %in% gt$claim_id) |> select(claim_id, extraction_paper = value_paper),
  "claim_id"
)
stopifnot(identical(reconciled$gt_paper, reconciled$extraction_paper))

# Where both instruments land on a bare number, they must give the same digits at the
# precision the extraction records for that claim. This is the part of the second
# instrument that pays: the two arrive by different routes through the same outputs.
cross_checked <- printed |>
  inner_join(gt |> select(claim_id, value_rewrite), by = "claim_id") |>
  inner_join(published_claims |> select(claim_id, digits), by = "claim_id") |>
  filter(
    !is.na(digits),
    str_detect(value, "^-?[0-9]+(\\.[0-9]+)?$"),
    str_detect(value_rewrite, "^-?[0-9]+(\\.[0-9]+)?$")
  ) |>
  mutate(
    within_tolerance =
      abs(as.numeric(value) - as.numeric(value_rewrite)) < 10^(-digits) / 2 + 1e-9,
    same_digits = map2_chr(as.numeric(value_rewrite), digits,
                           \(x, d) formatC(x, format = "f", digits = d)) == value
  )

stopifnot(nrow(cross_checked) > 0, all(cross_checked$within_tolerance), all(cross_checked$same_digits))

print(str_glue("coverage gate: {nrow(printed)} claims printed against ",
               "{nrow(required)} extraction rows needing a block; ",
               "{nrow(reconciled)} published values reconciled across the two transcriptions; ",
               "{nrow(cross_checked)} values compared numerically across the two instruments"))
