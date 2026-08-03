# coppock_2021/ground_truth/build_ground_truth.R
# Output: ground_truth/coppock_2021_ground_truth.csv
# Depends on: maintained/output/ (run run_all.R first)
# Description: Assemble the ground truth table. Every value_paper entry was read off
#   the published chapter and is used only as a comparison target; no published value
#   is an input to any computation here or anywhere in maintained/. Every value_script
#   and value_rewrite entry is read back out of maintained/output/, so the table cannot
#   drift from the pipeline.
#
#   The chapter prints no tables and states no estimates, so most of its checkable
#   content is qualitative: a direction, an ordering, a range, an axis label. Those
#   rows carry a predicate evaluated against the pipeline output rather than a typed
#   verdict, so `match` is computed in every row of this file.
#
#   value_script and value_rewrite are the same number throughout, and that is a
#   finding rather than a shortcut: text_archive_agreement.R re-derives every estimate
#   behind the eight figures from the deposited data using the deposited model
#   specifications, and the largest disagreement with the rewrite is asserted below.

library(here)
library(tidyverse)

here::i_am("ground_truth/build_ground_truth.R")

out <- function(f) read_csv(here::here("maintained", "output", f), show_col_types = FALSE)

design <- out("text_design_parameters.csv")
two_arm <- out("figure_1_two_arm_estimates.csv")
blocked_ates <- out("figure_2_blocked_ates.csv")
block_ates <- out("figure_3_blocked_block_ates.csv")
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

clustered_wide <- clustered |>
  select(condition, panel, estimate, conf.low, conf.high) |>
  pivot_wider(names_from = panel, values_from = c(estimate, conf.low, conf.high))

width_clustered <- clustered_wide$`conf.high_Cluster robust` - clustered_wide$`conf.low_Cluster robust`
width_unclustered <- clustered_wide$`conf.high_Student level, no clustering` -
  clustered_wide$`conf.low_Student level, no clustering`

lin_estimate <- covariate$estimate[covariate$estimator == "lm_lin"]
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
# `match` is a predicate evaluated here, never a typed verdict. A numeric claim is
# matched at the precision the chapter prints it to; a qualitative claim states the
# comparison it is making.
gt <- tribble(
  ~table_figure, ~claim, ~value_script, ~value_paper, ~match, ~defect_locus, ~notes,

  "Figure 17.1", "N", param("Figure 17.1", "N"), "500",
  as.numeric(param("Figure 17.1", "N") == "500"), "",
  "Chapter: \"imagine a 500-person experiment\"",

  "Figure 17.1", "N treated", param("Figure 17.1", "N treated"), "100",
  as.numeric(param("Figure 17.1", "N treated") == "100"), "",
  "Chapter: \"exactly 100 units are treated\"",

  "Figure 17.1", "Control group mean", fmt(two_arm$estimate[two_arm$condition == "Control"], 3),
  NA_character_, NA_real_, "",
  "The chapter prints no group means; the figure shows the two points without labelling them",

  "Figure 17.1", "Treatment group mean", fmt(two_arm$estimate[two_arm$condition == "Treatment"], 3),
  NA_character_, NA_real_, "",
  "As above",

  "Figure 17.1", "The deposited Stata file draws something similar to the R figure",
  paste0("intervals differ in width by at most ", fmt(stata_interval_gap, 4)),
  NA_character_, NA_real_, "",
  paste0("The deposit's README says its .do file \"produces something similar to Figure 1\", ",
         "which the chapter itself does not claim. The .do file builds a normal-approximation ",
         "interval from the group standard deviation where the R script builds an HC2 interval ",
         "on the t distribution. The two group means are identical and the interval widths ",
         "differ by at most ", fmt(stata_interval_gap, 4), " on a 0 to 1 outcome. It also reads ",
         "data/two_arm_simulated_data.csv, a directory the deposit does not contain"),

  "Figures 17.2 and 17.3", "Number of neighborhoods",
  param("Figures 17.2 and 17.3", "Number of neighborhoods"), "2",
  as.numeric(param("Figures 17.2 and 17.3", "Number of neighborhoods") == "2"), "",
  "Chapter: \"an experiment conducted in two neighborhoods\"",

  "Figures 17.2 and 17.3", "N in neighborhood 1",
  param("Figures 17.2 and 17.3", "N in neighborhood 1"), "50",
  as.numeric(param("Figures 17.2 and 17.3", "N in neighborhood 1") == "50"), "",
  "Chapter: \"The first neighborhood has 50 residents\"",

  "Figures 17.2 and 17.3", "N in neighborhood 2",
  param("Figures 17.2 and 17.3", "N in neighborhood 2"), "100",
  as.numeric(param("Figures 17.2 and 17.3", "N in neighborhood 2") == "100"), "",
  "Chapter: \"and the second has 100\"",

  "Figures 17.2 and 17.3", "N treated per neighborhood",
  param("Figures 17.2 and 17.3", "N treated per neighborhood"), "25",
  as.numeric(param("Figures 17.2 and 17.3", "N treated per neighborhood") == "25"), "",
  "Chapter: \"needs to treat exactly 25 residents in each neighborhood\"",

  "Figures 17.2 and 17.3", "N total", param("Figures 17.2 and 17.3", "N"),
  NA_character_, NA_real_, "",
  "The chapter states the two block sizes but not their sum",

  "Figure 17.2", "Probability of treatment is higher in the first neighborhood",
  paste0(fmt(prob_block_1), " against ", fmt(prob_block_2)), "higher in the first",
  as.numeric(prob_block_1 > prob_block_2), "",
  "Chapter: \"The probability of treatment is higher in the first neighborhood than the second\"",

  "Figure 17.2", "Inverse probability weighted ATE is large and negative",
  fmt(ate_weighted, 3), "large and negative",
  as.numeric(weighted_excludes_zero), "",
  "Chapter: \"the correct analysis shows that the ATE estimate is large and negative\"",

  "Figure 17.2", "Unweighted ATE is close to zero", fmt(ate_unweighted, 3), "close to zero",
  as.numeric(unweighted_covers_zero), "",
  paste0("Chapter: \"the incorrect analysis estimates the ATE to be close to zero\". Attenuated ",
         "by ", round(100 * (1 - ate_unweighted / ate_weighted)), " per cent and no longer ",
         "distinguishable from zero (p = ", fmt(p_unweighted, 2), ") rather than literally zero"),

  "Figure 17.3", "Small effect in neighborhood 1 and a large negative effect in neighborhood 2",
  paste0("neighborhood 1 = ", fmt(ate_block_1), "; neighborhood 2 = ", fmt(ate_block_2)),
  "small in 1 and large negative in 2",
  as.numeric(abs(ate_block_1) < abs(ate_block_2)), "paper_internal",
  paste0("The two neighborhoods are transposed in the sentence. The large negative effect is in ",
         "neighborhood 1 (", fmt(ate_block_1), ") and the small one is in neighborhood 2 (",
         fmt(ate_block_2), "). The published figure is right and its facets are labelled ",
         "correctly, and the deposited generator writes the effect as -4 * Z * (neighborhood == 1)"),

  "Figure 17.4", "Number of classes", param("Figure 17.4", "Number of classes"),
  NA_character_, NA_real_, "",
  "The chapter describes a classroom-level trial without giving the number of classrooms",

  "Figure 17.4", "N students", param("Figure 17.4", "N students"),
  NA_character_, NA_real_, "", "Not stated in the chapter",

  "Figure 17.4", "Outcome measured on a 400 to 1600 point scale",
  param("Figure 17.4", "Outcome range"), "400 to 1600",
  as.numeric(param("Figure 17.4", "Outcome range") == "400 to 1600"), "paper_internal",
  paste0("Chapter: \"the outcome is measured on a 400-1600-point scale\". The simulated outcomes ",
         "run ", param("Figure 17.4", "Outcome range"), ", and both panels clip at 400 and 1600, ",
         "so the published figure does not show the eight students who fall outside"),

  "Figure 17.4", "Group means are the same in both panels",
  paste0(fmt(clustered_wide$`estimate_Cluster robust`[1], 1), " and ",
         fmt(clustered_wide$`estimate_Cluster robust`[2], 1), " in both"),
  "the same in both panels",
  as.numeric(max(abs(clustered_wide$`estimate_Cluster robust` -
                       clustered_wide$`estimate_Student level, no clustering`)) < 1e-10), "",
  paste0("Chapter: \"While the group means are the same in both panels, the 95% confidence ",
         "intervals in Figure 17.4b were constructed ignoring the clustering\""),

  "Figure 17.4", "Panel b confidence intervals ignore clustering and are narrower",
  paste0("widths ", fmt(width_unclustered[1], 0), " and ", fmt(width_unclustered[2], 0),
         " against ", fmt(width_clustered[1], 0), " and ", fmt(width_clustered[2], 0)),
  "narrower", as.numeric(all(width_unclustered < width_clustered)), "",
  "Same sentence as the row above; the unclustered intervals are about a third as wide",

  "Figure 17.4", "Panel b vertical axis label",
  clustered_labels$y_label[clustered_labels$panel == "(b) Individuals"],
  "Outcome variable: SAT score",
  as.numeric(str_to_lower(clustered_labels$y_label[clustered_labels$panel == "(b) Individuals"]) ==
               str_to_lower("Outcome variable: SAT score")),
  "archive",
  paste0("The deposited script gives both panels the class-mean wording. The book restyled every ",
         "axis label to sentence case, and in panel (b) the restyling also dropped \"classroom ",
         "average\", which is a correction rather than a house style change, since panel (b) ",
         "plots students. Running the deposited script does not produce the published label"),

  "Figure 17.4", "Clustering can be handled by clustered standard errors or by weighting class means",
  paste0("both give ", fmt(clustered_ates$estimate[1], 3)),
  "either approach",
  as.numeric(abs(diff(clustered_ates$estimate)) < 1e-10), "",
  paste0("Chapter: \"either by using clustered standard errors or by aggregating outcomes to the ",
         "cluster-level and weighting the difference-in-means estimator by cluster size\". The two ",
         "point estimates agree to ", signif(abs(diff(clustered_ates$estimate)), 2),
         " and the standard errors to ", signif(abs(diff(clustered_ates$std.error)), 2),
         "; only the degrees of freedom differ"),

  "Figure 17.5", "N", param("Figure 17.5", "N"), NA_character_, NA_real_, "",
  "Not stated in the chapter",

  "Figure 17.5", "The vertical scale of both facets is the same but their range is not",
  paste0(fmt(covariate_ranges$y_span[1], 0), " units in each facet; ",
         fmt(covariate_ranges$y_min[covariate_ranges$estimation == "Unadjusted"], 0), " to ",
         fmt(covariate_ranges$y_max[covariate_ranges$estimation == "Unadjusted"], 0), " against ",
         fmt(covariate_ranges$y_min[covariate_ranges$estimation == "Adjusted"], 0), " to ",
         fmt(covariate_ranges$y_max[covariate_ranges$estimation == "Adjusted"], 0)),
  "the same scale but not the same range",
  as.numeric(n_distinct(covariate_ranges$y_span) == 1 &
               n_distinct(covariate_ranges$y_min) > 1), "",
  "Chapter: \"the vertical scale of both facets (but not their range) is set to be the same\"",

  "Figure 17.5", "The residual on residual slope equals the multiple regression estimate",
  fmt(resres_estimate, 4), "exactly equal",
  as.numeric(abs(lin_estimate - resres_estimate) < 1e-10), "",
  paste0("Chapter: the best linear summary of the residuals \"will be exactly the estimate ",
         "obtained from the multiple regression\". The two agree to ",
         signif(abs(lin_estimate - resres_estimate), 2), ". The deposited script asserts in a ",
         "comment that the standard errors are \"v. slightly off\" while the point estimates are ",
         "fine, and both halves hold: the standard errors are ",
         fmt(covariate$std.error[covariate$estimator == "lm_lin"], 5), " against ",
         fmt(covariate$std.error[covariate$estimator == "Residual on residual"], 5)),

  "Figure 17.6", "N", param("Figure 17.6", "N"), NA_character_, NA_real_, "",
  "Not stated in the chapter; the design samples 2,500 units with probability pnorm(X)",

  "Figure 17.6", "Negative effects at low covariate values and positive effects at high ones",
  paste0(fmt(cate_low), " at X = -2 and ", fmt(cate_high), " at X = 2"),
  "negative at low and positive at high",
  as.numeric(cate_low < 0 & cate_high > 0), "",
  paste0("Chapter: \"The model estimates negative effects for low values of the covariate and ",
         "positive effects for high values of the covariate\""),

  "Figure 17.6", "The linear model fits worst at low covariate values",
  paste0("mean residual ", fmt(lowest_bin_residual), " in the lowest bin"),
  "does not fit well, especially at low values",
  as.numeric(which.max(abs(residuals_binned$mean_residual)) == 1), "",
  paste0("Chapter: \"the statistical model does not fit the data particularly well, especially at ",
         "low values of the covariate\". Binning the covariate in half-unit steps, the largest ",
         "mean residual is in the lowest bin. The fit is also poor at the top of the range, where ",
         "the chapter does not say it is"),

  "Figure 17.7", "N", param("Figure 17.7", "N"), NA_character_, NA_real_, "",
  "Not stated in the chapter",

  "Figure 17.7", "N per assigned arm", param("Figure 17.7", "N assigned to treatment"),
  NA_character_, NA_real_, "", "Not stated in the chapter",

  "Figure 17.7", "Noncompliance is two-sided",
  paste0("treatment arm ", fmt(receipt_treated, 3), " and control arm ", fmt(receipt_control, 3)),
  "two-sided", as.numeric(receipt_control > 0 & receipt_treated < 1), "",
  paste0("Caption: \"Simulated experiment encountering two-sided noncompliance\". Some control ",
         "units take the treatment and some assigned units do not, so the label holds"),

  "Figure 17.8", "N", param("Figure 17.8", "N"), NA_character_, NA_real_, "",
  "Not stated in the chapter",

  "Figure 17.8", "N missing an outcome", param("Figure 17.8", "N missing an outcome"),
  NA_character_, NA_real_, "", "Not stated in the chapter",

  "Figure 17.8", "Outcome on a seven point Likert scale",
  param("Figure 17.8", "Outcome range"), "seven point Likert",
  as.numeric(param("Figure 17.8", "Outcome range") %in%
               c("1 to 7", "2 to 7", "1 to 6", "2 to 6")), "",
  paste0("Chapter: \"the outcome variable (measured on a seven-point Likert scale)\". The realised ",
         "draws use six of the seven categories, running ", param("Figure 17.8", "Outcome range")),

  "Figure 17.8", "Lower bound group means are very similar and the worst case effect is near zero",
  paste0(fmt(bound_mean("Control", "Lower Bound")), " and ",
         fmt(bound_mean("Treatment", "Lower Bound")), ", a difference of ",
         fmt(lower_bound_ate)),
  "very similar and close to zero", as.numeric(abs(lower_bound_ate) < 0.05), "",
  paste0("Chapter: \"the resulting group means (shown as large black points) are very similar in ",
         "treatment and control, so the implied worst-case treatment effect estimate is very close ",
         "to zero\". They are in fact equal to two decimal places"),

  "Figure 17.8", "Upper bound effect is close to a full scale point",
  fmt(upper_bound_ate), "close to a full-scale point",
  as.numeric(abs(upper_bound_ate - 1) < 0.25), "",
  paste0("Chapter: \"The upper-bound treatment effect estimate is close to a full-scale point\" ",
         "and the ATE \"is likely to lie somewhere between zero and one\". The bounds are ",
         fmt(lower_bound_ate), " to ", fmt(upper_bound_ate), ", so the upper bound sits a little ",
         "above the full point the chapter brackets it at")
)

# The rewrite and the archive agree on every estimate, asserted above, so the two
# columns carry the same value and the two match verdicts are the same verdict.
gt <- gt |>
  mutate(
    paper_id = "coppock_2021",
    value_rewrite = value_script,
    match_rewrite = match,
    defect_locus = if_else(match_rewrite == 0 & defect_locus == "", "unresolved", defect_locus),
    defect_locus = replace_na(defect_locus, "")
  ) |>
  select(paper_id, table_figure, claim, value_script, value_paper, match,
         value_rewrite, match_rewrite, defect_locus, notes)

stopifnot(all(gt$defect_locus[!is.na(gt$match_rewrite) & gt$match_rewrite == 0] != ""))

write_csv(gt, here::here("ground_truth", "coppock_2021_ground_truth.csv"))

print(gt |> select(table_figure, claim, value_script, value_paper, match), n = nrow(gt), width = 200)
print(str_glue("rows: {nrow(gt)}  match=1: {sum(gt$match == 1, na.rm = TRUE)}  ",
               "match=0: {sum(gt$match == 0, na.rm = TRUE)}  match=NA: {sum(is.na(gt$match))}"))
