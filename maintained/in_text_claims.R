# coppock_2021/maintained/in_text_claims.R
# Output: none (prints a claim-by-claim audit trail and a CLAIM SUMMARY line to stdout)
# Depends on: maintained/output/, ground_truth/published_claims.csv, errata_entries.csv,
#   helpers.R, excheckr
# Description: The second of the two instruments. Every claim the chapter makes that
#   the pipeline can reach gets a block here: the published sentence verbatim, then
#   code that reads the already-computed output and prints the quantity in the
#   chapter's own units and rounding.
#
#   It recomputes nothing. Estimation happens once, in the figure scripts; derivation
#   (differencing two rows, forming a ratio, recovering a confidence level from an
#   interval) happens twice, here and in ground_truth/build_ground_truth.R, by
#   deliberately different routes through the same outputs. Where the two disagree,
#   one of them is wrong.
#
#   It reads ground_truth/published_claims.csv, which is the extraction, and
#   errata_entries.csv, which is the errata spine, and never the ground-truth table,
#   which is the comparison.
#
#   The scoring comes from excheckr. claim() prints one line per claim, which is the
#   only link the coverage gate in build_ground_truth.R reads:
#     CLAIM <claim_id> = <value> || [<verdict>] <label>
#   A claim whose published value is a number is scored against it, and must [match].
#   Most of what this chapter says is a word rather than a number ("large and
#   negative", "schools", "400 to 1600"), and a word cannot be compared with a number,
#   so those blocks pass published = NA and declare expect = "shape": they print the
#   quantity that settles the sentence, and the predicate that settles it is evaluated
#   in build_ground_truth.R as `match` or `holds`. assert_claims() stops the run on a
#   MISMATCH, STALE, DRIFT or MISSING verdict, and claim_summary() prints the counts.

source(here::here("maintained", "helpers.R"))
library(excheckr)

options(width = 200)

published_claims <- read_csv(
  here::here("ground_truth", "published_claims.csv"),
  col_types = cols(.default = col_character())
)

errata <- read_csv(here::here("errata_entries.csv"), col_types = cols(.default = col_character()))

claim_start(published = published_claims, errata = errata, format = "id")

out <- function(f) read_csv(here::here("maintained", "output", f), show_col_types = FALSE)

# The article's own string, for a block that needs to name what the chapter says.
says <- function(id) {
  v <- published_claims$value_paper[published_claims$claim_id == id]
  stopifnot(length(v) == 1)
  v
}

# The precision the extraction records for a claim, so the two instruments round it
# the same way.
digits_for <- function(id) {
  d <- as.integer(published_claims$digits[published_claims$claim_id == id])
  stopifnot(length(d) == 1, !is.na(d))
  d
}

# A count the chapter spells out in words ("two"), as the numeral excheckr compares.
spelled <- function(id) {
  n <- match(says(id), c("one", "two", "three", "four", "five", "six", "seven", "eight"))
  stopifnot(!is.na(n))
  as.character(n)
}

design <- out("text_design_parameters.csv")
descriptive <- out("text_descriptive_claims.csv")
two_arm <- out("figure_1_two_arm_estimates.csv")
blocked_estimates <- out("figure_2_blocked_estimates.csv")
block_ates <- out("figure_3_blocked_block_ates.csv")
facet_estimates <- out("figure_3_blocked_facets_estimates.csv")
clustered <- out("figure_4_clustered_estimates.csv")
clustered_ates <- out("figure_4_clustered_ates.csv")
clustered_labels <- out("figure_4_clustered_axis_labels.csv")
covariate <- out("figure_5_covariate_adjustment_estimators.csv")
panel_ranges <- out("figure_5_covariate_adjustment_panel_ranges.csv")
cates <- out("figure_6_interaction_cates.csv")
binned <- out("figure_6_interaction_binned_residuals.csv")
noncompliance <- out("figure_7_noncompliance_estimates.csv")
attrition <- out("figure_8_attrition_estimates.csv")
stata <- out("text_stata_equivalent.csv")
manifest <- read_csv(here::here("original_manifest.csv"), show_col_types = FALSE)

param <- function(fig, q) design$value[design$figure == fig & design$quantity == q]
target <- function(fig, q) descriptive$value[descriptive$figure == fig & descriptive$quantity == q]

# A confidence level is recoverable from an interval, its standard error and its
# degrees of freedom, which is how both instruments read the chapter's "95%".
recover_level <- function(estimate, conf.high, std.error, df) {
  100 * (2 * pt((conf.high - estimate) / std.error, df) - 1)
}

# Section 17.2 Components of Experimental Designs ----

# "that, on average, the treatment raises subjects' latent probability of
#  supporting the advertising candidate by, for example, 10 percentage points;
#  and that realized outcomes are binary."
# The chapter hedges this one, and Section 17.5.1 then draws it as Figure 17.1. The 10
# is a property of the model the sentence describes, and the realised difference in
# means is one draw from it, so the block tests the sentence without being the token.
two_arm_mean <- function(cond) two_arm$estimate[two_arm$condition == cond]
canvassing_effect <- 100 * (two_arm_mean("Treatment") - two_arm_mean("Control"))
claim("canvassing_effect_pp", canvassing_effect,
      paste0("Figure 17.1 difference in means, percentage points; the chapter hedges with ",
             "\"for example\" and says ", says("canvassing_effect_pp")),
      digits = digits_for("canvassing_effect_pp"), expect = "derived")

# "In our two-arm trial, we might obtain a convenience sample of 500 voters from a
#  proprietary contact list, assign exactly 100 (due to budget or logistical
#  constraints) of them to treatment using complete random assignment, and then use
#  survey data to measure vote intentions."
claim("two_arm_sample", as.numeric(param("Figure 17.1", "N")),
      "Rows in the deposited two-arm dataset")
claim("two_arm_assigned", as.numeric(param("Figure 17.1", "N treated")),
      "Units with Z == 1 in the deposited two-arm dataset")

# Section 17.3 The Semiology and Grammar of Graphics ----

# "The best linear summary of the bivariate relationship between the residuals
#  Yir = Yi - (a0 + a1 X2,i) and X1,ir = X1,i - (g0 + g1 X2,i) will be exactly the
#  estimate b1 obtained from the multiple regression."
resres <- covariate$estimate[covariate$term == "Z_Adjusted"]
multiple_regression <- covariate$estimate[covariate$estimator == "Interacted, centred covariate"]
claim("resres_equals_mr", resres,
      paste0("Res-res slope; the multiple regression estimate is ",
             formatC(multiple_regression, format = "f", digits = digits_for("resres_equals_mr")),
             ", a difference of ", signif(abs(resres - multiple_regression), 2),
             "; the chapter says ", says("resres_equals_mr")),
      published = NA, digits = digits_for("resres_equals_mr"), expect = "shape")

# Footnote 3, p. 326 ----

# "The programs and data sets used to construct all figures are available on
#  Dataverse at https://doi.org/10.7910/DVN/VE6VSR. Equivalent Stata code for
#  Figure 17.1 is also provided."
archive_doi <- manifest$file_persistent_id |>
  str_remove("^doi:") |>
  str_remove("/[^/]+$") |>
  unique()
claim("archive_doi", as.numeric(archive_doi == says("archive_doi")),
      paste0("1 when the Dataverse persistent identifier this repository fetches, from ",
             "original_manifest.csv, is the one the footnote prints; it is ", archive_doi),
      published = NA, digits = 0, expect = "shape")

stata_widths <- stata |>
  mutate(width = conf.high - conf.low) |>
  select(condition, method, width) |>
  pivot_wider(names_from = method, values_from = width)
claim("stata_code_provided", max(abs(stata_widths[[2]] - stata_widths[[3]])),
      paste0("Largest gap between the Stata file's intervals and the R script's for the same ",
             "two group means, on a 0 to 1 outcome; the two point estimates are identical"),
      published = NA, digits = 4, expect = "shape")

# Section 17.5.1 Two-Arm Trial ----

# "Specifically, imagine a 500-person experiment in which exactly 100 units are
#  treated and the outcome is binary."
# Read off the regressions rather than the dataset, so this route is not the one
# the design-parameter script takes: each group mean is an intercept-only fit whose
# residual degrees of freedom are one less than the group size.
group_n <- two_arm$df + 1
claim("fig1_n", sum(group_n),
      "Sum of the two group sizes implied by the Figure 17.1 regressions",
      digits = digits_for("fig1_n"))
claim("fig1_n_treated", group_n[two_arm$condition == "Treatment"],
      "Treated group size implied by the Figure 17.1 regressions",
      digits = digits_for("fig1_n_treated"))
claim("fig1_outcome_binary",
      as.numeric(param("Figure 17.1", "Number of distinct outcome values")),
      paste0("Distinct values of the deposited two-arm outcome, over ",
             param("Figure 17.1", "Outcome range")),
      published = NA, digits = 0, expect = "shape")

# "Another virtue of Figure 17.1a is that it displays the uncertainty estimates in
#  the form of 95% confidence intervals for each group mean."
fig1_levels <- recover_level(two_arm$estimate, two_arm$conf.high, two_arm$std.error, two_arm$df)
stopifnot(diff(range(fig1_levels)) < 1e-9)
claim("fig1_ci_level", fig1_levels[1],
      "Confidence level recovered from the Figure 17.1 interval half-widths, per cent",
      digits = digits_for("fig1_ci_level"))

# "By contrast, Figure 17.1b communicates far less. It only displays two numbers
#  that could be represented just as well in text with no figure at all."
claim("fig1b_two_numbers", nrow(two_arm),
      "Group means the bad panel of Figure 17.1 plots, and the only quantities it shows",
      published = spelled("fig1b_two_numbers"), digits = digits_for("fig1b_two_numbers"))

# Section 17.5.2 Blocked Experiments ----

# "For this example, imagine an experiment conducted in two neighborhoods. The first
#  neighborhood has 50 residents and the second has 100. For logistical reasons, the
#  experimental partner needs to treat exactly 25 residents in each neighborhood."
# Again read off the fits behind Figure 17.3a: within each block, the control and
# treatment group sizes are one more than the residual degrees of freedom.
block_fits <- facet_estimates |> filter(panel == "Faceted by block")
block_sizes <- block_fits |>
  group_by(block) |>
  summarise(n = sum(df + 1), .groups = "drop") |>
  arrange(block, .locale = "en")
claim("blocked_n_neighborhoods", nrow(block_sizes),
      "Blocks with their own panel in Figure 17.3a",
      digits = digits_for("blocked_n_neighborhoods"))
claim("blocked_n1", block_sizes$n[1],
      paste0("Residents in ", block_sizes$block[1], ", from the Figure 17.3a fits"),
      digits = digits_for("blocked_n1"))
claim("blocked_n2", block_sizes$n[2],
      paste0("Residents in ", block_sizes$block[2], ", from the Figure 17.3a fits"),
      digits = digits_for("blocked_n2"))
treated_per_block <- block_fits |> filter(condition == "Treatment") |> pull(df) + 1
stopifnot(n_distinct(treated_per_block) == 1)
claim("blocked_n_treated", treated_per_block[1],
      "Treated residents per block, identical in both blocks",
      digits = digits_for("blocked_n_treated"))

# "The probability of treatment is higher in the first neighborhood than the second,
#  which could cause bias in the unadjusted difference-in-means estimator."
prob_by_block <- block_ates |> arrange(neighborhood)
claim("blocked_prob_higher_first", prob_by_block$prob_treated[1],
      paste0("Assignment probability in the first neighborhood, against ",
             prob_by_block$prob_treated[2], " in the second; the chapter says ",
             says("blocked_prob_higher_first")),
      published = NA, digits = 2, expect = "shape")

# "The outcome is a count of some behavioral outcome over the course of the
#  experiment, which varies by neighborhood and by treatment condition."
claim("blocked_outcome_count",
      as.numeric(param("Figures 17.2 and 17.3", "Number of non-integer outcomes")),
      paste0("Deposited blocked outcomes that are not non-negative integers, over the range ",
             param("Figures 17.2 and 17.3", "Outcome range"),
             ", tested against what a count would allow"),
      published = NA, digits = 0, expect = "shape")

# "In this cooked-up example, the correct analysis shows that the ATE estimate is
#  large and negative whereas the incorrect analysis estimates the ATE to be close
#  to zero."
# Differenced from the plotted group means rather than read off the ATE table, so
# this is a second route to the same two numbers.
panel_ate <- function(p) {
  m <- blocked_estimates |> filter(panel == p)
  m$estimate[m$condition == "Treatment"] - m$estimate[m$condition == "Control"]
}
claim("blocked_weighted_ate", panel_ate("Inverse probability weighted"),
      paste0("Difference between the weighted group means of Figure 17.2a; the chapter says ",
             says("blocked_weighted_ate")),
      published = NA, digits = digits_for("blocked_weighted_ate"), expect = "shape")
claim("blocked_unweighted_ate", panel_ate("Unweighted"),
      paste0("Difference between the unweighted group means of Figure 17.2b; the chapter says ",
             says("blocked_unweighted_ate")),
      published = NA, digits = digits_for("blocked_unweighted_ate"), expect = "shape")

# "Within each facet, the groups that are compared are formed by random assignment:
#  we see small effects of treatment in neighborhood 1 and large negative effects of
#  treatment in neighborhood 2."
block_effect <- function(b) {
  m <- block_fits |> filter(block == b)
  m$estimate[m$condition == "Treatment"] - m$estimate[m$condition == "Control"]
}
claim("fig3_block_effects", block_effect(block_sizes$block[1]),
      paste0("Within-block effect in ", block_sizes$block[1], " behind Figure 17.3a, against ",
             formatC(block_effect(block_sizes$block[2]), format = "f",
                     digits = digits_for("fig3_block_effects")),
             " in ", block_sizes$block[2], "; the chapter says ", says("fig3_block_effects")),
      published = NA, digits = digits_for("fig3_block_effects"), expect = "shape")

# "By contrast, in Figure 17.3b, we facet by randomly assigned group, so we compare
#  across schools and within treatment group."
compared_units <- facet_estimates |>
  filter(panel == "Faceted by condition") |>
  distinct(block) |>
  arrange(block, .locale = "en") |>
  pull(block)
claim("fig3b_across_schools", length(compared_units),
      paste0("Groups Figure 17.3b compares across (", paste(compared_units, collapse = ", "),
             "); the chapter calls them ", says("fig3b_across_schools"),
             " and the deposit has no school or class variable outside the Figure 17.4 dataset"),
      published = NA, digits = 0, expect = "shape")

# The published panel (b) reads "Neighborhood 1" and "Neighborhood 2"; the deposited
# script abbreviates. Nothing published is wrong, so this is a fact about the deposit.
claim("fig3b_axis_labels", as.numeric(compared_units[1] == says("fig3b_axis_labels")),
      paste0("1 when the first horizontal axis label Figure 17.3b's script writes (",
             compared_units[1], ") is the one the published panel reads (",
             says("fig3b_axis_labels"), ")"),
      published = NA, digits = 0, expect = "shape")

# Section 17.5.3 Clustered Experiments ----

# "In this example, we imagine a 'test-prep' treatment that is randomly assigned to
#  some classrooms but not to others; the outcome is measured on a 400-1600-point
#  scale."
claim("clustered_outcome_scale",
      as.numeric(target("Figure 17.4", "Students with an outcome outside the stated 400 to 1600 scale")),
      paste0("Students of ", target("Figure 17.4", "Students in total"),
             " outside the scale the chapter states (", says("clustered_outcome_scale"),
             "); the deposited clustered outcome runs ", param("Figure 17.4", "Outcome range")),
      published = NA, digits = 0, expect = "shape")

# "The analysis must account for the clustering, either by using clustered standard
#  errors or by aggregating outcomes to the cluster-level and weighting the
#  difference-in-means estimator by cluster size."
claim("clustered_either_or", abs(diff(clustered_ates$estimate)),
      paste0("Gap between the two estimators the chapter says are interchangeable, on a ",
             "point estimate of ", round(clustered_ates$estimate[1], 1)),
      published = NA, digits = 10, expect = "shape")

# "The weighted means are plotted along with 95% confidence intervals that are
#  calculated accounting for clustering."
fig4_levels <- clustered |>
  filter(panel == "Cluster robust") |>
  transmute(level = recover_level(estimate, conf.high, std.error, df))
claim("fig4_ci_level", max(fig4_levels$level),
      "Confidence level recovered from the clustered interval half-widths, per cent",
      digits = digits_for("fig4_ci_level"))

# "While the group means are the same in both panels, the 95% confidence intervals
#  in Figure 17.4b were constructed ignoring the clustering."
means_wide <- clustered |>
  select(condition, panel, estimate) |>
  pivot_wider(names_from = panel, values_from = estimate)
claim("fig4_means_same", max(abs(means_wide[[2]] - means_wide[[3]])),
      "Largest gap between the group means of Figure 17.4a and Figure 17.4b",
      published = NA, digits = 10, expect = "shape")

widths <- clustered |>
  mutate(width = conf.high - conf.low) |>
  select(condition, panel, width) |>
  pivot_wider(names_from = panel, values_from = width)
width_ratio <- 100 * widths$`Student level, no clustering` / widths$`Cluster robust`
claim("fig4_ci_ignore_clustering", max(width_ratio),
      paste0("Unclustered interval width as a percentage of the clustered width, the wider of ",
             "the two arms; control then treatment are ",
             paste(round(width_ratio), collapse = " and ")),
      published = NA, digits = 0, expect = "shape")

# "Outcome variable: SAT score" (the vertical axis of Figure 17.4b)
panel_b_label <- clustered_labels$y_label[clustered_labels$panel == "(b) Individuals"]
claim("fig4b_axis_label", as.numeric(panel_b_label == says("fig4b_axis_label")),
      paste0("1 when the label the script writes for panel (b) (\"", panel_b_label,
             "\") is the one the published panel reads (\"", says("fig4b_axis_label"), "\")"),
      published = NA, digits = 0, expect = "shape")

# Section 17.5.4 Covariate Adjustment ----

# "Here, I use the procedure recommended in Lin (2013) to adjust each treatment arm
#  separately, equivalent to interacting the covariates with the treatment indicator
#  and then predicting the average outcome in each treatment condition."
lin <- covariate$estimate[covariate$estimator == "lm_lin"]
claim("fig5_lin_separately", lin,
      paste0("lm_lin estimate; the interacted, centred-covariate regression gives ",
             formatC(multiple_regression, format = "f", digits = digits_for("fig5_lin_separately")),
             "; the chapter says ", says("fig5_lin_separately")),
      published = NA, digits = digits_for("fig5_lin_separately"), expect = "shape")

# "In order to clearly see the variance reduction from covariate adjustment, the
#  vertical scale of both facets (but not their range) is set to be the same."
claim("fig5_scale_not_range", n_distinct(panel_ranges$y_span),
      paste0("Distinct vertical spans across the two facets of Figure 17.5 (spans ",
             paste(panel_ranges$y_span, collapse = " and "), ", ranges ",
             paste0(panel_ranges$y_min, " to ", panel_ranges$y_max, collapse = " and "), ")"),
      published = NA, digits = 0, expect = "shape")

# "Outcome variable (raw scale is 7-point Likert)" (the vertical axis of Figure 17.5)
claim("fig5_axis_label_likert",
      as.numeric(param("Figure 17.5", "Number of distinct outcome values")),
      paste0("Distinct values of the deposited covariate outcome over ",
             param("Figure 17.5", "Outcome range"), ", across ", param("Figure 17.5", "N"),
             " units; the axis says ", says("fig5_axis_label_likert"), " and ",
             target("Figure 17.5", "Units with an outcome outside the 1 to 7 range the axis label implies"),
             " units fall outside the 1 to 7 range such a scale allows"),
      published = NA, digits = 0, expect = "shape")

# Section 17.5.5 Interactions with a Continuous Covariate ----

# "The model estimates negative effects for low values of the covariate and positive
#  effects for high values of the covariate."
claim("fig6_cate_signs", cates$AME[which.min(cates$X)],
      paste0("Estimated CATE at X = ", min(cates$X), ", the low end of the grid Figure 17.6b ",
             "plots, against ", formatC(cates$AME[which.max(cates$X)], format = "f", digits = 2),
             " at X = ", max(cates$X)),
      published = NA, digits = 2, expect = "shape")

# "The graph also reveals that the statistical model does not fit the data
#  particularly well, especially at low values of the covariate."
worst <- binned |> slice_max(abs(mean_residual), n = 1)
claim("fig6_fit_low", worst$mean_residual,
      paste0("Largest mean residual over half-unit bins of the covariate, in bin ", worst$X_bin,
             ", the lowest occupied bin being ", binned$X_bin[1]),
      published = NA, digits = 2, expect = "shape")

# "Figure 17.6b visualizes the results by plotting the estimated CATE for a series of
#  values of the covariate."
claim("fig6_cate_grid", nrow(cates),
      paste0("Points Figure 17.6b plots, over ", min(cates$X), " to ", max(cates$X),
             "; the chapter states no count"),
      digits = digits_for("fig6_cate_grid"), expect = "shape")

# Section 17.5.6 Noncompliance ----

# "The standard analytic approach in such cases is to estimate the effect of
#  assignment on two post-treatment variables: treatment receipt and the outcome of
#  interest."
assigned <- noncompliance |> filter(panel == "By assigned group")
claim("noncompliance_two_dvs", n_distinct(assigned$group),
      paste0("Dependent variables Figure 17.7a plots: ",
             paste(sort(unique(assigned$group)), collapse = " and ")),
      published = spelled("noncompliance_two_dvs"), digits = digits_for("noncompliance_two_dvs"))

# "Figure 17.7 Simulated experiment encountering two-sided noncompliance."
receipt <- assigned |> filter(group == "Treatment Receipt")
claim("fig7_two_sided", receipt$estimate[receipt$Z == "Control"],
      paste0("Treatment receipt rate in control, against ",
             round(receipt$estimate[receipt$Z == "Treatment"], 3), " in treatment; ",
             "noncompliance is two-sided when the first exceeds zero and the second falls short of one"),
      published = NA, digits = 3, expect = "shape")

# Section 17.5.7 Attrition ----

# "As in Figure 17.1, the random assignment is mapped to the horizontal axis and the
#  outcome variable (measured on a seven-point Likert scale) is mapped to the
#  vertical axis."
claim("attrition_likert_scale",
      as.numeric(target("Figure 17.8", "Distinct Likert categories the draws use, of the stated seven")),
      paste0("Categories of seven the observed attrition outcomes use, over ",
             param("Figure 17.8", "Outcome range"), "; ",
             target("Figure 17.8", "Observed outcomes outside the stated 1 to 7 Likert scale"),
             " fall outside the stated scale"),
      published = NA, digits = 0, expect = "shape")

# "An upper bound on the treatment effect estimate is obtained by imputing the
#  maximum possible outcome for all missing treated units and the minimum possible
#  outcome for all missing control units. The lower bound is obtained by doing the
#  reverse."
# Swapping the imputations moves each arm's mean by the imputation span times that
# arm's missing share, so the span is recoverable from the two bounds and the total
# number of missing outcomes without knowing how they split across arms.
bound_mean <- function(cond, b) {
  attrition$estimate[attrition$Condition == cond & attrition$bound == b]
}
n_units <- unique(attrition$n_total) / n_distinct(attrition$Condition)
swing <- (bound_mean("Treatment", "Upper Bound") - bound_mean("Treatment", "Lower Bound")) +
  (bound_mean("Control", "Lower Bound") - bound_mean("Control", "Upper Bound"))
imputation_span <- swing * n_units / unique(attrition$n_missing_outcome)
claim("attrition_bound_imputation", imputation_span,
      "Distance between the imputed maximum and minimum, recovered from the two bounds; a seven-point scale anchored at 1 gives 6",
      published = NA, digits = 1, expect = "shape")

# "In this case, the resulting group means (shown as large black points) are very
#  similar in treatment and control, so the implied worst-case treatment effect
#  estimate is very close to zero."
lower_ate <- bound_mean("Treatment", "Lower Bound") - bound_mean("Control", "Lower Bound")
lower_digits <- digits_for("fig8_lower_bound")
claim("fig8_lower_bound", lower_ate,
      paste0("Difference between the lower-bound group means, ",
             formatC(bound_mean("Control", "Lower Bound"), format = "f", digits = lower_digits),
             " in control and ",
             formatC(bound_mean("Treatment", "Lower Bound"), format = "f", digits = lower_digits),
             " in treatment"),
      published = NA, digits = lower_digits, expect = "shape")

# "The upper-bound treatment effect estimate is close to a full-scale point."
upper_ate <- bound_mean("Treatment", "Upper Bound") - bound_mean("Control", "Upper Bound")
claim("fig8_upper_bound", upper_ate,
      "Upper-bound treatment effect, on a scale whose full point is 1",
      published = NA, digits = digits_for("fig8_upper_bound"), expect = "shape")

# "even with attrition that is extremely correlated with potential outcomes, the ATE
#  is likely to lie somewhere between zero and one."
claim("fig8_bounds_range", upper_ate,
      paste0("Upper end of the extreme value bounds on the ATE, whose lower end is ",
             formatC(round(lower_ate, digits_for("fig8_bounds_range")) + 0, format = "f",
                     digits = digits_for("fig8_bounds_range")),
             "; the chapter says ", says("fig8_bounds_range"), ", and says in the same ",
             "paragraph that the bounds are themselves estimates subject to sampling variability"),
      published = NA, digits = digits_for("fig8_bounds_range"), expect = "shape")

# Gates ----
# assert_claims() stops the run on a failing verdict, on a quantity erratum whose claim
# was not printed, and on any difference between the claims printed and the extraction
# rows that declare needs_block. claim_summary() then prints the verdict counts.
assert_claims()
invisible(claim_summary())
