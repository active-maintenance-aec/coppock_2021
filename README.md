# Reproducibility Report: Coppock (2021)


- [Summary](#summary)
  - [Does the deposited archive run?](#does-the-deposited-archive-run)
  - [Does the maintained rewrite reproduce the
    chapter?](#does-the-maintained-rewrite-reproduce-the-chapter)
- [Chapter overview](#chapter-overview)
- [Original archive reproducibility](#original-archive-reproducibility)
  - [The data generator](#the-data-generator)
  - [The Stata file](#the-stata-file)
- [Number-by-number comparison](#number-by-number-comparison)
  - [The transposed neighborhoods](#the-transposed-neighborhoods)
  - [The blocks that are called
    schools](#the-blocks-that-are-called-schools)
  - [The 400 to 1600 point scale](#the-400-to-1600-point-scale)
  - [The Likert scale that is not
    one](#the-likert-scale-that-is-not-one)
  - [The relabelled panel](#the-relabelled-panel)
- [The extraction and the two
  instruments](#the-extraction-and-the-two-instruments)
- [Maintained rewrite](#maintained-rewrite)
  - [Deprecated patterns replaced](#deprecated-patterns-replaced)
  - [Three things left alone](#three-things-left-alone)
- [The rewrite against the archive](#the-rewrite-against-the-archive)
- [The data generator under the current
  API](#the-data-generator-under-the-current-api)
- [Figure verification](#figure-verification)
- [Maintained rewrite verification](#maintained-rewrite-verification)
- [R environment](#r-environment)

*Drafted by Claude Opus 5 under the supervision of Alex Coppock.*

This repository holds the actively maintained replication code for
Coppock (2021), together with the reproducibility report that documents
what the original archive did and did not do. It is part of a program
applying the maintenance proposal in Peer, Orr and Coppock (2021, *PS:
Political Science & Politics*, doi
[10.1017/S1049096521000366](https://doi.org/10.1017/S1049096521000366))
to a set of published archives.

|  |  |
|----|----|
| Chapter | [10.1017/9781108777919.022](https://doi.org/10.1017/9781108777919.022) |
| Replication archive | [10.7910/DVN/VE6VSR](https://doi.org/10.7910/DVN/VE6VSR) |
| Pre-analysis plan | none: the chapter analyses simulated data |

**The data are not redistributed here.** The deposit is 280 KB across 17
files and lives at Harvard Dataverse, which is the only copy this
repository points at. `download_original.R` fetches it and verifies
every file; `original_manifest.csv` pins the Dataverse file identifiers,
the UNF of each ingested data file, the deposit’s own directory layout,
and two checksums per file: the MD5 of the bytes Dataverse serves for
`?format=original`, which is what this code was written against, and the
MD5 Dataverse publishes. Here the two agree for all seventeen. They do
not always, so the script verifies against the served bytes and reports
any disagreement. Either way the exact bytes are pinned in version
control even though the bytes themselves are not.

**Repository layout.** `maintained/` is the maintained rewrite: one
script per published figure, writing to `output/`, which is committed so
a reader can compare a fresh run against it without downloading
anything. `ground_truth/` ties every claim the chapter makes about its
figures to the code that produces it, and is itself built by a script
rather than typed; `ground_truth/published_claims.csv` is the
extraction, a hand-reviewed inventory of every number and counted
quantity in the chapter. `maintained/in_text_claims.R` recomputes each
of those the pipeline can reach and prints it beside the sentence that
states it. `coppock_2021_errata.pdf` lists the sentences and labels in
the chapter that the deposited data do not support. `original/` is
created by the download script and is deliberately absent from the
repository. This file is the reproducibility report, also available as a
PDF in `report/`.

**License.** CC0 1.0 Universal, matching the terms of the deposit this
repository maintains, so nothing in the chain is more restrictive than
the archive itself. See `LICENSE`.

**To reproduce.** Clone or download the repository, open
`coppock_2021.Rproj`, and run:

``` r
source("run_all.R")
```

That fetches the deposit, verifies its seventeen checksums, produces
every figure into `maintained/output/`, rebuilds the ground truth table
from those outputs, runs the coverage gate over the claims file, and
prints the claim-by-claim audit trail. It takes about ten seconds.
Required packages: tidyverse, estimatr, broom, margins, DeclareDesign,
knitr, kableExtra, here. Paths resolve through `here`, so nothing
depends on the working directory and the scripts work equally well under
`Rscript` outside RStudio. Individual scripts can be run on their own in
any order, with two exceptions: `text_archive_agreement.R` reads the
figure scripts’ output and `build_ground_truth.R` reads everything, so
both come last.

A successful run overwrites `maintained/output/`, which is committed:
**`git diff` on that folder is the reproduction check**, and the CSV and
PNG output should come back byte-identical. The fourteen PDF figures
always show as changed, because a PDF records the time it was written;
compare their PNG twins instead.

# Summary

Two questions, answered before the detail.

## Does the deposited archive run?

Not on its own. Six of the archive’s eight R scripts fail in a clean R
session, and the two that survive do so by accident: `blocked.R` and
`interaction.R` happen to load `estimatr`, which re-exports the `tidy()`
generic the other scripts call without loading anything that provides
it. Four scripts stop at `could not find function "tidy"` and one at
`could not find function "lm_lin"`, all for the same reason, which is
that every script opens with `library(tidyverse)` and nothing else.
Adding `library(broom)` and `library(estimatr)` makes all seven analysis
scripts run to completion, so the failure is a missing declaration
rather than a missing package. This is what a script that has only ever
been run in a warm session looks like from outside: it works for its
author and stops for everybody else.

The eighth script is the data generator, and it fails for three separate
reasons. `declare_assignment()` rejects the pre-1.0 DeclareDesign
syntax, `draw_likert()` no longer supplies default cut points, and
`declare_sampling()` rejects its own pre-1.0 syntax at the last design
in the file. All three are repairable from the deposit alone, and none
requires an old version of anything: DeclareDesign 1.1.1 still accepts
the old assignment and sampling syntax behind `legacy = TRUE`.

## Does the maintained rewrite reproduce the chapter?

Every figure reproduces. What does not is a handful of claims the
chapter makes about those figures, most of which belong to the chapter
rather than to the code. Of 40 recorded claims, 27 can be checked
against the published chapter; 21 hold and 6 do not, four of the latter
being sentences or labels the chapter’s own data contradict. The 13
remaining are quantities the chapter never states, which is a large
share, because this is a methods chapter with no tables and no reported
estimates: its figures are drawn from simulated data and its claims are
about shape and direction rather than magnitude. The four that are the
chapter’s own are collected in `coppock_2021_errata.pdf` at the root of
this repository; none of them changes a conclusion.

| Component | Verdict |
|:---|:---|
| Figures 17.1, 17.2, 17.6, 17.7, 17.8 | Reproduce |
| Figure 17.3 (blocked, faceted by block) | Figure reproduces; the text describing it transposes the two neighborhoods, and calls the blocks schools |
| Figure 17.3, panel (b) axis labels | The deposited script abbreviates where the published panel spells out |
| Figure 17.4 (clustered), estimates | Reproduce |
| Figure 17.4 (clustered), stated outcome scale | 8 of 441 outcomes fall outside the stated 400 to 1600 range |
| Figure 17.4 (clustered), panel (b) axis label | The deposited script does not produce the published label |
| Figure 17.5 (covariate adjustment), estimates | Reproduce |
| Figure 17.5, vertical axis label | Describes a seven-point Likert raw scale; the outcome takes 100 distinct values |
| Data generator (seven simulated datasets) | 48 of 60 columns regenerate identically |

Reproduction verdict by component.

All six failures are worth naming, and four of them are the chapter’s
own. It says the blocked experiment shows “small effects of treatment in
neighborhood 1 and large negative effects of treatment in neighborhood
2”; the two neighborhoods are the other way round, in the deposited
data, in the deposited generator, and in the published figure. In the
next sentence it says Figure 17.3b compares “across schools”, of a
design with 2 neighborhoods and no schools. It says the clustered
outcome is “measured on a 400-1600-point scale”; 8 of the 441 simulated
outcomes fall outside it and both panels clip. And the vertical axis of
Figure 17.5 calls its raw scale a seven-point Likert, where the outcome
takes 100 distinct values across 100 units.

The remaining two are the deposit falling short of the book, in the same
place and in the same direction both times: the deposited scripts label
two panels less well than the published figures do. `clustered.R` labels
both panels of Figure 17.4 as classroom averages, while the published
panel (b), which plots students, is labelled “Outcome variable: SAT
score”. `blocked.R` abbreviates the horizontal axis of Figure 17.3b to
“N/hood”, while the published panel spells “Neighborhood” out. In both
cases the published figure is the better one and running the deposited
script does not produce it.

# Chapter overview

**Citation**: Coppock, A. (2021). “Visualize as you randomize:
Design-based statistical graphs for randomized experiments.” In J. N.
Druckman and D. P. Green (eds.), *Advances in Experimental Political
Science*, 320-336. Cambridge University Press. DOI:
10.1017/9781108777919.022, dated by Crossref to April 2021.

**Summary**: A methods chapter arguing that a graph of an experiment
should encode the design as well as the result. Two design principles do
the work: invite visual comparisons across randomly formed groups rather
than across groups formed before or after treatment, and show the fitted
model and its uncertainty in data space rather than on its own. A third
asks that design features like blocking, clustering and differential
assignment probabilities be mapped to visual cues. The chapter works
through seven designs, each with a panel that follows the advice and a
panel that does not: a two-arm trial, a blocked experiment, the same
blocked experiment faceted two ways, a cluster-randomized trial,
covariate adjustment shown as a residual-on-residual plot, an
interaction with a continuous covariate, two-sided noncompliance, and
attrition with extreme value bounds. All of the data are simulated, so
the chapter reports no estimates and prints no tables. Its empirical
content is the figures themselves and the design parameters it states in
prose.

# Original archive reproducibility

**Archive source**: Harvard Dataverse, DOI 10.7910/DVN/VE6VSR. Seventeen
files under a single `replication_archive` directory label: eight R
scripts, one Stata `.do` file, seven simulated datasets and a README.
All seventeen served checksums match the published ones, and the
deposit’s own README describes the contents accurately, which is not
something to take for granted.

| Script | Status in a clean R session | Resolution |
|:---|:---|:---|
| two_arm_trial.R | Fails: could not find function ‘tidy’ | library(broom) |
| blocked.R | Runs | No changes required |
| clustered.R | Fails: could not find function ‘tidy’ | library(broom) |
| covariate_adjustment.R | Fails: could not find function ‘lm_lin’ | library(estimatr), then library(broom) |
| noncompliance.R | Fails: could not find function ‘tidy’ | library(broom) |
| interaction.R | Runs | No changes required |
| attrition.R | Fails: could not find function ‘tidy’ | library(broom) |
| make_datasets.R | Fails: three separate pre-1.0 DeclareDesign incompatibilities | legacy = TRUE on assignment and sampling; explicit breaks for draw_likert |
| two_arm_trial.do | Not run: no Stata licence here, and it reads from a data/ path the deposit does not contain | Its arithmetic is reproduced in R and compared against the R figure |

Original archive reproducibility, checked against R 4.6.0.

Every package the archive names still installs from CRAN: `tidyverse`,
`estimatr`, `DeclareDesign`, `MASS`, `margins` and `ggrepel`. Nothing
here is unmaintained, and nothing needed substituting for want of a
package. Two patterns that commonly break in archives of this vintage do
not break here. `do(tidy(...))`, `gather()` and `spread()` emit no
warnings at all, being superseded rather than deprecated. And
`stat_smooth(method = "lm_robust")` is recognised by ggplot2 4.0.3,
because `stat_smooth` resolves a method string with `match.fun()` and
`estimatr` is attached by the time the plot is drawn.

Running the archive also leaves seven files behind that the deposit does
not contain: six figure PDFs written into whatever directory the scripts
were run from, plus an `Rplots.pdf` from the device R opens under
`Rscript`. Those scripts were run in a scratch copy for that reason, and
`download_original.R` prints a warning if anything but the deposit turns
up in `original/`.

## The data generator

`make_datasets.R` is the interesting failure. It declares seven designs
in the DeclareDesign syntax of 2019, writes out the seven datasets the
analysis scripts read, and stops at the third line of the first design.
There are three incompatibilities, and none of them requires an old
version:

- `declare_assignment(m = 100)` and its blocked and clustered variants.
  DeclareDesign 1.0 removed the named-argument form, and its own error
  message names the replacement. Passing `legacy = TRUE` restores the
  old behaviour in 1.1.1.
- `draw_likert(0.5 * Z + U)`. `fabricatr` no longer supplies default cut
  points and now demands `breaks`, or `min`, `max` and `bins`. The cut
  points the deposit was built with are recoverable from the deposited
  file, and are the unit-width breaks from -2.5 to 2.5.
- `declare_sampling(prob_unit = pnorm(X), simple = TRUE)`, which fails
  the same way as the assignment steps and takes the same
  `legacy = TRUE`.

Repaired, the generator runs to the end, and what it produces is the
subject of a later section.

## The Stata file

The deposit’s README says its Stata file “produces something similar to
Figure 1”, which is a claim about output that nobody without a Stata
licence can check by running it. The file is nine lines of arithmetic,
so `text_stata_equivalent.R` reproduces them in R and compares the
result against the intervals the R script draws.

| Condition | Method | Mean | Lower | Upper |
|:---|:---|:---|:---|:---|
| Control | R script: lm_robust HC2, t distribution | 0.515 | 0.466 | 0.564 |
| Control | Stata .do file: sd / sqrt(N), normal approximation | 0.515 | 0.466 | 0.564 |
| Treatment | R script: lm_robust HC2, t distribution | 0.610 | 0.513 | 0.707 |
| Treatment | Stata .do file: sd / sqrt(N), normal approximation | 0.610 | 0.514 | 0.706 |

The deposited Stata file’s intervals against the R script’s, on the same
data.

The claim holds. The group means are identical, and the intervals differ
only because the Stata file builds a normal-approximation interval from
the group standard deviation where the R script builds an HC2 interval
on the t distribution. On a 0 to 1 outcome the widths differ by at most
0.0024. What the Stata file cannot do is run as deposited: it reads
`data/two_arm_simulated_data.csv` and the deposit has no `data/`
directory, so the path has to be repaired before the file will open its
own data.

# Number-by-number comparison

The ground truth is built by `ground_truth/build_ground_truth.R`, which
reads every archive and rewrite value out of `maintained/output/` and
writes the table. Nothing in it is typed except the values read from the
published chapter, and those are only ever comparison targets: no
published number is an input to any computation in this repository.

The chapter is unusual in what it gives a reader to check. It has no
tables, prints no estimates, and states no standard errors, so most of
its checkable content is qualitative: a direction, an ordering, a stated
range, an axis label. A claim about a value carries its verdict in the
`Match` column; a claim about shape, sign or ordering has no value to
compare and carries its verdict in `Holds`. Neither is typed. Both are
predicates the build script evaluates against the pipeline output, so
that a claim like “the ATE estimate is large and negative” is settled by
asking whether the interval excludes zero rather than by an author’s
judgement about what counts as large.

| Location | Claim | Chapter | Archive data | Match | Holds |
|:---|:---|:---|:---|:---|:---|
| Figure 17.1 | N | 500 | 500 | 1 |  |
| Figure 17.1 | N treated | 100 | 100 | 1 |  |
| Figure 17.1 | Control group mean |  | 0.515 |  |  |
| Figure 17.1 | Treatment group mean |  | 0.610 |  |  |
| Figure 17.1 | The deposited Stata file draws something similar to the R figure |  | intervals differ in width by at most 0.0024 |  |  |
| Figures 17.2 and 17.3 | Number of neighborhoods | 2 | 2 | 1 |  |
| Figures 17.2 and 17.3 | N in neighborhood 1 | 50 | 50 | 1 |  |
| Figures 17.2 and 17.3 | N in neighborhood 2 | 100 | 100 | 1 |  |
| Figures 17.2 and 17.3 | N treated per neighborhood | 25 | 25 | 1 |  |
| Figures 17.2 and 17.3 | N total |  | 150 |  |  |
| Figure 17.2 | Probability of treatment is higher in the first neighborhood | higher in the first | 0.50 against 0.25 |  | 1 |
| Figure 17.2 | Inverse probability weighted ATE is large and negative | large and negative | -1.284 |  | 1 |
| Figure 17.2 | Unweighted ATE is close to zero | close to zero | -0.700 |  | 1 |
| Figure 17.3 | Small effect in neighborhood 1 and a large negative effect in neighborhood 2 | small in 1 and large negative in 2 | neighborhood 1 = -4.36; neighborhood 2 = 0.25 |  | 0 |
| Figure 17.3 | Panel b compares across schools | schools | N/hood 1, N/hood 2 |  | 0 |
| Figure 17.3 | Panel b horizontal axis labels | Neighborhood 1 | N/hood 1, N/hood 2 | 0 |  |
| Figure 17.4 | Number of classes |  | 30 |  |  |
| Figure 17.4 | N students |  | 441 |  |  |
| Figure 17.4 | Outcome measured on a 400 to 1600 point scale | 400 to 1600 | 217 to 1720 | 0 |  |
| Figure 17.4 | Group means are the same in both panels | the same in both panels | 957.9 and 1117.8 in both |  | 1 |
| Figure 17.4 | Panel b confidence intervals ignore clustering and are narrower | narrower | widths 59 and 51 against 174 and 107 |  | 1 |
| Figure 17.4 | Panel b vertical axis label | Outcome variable: SAT score | Outcome variable: Classroom Average SAT score | 0 |  |
| Figure 17.4 | Clustering can be handled by clustered standard errors or by weighting class means | either approach | both give 159.884 |  | 1 |
| Figure 17.5 | N |  | 100 |  |  |
| Figure 17.5 | The vertical scale of both facets is the same but their range is not | the same scale but not the same range | 10 units in each facet; 0 to 10 against -5 to 5 |  | 1 |
| Figure 17.5 | lm_lin is equivalent to interacting the covariate with the treatment indicator | equivalent | 1.9796 |  | 1 |
| Figure 17.5 | The residual on residual slope equals the multiple regression estimate | exactly equal | 1.9796 |  | 1 |
| Figure 17.5 | Vertical axis label describes the raw scale as a seven point Likert | 7-point Likert | 100 distinct values over 0.6 to 8.7 | 0 |  |
| Figure 17.6 | N |  | 1189 |  |  |
| Figure 17.6 | Negative effects at low covariate values and positive effects at high ones | negative at low and positive at high | -3.64 at X = -2 and 4.40 at X = 2 |  | 1 |
| Figure 17.6 | The linear model fits worst at low covariate values | does not fit well, especially at low values | mean residual 10.16 in the lowest bin |  | 1 |
| Figure 17.7 | N |  | 600 |  |  |
| Figure 17.7 | N per assigned arm |  | 300 |  |  |
| Figure 17.7 | Noncompliance is two-sided | two-sided | treatment arm 0.667 and control arm 0.167 |  | 1 |
| Figure 17.8 | N |  | 200 |  |  |
| Figure 17.8 | N missing an outcome |  | 19 |  |  |
| Figure 17.8 | Outcome on a seven point Likert scale | seven point Likert | 2 to 7 | 1 |  |
| Figure 17.8 | Lower bound group means are very similar and the worst case effect is near zero | very similar and close to zero | 4.35 and 4.35, a difference of 0.00 |  | 1 |
| Figure 17.8 | Upper bound effect is close to a full scale point | close to a full-scale point | 1.14 |  | 1 |
| Figure 17.8 | The ATE lies somewhere between zero and one | zero and one | 0.00 to 1.14 |  |  |

Ground truth: what the chapter states against what the deposited data
and scripts produce. Both verdict columns blank means the chapter does
not state the quantity.

Of the 40 recorded claims, 27 state something the chapter can be held
to. 21 hold and 6 do not. Every one of the eight published figures
carries at least one row.

## The transposed neighborhoods

The chapter’s discussion of Figure 17.3 reads:

> In Figure 17.3a, we facet by block. Within each facet, the groups that
> are compared are formed by random assignment: we see small effects of
> treatment in neighborhood 1 and large negative effects of treatment in
> neighborhood 2.

| Neighborhood | Residents | Pr(treated) | Effect | SE   | p     |
|-------------:|:----------|:------------|:-------|:-----|:------|
|            1 | 50        | 0.50        | -4.36  | 0.79 | 0.000 |
|            2 | 100       | 0.25        | 0.25   | 0.50 | 0.611 |

Block-level treatment effects in the deposited blocked dataset.

The large negative effect is in neighborhood 1 and the small one is in
neighborhood 2. Three independent sources agree: the deposited dataset,
the deposited generator, whose potential outcomes are written
`-4 * Z * (neighborhood == 1)`, and the published figure itself, whose
left facet is labelled “Neighborhood 1” and shows a drop from 10.5 to
6.2. Only the sentence on the facing page has them the other way round.
Nothing in the code needs changing and the maintained rewrite reproduces
the figure as published; the error is in the prose.

## The blocks that are called schools

The very next sentence reads:

> By contrast, in Figure 17.3b, we facet by randomly assigned group, so
> we compare across schools and within treatment group.

There are no schools in this design. The blocked example has 2
neighborhoods of 50 and 100 residents, and the horizontal axis of the
published Figure 17.3b is labelled “Neighborhood 1” and “Neighborhood
2”. The chapter’s classroom example is the cluster-randomized experiment
two sections later. The word is a slip in a paragraph that otherwise
names the neighborhoods correctly four times, but it names a design
feature the example does not have, which is why it belongs with the
other corrections rather than in a list of typographical points.

The published panel (b) is also where the deposit falls short of the
book a second time. `blocked.R` abbreviates the horizontal axis labels
of that panel to `N/hood 1` and `N/hood 2` while writing panel (a)’s in
full, so running the deposited script does not produce the published
labels. Nothing published is wrong here; as with the Figure 17.4 panel
(b) label below, the published figure is the better one.

## The 400 to 1600 point scale

The clustered example describes an outcome “measured on a 400-1600-point
scale”, and both panels of Figure 17.4 set their limits accordingly. The
simulated outcomes run from 217 to 1720, so 8 of the 441 students sit
outside the stated scale and are clipped out of the figure by
`coord_cartesian()`. The design draws a classroom shock at
`rnorm(30, 1000, 100)` and a student shock at `rnorm(n, sd = 175)`,
which puts about two percent of the distribution outside the range on
arithmetic alone. The clipping is the chapter’s own choice and the
rewrite keeps it; the mismatch is between the data and the sentence that
describes them.

## The Likert scale that is not one

The vertical axis of Figure 17.5 reads
`Outcome variable (raw scale is 7-point Likert)`. The outcome it plots
is not a Likert item and is not seven-point. It takes 100 distinct
values across 100 units, running from 0.6 to 8.7, and 5 of them fall
outside the 1 to 7 range a seven-point item allows. The generator draws
it as `Y ~ 0.5 * Z + 1.0 * X + 0.5 * Z * X + U` with a normal covariate
and a normal disturbance, and the unadjusted facet’s own axis runs from
0 to 10, which is the shape of a continuous variable rather than an
ordinal one. The chapter’s only Likert outcome is Figure 17.8’s
attrition example, which is genuinely seven-point.

`covariate_adjustment.R` writes this label, so the chapter printed what
its own code gave it, and the maintained rewrite keeps the label as
deposited rather than silently improving it. Nothing plotted in the
figure is affected: the correction is to the axis title alone.

## The relabelled panel

`clustered.R` gives both panels the y-axis label
`Outcome variable: Classroom Average SAT score`. Panel (b) plots
students rather than classes, and the published panel (b) is labelled
`Outcome variable: SAT score`. The book restyled every axis label in the
chapter to sentence case, so some difference was expected; what makes
this one substantive is that the restyling also dropped “classroom
average”, which is a correction rather than a matter of house style. The
published figure is the better one and the deposited code does not
produce it.

# The extraction and the two instruments

A table of claims can only be as complete as the reading behind it, and
a table built up figure by figure has no way to notice a sentence nobody
thought to check. So the chapter was read a second time as an inventory
rather than as an argument: every numeral in the body text, every number
spelled out in words, and every quantity a figure asserts without
printing it, each recorded with where it appears and what kind of claim
it is. That inventory is `ground_truth/published_claims.csv`, 65 rows,
hand-reviewed and committed.

| Claim type   | Recomputed | Rows |
|:-------------|:-----------|-----:|
| definitional | No         |    5 |
| definitional | Yes        |   21 |
| descriptive  | Yes        |   17 |
| structural   | No         |   13 |
| structural   | Yes        |    3 |
| transcribed  | No         |    6 |

The extraction. A claim is recomputed when the pipeline can reach the
quantity; the rest are verified where they are used, or belong to a
cited source rather than to this chapter.

No row is typed `pipeline`, and that is the finding rather than an
oversight: the chapter states no estimate anywhere. What it does state
is design parameters, response scales, axis labels and claims about the
shape of its own results, so a check on this chapter is mostly a check
on whether the simulated data behind a figure are what the surrounding
sentence says they are. Three of the four errata came out of this pass,
and only one of them, the transposed neighborhoods, was reachable by
asking what a figure plots.

Six rows are `transcribed`: Fisher’s eight cups of tea and the four
assigned to milk first, the seventy ways of allocating them, the MIDA
framework’s four elements, Healy’s three dimensions of criticism, and
the differencing intuition from Gerber and Green. These belong to the
works the chapter cites, cannot drift, and were checked once against
those works.

`maintained/in_text_claims.R` is the second instrument. It carries 41
blocks, one for each extraction row the pipeline can reach: the
published sentence verbatim in a comment, then code that reads
`maintained/output/` and prints the quantity in the chapter’s own units,
labelled. It recomputes nothing. Estimation happens once, in the figure
scripts; only derivation happens twice, and it takes a different route
each time. The chapter’s two group sizes, for instance, are read out of
the deposited dataset by `text_design_parameters.R` and recovered here
from the residual degrees of freedom of the two regressions Figure 17.1
plots. The confidence level the chapter calls 95 per cent is recovered
from the interval half-widths rather than asserted. The imputation range
behind the extreme value bounds is recovered from the distance between
the two bounds and the number of missing outcomes, without knowing how
those missing outcomes split across arms.

`build_ground_truth.R` ends by running that file as a program, not by
reading it as text, since a block that errors or prints nothing would
satisfy any textual check. It captures what the file prints and requires
three things: that the number of claims printed equal the number of
extraction rows needing a block, and that the two sets of identifiers be
equal in both directions; that the chapter’s own values, transcribed
independently into the extraction and into the ground truth, agree
string for string; and that wherever both instruments arrive at a bare
number, they agree at the precision the extraction records for that
claim. The claims file is sourced into its own environment, because the
two files necessarily read the same outputs and name the same things,
and a bare `source()` would replace the build’s objects with the claims
file’s before any of those assertions ran.

Of the failing claims, four fail in a way that is the chapter’s own
rather than the deposit’s or the environment’s. Those are set out in
`coppock_2021_errata.pdf` at the root of this repository: the published
sentence, the corrected sentence with the changed token in bold, and the
evidence. Every number in that document is computed from
`maintained/output/` when it is rendered, including the numbers that
were not wrong. None of the four changes a conclusion of the chapter.

# Maintained rewrite

The rewrite lives in `maintained/`: seven figure scripts, four scripts
for quantities that belong to no single figure, a port of the data
generator, and a shared `helpers.R`. It is a translation, not a
reanalysis: every estimator and every plotted quantity is the one the
chapter used.

| Script | Produces |
|:---|:---|
| figure_1_two_arm_trial.R | Figure 17.1, both panels |
| figure_2_blocked_experiment.R | Figures 17.2 and 17.3, four panels from one dataset |
| figure_4_clustered_experiment.R | Figure 17.4, both panels |
| figure_5_covariate_adjustment.R | Figure 17.5 |
| figure_6_interaction_continuous.R | Figure 17.6, both panels |
| figure_7_noncompliance.R | Figure 17.7, both panels |
| figure_8_attrition.R | Figure 17.8 |
| text_design_parameters.R | The design facts the chapter states in prose |
| text_descriptive_claims.R | The three response scales the chapter states a bound for |
| in_text_claims.R | The claim-by-claim audit trail, and the coverage gate’s input |
| text_stata_equivalent.R | The deposited Stata file’s estimator, in R, against the R script’s |
| text_archive_agreement.R | Every figure’s estimates re-derived from the deposit, against the rewrite’s |
| make_datasets.R | The seven simulated datasets, regenerated under the current DeclareDesign API |

Maintained rewrite scripts.

Every figure script also writes the estimates it plots to a CSV, along
with the axis labels and panel ranges the chapter makes claims about.
The chapter prints no numbers, so an output folder of images alone would
make the reproduction diff a formality: a PDF differs on every run
because it records the time it was written. Writing the plotted
quantities out is what makes `git diff` on `maintained/output/` a check
rather than a ritual.

## Deprecated patterns replaced

| Original pattern | Replacement |
|:---|:---|
| `rm(list = ls())` | (omitted) |
| `library()` in each analysis script | `source(here::here("maintained", "helpers.R"))` |
| relative paths to the working directory | `here::here()` |
| `do(tidy(lm_robust(..., data = .)))` | `reframe(tidy(lm_robust(..., data = pick(everything()))))` |
| `gather()` / `spread()` | `pivot_longer()` / `pivot_wider()` |
| `geom_errorbar(width = 0)` | `geom_linerange()` |
| `data.frame()` for small hand-built frames | `tibble()` |
| `summary() &#124;> as.data.frame()` | `as_tibble()` |
| `declare_population()` / `declare_assignment(m = )` | `declare_model()` / `declare_assignment(Z = complete_ra(...))` |
| figures written into the archive directory | `ggsave()` to `maintained/output/` |
| magrittr pipe | native pipe |

Deprecated patterns and their replacements in the maintained rewrite.

One substantive correction was made. The archive’s `interaction.R`
writes `geom_errorbar(aes(ymax = lower, ymin = upper), width = 0)` with
the bounds transposed. Because `width = 0` draws a bare segment with no
caps, the transposition is invisible in the output, and the rewrite
writes them the right way round. No published value changes.

## Three things left alone

The `coord_cartesian()` limits that clip eight clustered observations
are the chapter’s, and they are kept. Widening them would produce a
figure the book does not contain.

`figure_5_covariate_adjustment.R` residualises with `lm()` and then
plots, exactly as the deposit does, rather than reading the coefficients
off `lm_lin()`. The archive prints three estimators side by side under
the comment “standard errors v. slightly off; point estimates OK”, and
the rewrite writes those three out instead of printing them into a
console that no longer exists. Both halves of the comment hold: the
point estimates agree to fifteen digits at 1.9796, which is what the
chapter says they should do, and the standard errors are 0.09486 against
0.09398.

The unseeded `position_jitter()` calls are seeded at 42. That moves the
point clouds slightly relative to the published figures and is the only
respect in which any panel differs from the book beyond the axis label
discussed above. Seeding a jitter changes an irreproducible figure into
a reproducible one; it does not change what the figure shows.

# The rewrite against the archive

The chapter states no estimates, so a claim that the rewrite reproduces
the archive cannot rest on comparing published numbers.
`text_archive_agreement.R` settles it directly: it re-derives every
estimate behind the eight figures from the deposited data, using the
model specifications the deposited scripts use, and compares the result
against what the figure scripts wrote. The reshaping and labelling
deliberately differ from the deposit, because that is where a
faithful-looking port goes wrong; the model calls do not.

| Figure | Quantity | Values | Largest absolute difference |
|:---|:---|---:|:---|
| Figure 17.1 | Group means and intervals | 8 | 5.55e-17 |
| Figure 17.2 | Weighted and unweighted group means | 16 | 5.55e-17 |
| Figure 17.3 | Group means within each facet | 32 | 0.00e+00 |
| Figure 17.4 | Clustered and unclustered group means | 16 | 2.27e-13 |
| Figure 17.5 | Three covariate adjustment estimators | 12 | 2.22e-16 |
| Figure 17.6 | CATE across the covariate grid | 51 | 4.44e-16 |
| Figure 17.7 | Group means by assigned group | 16 | 2.78e-17 |
| Figure 17.8 | Bounded group means | 16 | 4.44e-16 |

The rewrite’s plotted estimates against the same quantities re-derived
from the deposit.

All 167 values agree to within 2.3e-13, which is floating point noise.
The deposited scripts’ own `do(tidy(...))` idiom returns the same
numbers. The ground truth builder asserts this agreement and stops if it
ever fails, which is why its archive and rewrite columns carry the same
value.

# The data generator under the current API

`maintained/make_datasets.R` declares the same seven designs using
`declare_model()` and the current assignment functions, seeds at 343 as
the archive does, and compares every regenerated column against the
deposited file.

| Dataset       | Columns | Identical | Columns that differ |
|:--------------|--------:|----------:|:--------------------|
| attrition     |      10 |         7 | Z, R, Y             |
| blocked       |       9 |         9 | none                |
| clustered     |      11 |         8 | Z, Y, condition     |
| covariate     |       9 |         6 | Z, Y, condition     |
| interaction   |      10 |         7 | Z, Y, condition     |
| noncompliance |       3 |         3 | none                |
| two_arm       |       8 |         8 | none                |

The current-API port against the deposited simulated data.

48 of the 60 regenerated columns come back identical to the deposit.
Every fabricated covariate and every potential outcome reproduces, in
all seven datasets. What differs is the realised assignment vector in
four of them, and the columns revealed through it. The pattern is
specific: `complete_ra(N, m = 100)` and `block_ra(blocks, m = 25)`
return the assignments the deposit records, while `complete_ra(N)` with
no `m` and `cluster_ra(clusters)` do not. The drift is in `randomizr`,
not in R’s sampler. Running the repaired generator under
`RNGkind(sample.kind = "Rounding")`, which restores the pre-3.6
`sample()`, makes matters worse rather than better: the blocked dataset
stops reproducing too. Since the analysis scripts read the deposited
files rather than these, no published figure depends on the difference.

Two details of the port are worth recording because they are the reason
it reproduces anything at all.

The reveal steps use `declare_reveal()` rather than
`declare_measurement()` with `reveal_outcomes()`. The two are equivalent
here, but the second draws two extra random numbers per design, which
moves every design after the first off the archive’s random number
stream and makes the comparison above uniformly negative for reasons
that have nothing to do with the deposit.

The generator’s blocked section prints two regressions before moving on,
and the inverse-probability-weighted one draws two random numbers on its
way through `estimatr`. Those draws sit in the stream between the
blocked design and every design declared after it, so the deposited data
depend on a diagnostic call that computes nothing the file needs. The
port keeps the pair and writes their output out rather than printing it.
This is a real property of the deposit worth stating plainly: five of
its seven datasets cannot be regenerated by any script that tidies away
a dead `print()`.

# Figure verification

<img src="maintained/output/figure_2_blocked_good.png"
style="width:100.0%"
alt="Figures 17.2 and 17.3 as reproduced by the maintained rewrite: the blocked experiment weighted and unweighted, then faceted by block and by condition." />

<img src="maintained/output/figure_3_blocked_facets_good.png"
style="width:100.0%"
alt="Figures 17.2 and 17.3 as reproduced by the maintained rewrite: the blocked experiment weighted and unweighted, then faceted by block and by condition." />

<img src="maintained/output/figure_4_clustered_good.png"
style="width:100.0%"
alt="Figure 17.4 as reproduced by the maintained rewrite: cluster means with cluster-robust intervals, then students with intervals that ignore the clustering. Both carry the deposited script’s axis label; the published panel (b) does not." />

<img src="maintained/output/figure_4_clustered_bad.png"
style="width:100.0%"
alt="Figure 17.4 as reproduced by the maintained rewrite: cluster means with cluster-robust intervals, then students with intervals that ignore the clustering. Both carry the deposited script’s axis label; the published panel (b) does not." />

All fourteen panels reproduce the published figures. Beyond the seeded
jitter, the two axis labels the book improved on, and the Figure 17.5
axis label that neither the book nor the deposit got right, no panel
differs from its published counterpart.

| Figure | Quantity | Value in the deposited data |
|:---|:---|:---|
| Figure 17.1 | N | 500 |
| Figure 17.1 | N treated | 100 |
| Figure 17.1 | Number of distinct outcome values | 2 |
| Figure 17.1 | Outcome range | 0 to 1 |
| Figures 17.2 and 17.3 | N | 150 |
| Figures 17.2 and 17.3 | Number of neighborhoods | 2 |
| Figures 17.2 and 17.3 | N in neighborhood 1 | 50 |
| Figures 17.2 and 17.3 | N in neighborhood 2 | 100 |
| Figures 17.2 and 17.3 | N treated per neighborhood | 25 |
| Figures 17.2 and 17.3 | Outcome range | 1 to 16 |
| Figures 17.2 and 17.3 | Number of non-integer outcomes | 0 |
| Figure 17.4 | N students | 441 |
| Figure 17.4 | Number of classes | 30 |
| Figure 17.4 | Smallest class | 10 |
| Figure 17.4 | Largest class | 20 |
| Figure 17.4 | Outcome range | 217 to 1720 |
| Figure 17.5 | N | 100 |
| Figure 17.5 | Number of distinct outcome values | 100 |
| Figure 17.5 | Outcome range | 0.6 to 8.7 |
| Figure 17.6 | N | 1189 |
| Figure 17.7 | N | 600 |
| Figure 17.7 | N assigned to treatment | 300 |
| Figure 17.7 | N assigned to control | 300 |
| Figure 17.8 | N | 200 |
| Figure 17.8 | N missing an outcome | 19 |
| Figure 17.8 | Outcome range | 2 to 7 |

Design parameters read off the deposited datasets by the in-text script.

# Maintained rewrite verification

| Location | Claim | Chapter | Rewrite | Match | Locus |
|:---|:---|:---|:---|:---|:---|
| Figure 17.1 | N | 500 | 500 | 1 |  |
| Figure 17.1 | N treated | 100 | 100 | 1 |  |
| Figure 17.1 | Control group mean |  | 0.515 |  |  |
| Figure 17.1 | Treatment group mean |  | 0.610 |  |  |
| Figure 17.1 | The deposited Stata file draws something similar to the R figure |  | intervals differ in width by at most 0.0024 |  |  |
| Figures 17.2 and 17.3 | Number of neighborhoods | 2 | 2 | 1 |  |
| Figures 17.2 and 17.3 | N in neighborhood 1 | 50 | 50 | 1 |  |
| Figures 17.2 and 17.3 | N in neighborhood 2 | 100 | 100 | 1 |  |
| Figures 17.2 and 17.3 | N treated per neighborhood | 25 | 25 | 1 |  |
| Figures 17.2 and 17.3 | N total |  | 150 |  |  |
| Figure 17.2 | Probability of treatment is higher in the first neighborhood | higher in the first | 0.50 against 0.25 |  |  |
| Figure 17.2 | Inverse probability weighted ATE is large and negative | large and negative | -1.284 |  |  |
| Figure 17.2 | Unweighted ATE is close to zero | close to zero | -0.700 |  |  |
| Figure 17.3 | Small effect in neighborhood 1 and a large negative effect in neighborhood 2 | small in 1 and large negative in 2 | neighborhood 1 = -4.36; neighborhood 2 = 0.25 |  | paper_internal |
| Figure 17.3 | Panel b compares across schools | schools | N/hood 1, N/hood 2 |  | paper_internal |
| Figure 17.3 | Panel b horizontal axis labels | Neighborhood 1 | N/hood 1, N/hood 2 | 0 | archive |
| Figure 17.4 | Number of classes |  | 30 |  |  |
| Figure 17.4 | N students |  | 441 |  |  |
| Figure 17.4 | Outcome measured on a 400 to 1600 point scale | 400 to 1600 | 217 to 1720 | 0 | paper_internal |
| Figure 17.4 | Group means are the same in both panels | the same in both panels | 957.9 and 1117.8 in both |  |  |
| Figure 17.4 | Panel b confidence intervals ignore clustering and are narrower | narrower | widths 59 and 51 against 174 and 107 |  |  |
| Figure 17.4 | Panel b vertical axis label | Outcome variable: SAT score | Outcome variable: Classroom Average SAT score | 0 | archive |
| Figure 17.4 | Clustering can be handled by clustered standard errors or by weighting class means | either approach | both give 159.884 |  |  |
| Figure 17.5 | N |  | 100 |  |  |
| Figure 17.5 | The vertical scale of both facets is the same but their range is not | the same scale but not the same range | 10 units in each facet; 0 to 10 against -5 to 5 |  |  |
| Figure 17.5 | lm_lin is equivalent to interacting the covariate with the treatment indicator | equivalent | 1.9796 |  |  |
| Figure 17.5 | The residual on residual slope equals the multiple regression estimate | exactly equal | 1.9796 |  |  |
| Figure 17.5 | Vertical axis label describes the raw scale as a seven point Likert | 7-point Likert | 100 distinct values over 0.6 to 8.7 | 0 | paper_internal |
| Figure 17.6 | N |  | 1189 |  |  |
| Figure 17.6 | Negative effects at low covariate values and positive effects at high ones | negative at low and positive at high | -3.64 at X = -2 and 4.40 at X = 2 |  |  |
| Figure 17.6 | The linear model fits worst at low covariate values | does not fit well, especially at low values | mean residual 10.16 in the lowest bin |  |  |
| Figure 17.7 | N |  | 600 |  |  |
| Figure 17.7 | N per assigned arm |  | 300 |  |  |
| Figure 17.7 | Noncompliance is two-sided | two-sided | treatment arm 0.667 and control arm 0.167 |  |  |
| Figure 17.8 | N |  | 200 |  |  |
| Figure 17.8 | N missing an outcome |  | 19 |  |  |
| Figure 17.8 | Outcome on a seven point Likert scale | seven point Likert | 2 to 7 | 1 |  |
| Figure 17.8 | Lower bound group means are very similar and the worst case effect is near zero | very similar and close to zero | 4.35 and 4.35, a difference of 0.00 |  |  |
| Figure 17.8 | Upper bound effect is close to a full scale point | close to a full-scale point | 1.14 |  |  |
| Figure 17.8 | The ATE lies somewhere between zero and one | zero and one | 0.00 to 1.14 |  |  |

Maintained rewrite verification: what the chapter states against what
the rewrite produces.

The rewrite and the deposited data give the same answer to every claim,
which they should: both read the same seven deposited CSV files, and the
previous section measures how closely. Two of the three failures sit in
the chapter’s prose and no rewrite of the code can repair them; the
third sits in the deposited script, where the published figure carries a
label the code does not produce.

# R environment

| Item      | Value                  |
|:----------|:-----------------------|
| R version | 4.6.0                  |
| Platform  | aarch64-apple-darwin23 |
| Date run  | 2026-08-03             |

| Package       | Version |
|:--------------|:--------|
| tidyverse     | 2.0.0   |
| dplyr         | 1.2.1   |
| ggplot2       | 4.0.3   |
| tidyr         | 1.3.2   |
| estimatr      | 1.0.6   |
| broom         | 1.0.13  |
| margins       | 0.3.28  |
| DeclareDesign | 1.1.1   |
| randomizr     | 1.0.1   |
| fabricatr     | 1.0.2   |
| here          | 1.0.2   |

Package versions used for the run behind this report.
