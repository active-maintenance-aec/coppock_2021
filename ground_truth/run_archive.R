# coppock_2021/ground_truth/run_archive.R
# Output: ground_truth/archive_run_status.csv, ground_truth/archive_overwrites.csv
# Depends on: original/ (run download_original.R first), original_manifest.csv
# Description: Runs every deposited script in a clean R process, twice, and records
#   what happened to each one and what each pass left behind.
#
#   The deposit is flat: eight R scripts, one Stata file, seven simulated datasets
#   and a README, all in one directory. Every script reads its CSV from the working
#   directory and writes its figures back into it, so the deposit is copied to a
#   temporary directory before anything runs and original/ is never a working
#   directory.
#
#   The stripped pass gives the deposit its code alone. All seven CSVs are written
#   by make_datasets.R, so none of them is raw data, and a script that passes here
#   passes on the deposit's own work rather than on a dataset it ships. The
#   as_shipped pass restores the seven CSVs. In both passes the generator runs
#   first, then the seven figure scripts.
#
#   two_arm_trial.do is recorded as not run in both passes: there is no Stata here,
#   and maintained/text_stata_equivalent.R reproduces its nine lines in R instead.
#
#   This script is not part of run_all.R. It runs the deposit as deposited, so what
#   it records depends on the installed versions of the packages the deposit names,
#   and its output is committed so the report can read it without rerunning it.

library(here)
library(tidyverse)

here::i_am("ground_truth/run_archive.R")

deposit <- here::here("original", "replication_archive")

manifest <- read_csv(here::here("original_manifest.csv"), show_col_types = FALSE)
deposited_files <- basename(manifest$file)

generator <- "make_datasets.R"
figure_scripts <- c("two_arm_trial.R", "blocked.R", "clustered.R",
                    "covariate_adjustment.R", "noncompliance.R", "interaction.R",
                    "attrition.R")
stata_file <- "two_arm_trial.do"
generated_data <- str_subset(deposited_files, "_simulated_data\\.csv$")

stopifnot(
  length(generated_data) == 7,
  all(c(generator, figure_scripts, stata_file) %in% deposited_files)
)

# Stage a pass ----
stage <- function(label, include_data) {
  # normalizePath, because tempdir() resolves through /private on macOS, and the
  # error scrubbing below has to see the path the scripts see.
  root <- file.path(normalizePath(tempdir(), winslash = "/", mustWork = FALSE),
                    paste0("coppock_2021_", label))
  unlink(root, recursive = TRUE)
  dir.create(root, recursive = TRUE)
  shipped <- if (include_data) deposited_files else setdiff(deposited_files, generated_data)
  stopifnot(all(file.copy(file.path(deposit, shipped), root)))
  root
}

# Run one script in its own process ----
# Every deposited script opens with rm(list = ls()), and each has to be seen from a
# clean session to tell a missing declaration from a package that is merely attached.
run_one <- function(root, script) {
  log_file <- file.path(dirname(root), paste0(basename(root), "_", script, ".log"))
  expr <- paste0("setwd(", deparse(root), "); source(", deparse(script), ")")
  status <- system2(file.path(R.home("bin"), "Rscript"), args = c("-e", shQuote(expr)),
                    stdout = log_file, stderr = log_file)
  lines <- read_lines(log_file)
  first <- which(str_starts(lines, "Error"))
  stopped_at <- if (length(first) == 0) {
    NA_character_
  } else {
    lines[first[1]:min(first[1] + 2, length(lines))] |>
      discard(\(x) str_detect(x, "Execution halted")) |>
      str_c(collapse = " ") |>
      str_replace_all(fixed(root), "<archive>") |>
      str_replace_all(fixed(str_remove(root, "^/private")), "<archive>") |>
      str_squish() |>
      str_trunc(220)
  }
  tibble(script = script, exit_status = status, stopped_at = stopped_at)
}

# What a pass leaves behind ----
# A file the pass found and still holds is overwritten if its bytes changed. A
# generated dataset the stripped pass withheld is rebuilt if the generator wrote it,
# withheld otherwise. Anything else the pass wrote is added.
run_pass <- function(label, include_data) {
  root <- stage(label, include_data)
  before <- tools::md5sum(file.path(root, list.files(root)))
  names(before) <- basename(names(before))

  status <- map(c(generator, figure_scripts), \(s) run_one(root, s)) |>
    list_rbind()

  after_files <- list.files(root)
  after <- tools::md5sum(file.path(root, after_files))
  names(after) <- after_files

  matches_deposit <- function(file) {
    unname(after[file] == tools::md5sum(file.path(deposit, file)))
  }

  touched <- tibble(file = union(names(before), after_files)) |>
    mutate(
      present_before = file %in% names(before),
      present_after = file %in% after_files,
      changed = present_before & present_after & before[file] != after[file]
    )

  overwrites <- bind_rows(
    touched |>
      filter(changed) |>
      transmute(file, effect = "overwritten",
                matches_deposit = map_lgl(file, matches_deposit)),
    touched |>
      filter(!present_before, present_after, !file %in% generated_data) |>
      transmute(file, effect = "added", matches_deposit = NA),
    tibble(file = if (include_data) character() else generated_data) |>
      mutate(
        effect = if_else(file %in% after_files, "rebuilt", "withheld"),
        matches_deposit = if_else(effect == "rebuilt", map_lgl(file, matches_deposit), NA)
      ),
    touched |>
      filter(present_before, !present_after) |>
      transmute(file, effect = "deleted", matches_deposit = NA)
  )

  list(status = status, overwrites = overwrites)
}

stripped <- run_pass("stripped", include_data = FALSE)
as_shipped <- run_pass("as_shipped", include_data = TRUE)

# The two archive tables, in the canonical long form ----
roles <- tibble(
  script = c(generator, figure_scripts, stata_file),
  role = c("cleaning", rep("analysis", length(figure_scripts)), "analysis")
)

not_run <- tibble(
  script = stata_file, exit_status = NA_integer_,
  stopped_at = "No Stata here; maintained/text_stata_equivalent.R reproduces its arithmetic in R"
)

run_status <- bind_rows(
  stripped = bind_rows(stripped$status, not_run),
  as_shipped = bind_rows(as_shipped$status, not_run),
  .id = "pass"
) |>
  left_join(roles, by = "script") |>
  transmute(
    pass, script, role, exit_status,
    outcome = case_when(
      is.na(exit_status) ~ "not_run",
      exit_status == 0 ~ "clean",
      .default = "error"
    ),
    stopped_at
  )

overwrites <- bind_rows(
  stripped = stripped$overwrites,
  as_shipped = as_shipped$overwrites,
  .id = "pass"
) |>
  arrange(pass != "stripped", effect, file)

stopifnot(
  nrow(run_status) == 2 * nrow(roles),
  !anyNA(run_status$role),
  all(run_status$outcome %in% c("clean", "error", "timeout", "not_run")),
  all(overwrites$effect %in% c("added", "overwritten", "rebuilt", "withheld", "deleted"))
)

write_csv(run_status, here::here("ground_truth", "archive_run_status.csv"))
write_csv(overwrites, here::here("ground_truth", "archive_overwrites.csv"))

print(run_status, n = nrow(run_status), width = 200)
print(overwrites, n = nrow(overwrites))
