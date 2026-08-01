# coppock_2020/download_original.R
# Output: original/ (the deposited replication archive, not redistributed in this repo)
# Depends on: original_manifest.csv
# Description: Fetch the deposited archive from Harvard Dataverse and verify every
#   file. Run this once before running anything in maintained/. Re-running is free:
#   files already present with the right checksum are not downloaded again.
#
#   The manifest carries two checksums per file. md5_served is the MD5 of the bytes
#   Dataverse returns for `?format=original`, which is what this code was written
#   against. md5_published is the checksum Dataverse displays. Here all seventeen
#   agree, but they do not always: other deposits in this program carry published
#   checksums that verify neither the original nor the derived tabular file, so
#   verification runs against md5_served and any disagreement is reported.
#
#   The seven data files are ingested, so Dataverse also serves a derived .tab
#   version of each. `?format=original` asks for the deposited .csv instead, which
#   is what the analysis scripts read.
#
#   Every file in this deposit sits under a `replication_archive` directory label,
#   which the manifest carries as part of each path and this script recreates. A
#   flat manifest would make the check for files the deposit does not contain pass
#   without testing anything.

library(tidyverse)
library(here)

here::i_am("download_original.R")

dataset_doi <- "doi:10.7910/DVN/VE6VSR"
base_url <- "https://dataverse.harvard.edu/api/access/datafile"

# Manifest ----
manifest <- read_csv(here::here("original_manifest.csv"), show_col_types = FALSE)

walk(
  unique(dirname(here::here("original", manifest$file))),
  function(d) dir.create(d, showWarnings = FALSE, recursive = TRUE)
)

# Download what is missing or wrong ----
planned <- manifest |>
  mutate(
    path = here::here("original", file),
    url = str_glue("{base_url}/{dataverse_file_id}?format=original"),
    md5_local = unname(tools::md5sum(path)),
    needs_download = is.na(md5_local) | md5_local != md5_served
  )

walk2(
  planned$url[planned$needs_download],
  planned$path[planned$needs_download],
  function(url, path) download.file(url, destfile = path, mode = "wb", quiet = TRUE)
)

print(str_glue("Downloaded {sum(planned$needs_download)} of {nrow(planned)} files; ",
               "{sum(!planned$needs_download)} already present and verified."))

# Verify ----
verified <- planned |>
  mutate(
    md5_downloaded = unname(tools::md5sum(path)),
    match = md5_downloaded == md5_served,
    published_agrees = md5_served == md5_published
  ) |>
  select(file, bytes, md5_served, md5_downloaded, match, published_agrees)

print(verified, n = nrow(verified))

if (!all(verified$match)) {
  stop("Checksum mismatch: the downloaded archive does not match what Dataverse served when this code was written.")
}

# The archive's own scripts write their figures into the directory they run from,
# so a copy of original/ that has ever been used as a working directory will carry
# files the deposit does not. Nothing in maintained/ writes here, and this check
# says so out loud.
unexpected <- setdiff(
  list.files(here::here("original"), recursive = TRUE),
  manifest$file
)
if (length(unexpected) > 0) {
  print(str_glue("original/ holds {length(unexpected)} file(s) the deposit does not: ",
                 "{paste(unexpected, collapse = ', ')}"))
}

print(str_glue("All {nrow(verified)} files match. ",
               "{sum(!verified$published_agrees)} carry a published checksum that disagrees."))
print(str_glue("Archive: {dataset_doi}"))
