# Regenerate the integrity manifest -------------------------------------------
# Prints the md5 + byte size for each built artifact in data/. Run after
# build/run.R, then paste the result into od_data_manifest() in odsp's R/cache.R.
# Keep data/, this manifest, and the matching GitHub Release assets in sync — the
# md5s must match or odsp's downloads fail their integrity check.

files <- c(
  "geo_pod.rds",
  "tbl_pod.csv.gz",
  "geo_pod_travel.rds",
  "tbl_pod_travel.rds"
)

paths <- file.path("data", files)
stopifnot(all(file.exists(paths)))

entries <- vapply(
  seq_along(files),
  function(i) {
    sprintf(
      '    "%s" = list(md5 = "%s", bytes = %dL),',
      files[i],
      unname(tools::md5sum(paths[i])),
      as.integer(file.info(paths[i])$size)
    )
  },
  character(1)
)

cat("od_data_manifest() entries:\n\n", paste(entries, collapse = "\n"), "\n", sep = "")
