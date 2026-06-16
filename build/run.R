# Build the POD table artifacts from the raw Metro-SP database.
# Run from the odsp-data repo root:  Rscript build/run.R
#
# Requires:
#   - the raw SQLite database at data-raw/DB_ORIGEM_DESTINO_SP (not in this repo)
#   - the odsp package installed (provides the bundled zones/cities/districts sf)
#   - build packages: DBI, RSQLite, dplyr, tidyr, purrr, stringr, janitor, sf,
#     readr, vroom
library(odsp)
library(vroom)

source("build/build_pod_tables.R")
source("build/build_pod_travel_table.R")

tbls_pod    <- import_pod_tables(cached = FALSE, tables = "all")
tbls_travel <- import_pod_travel_tables(cached = FALSE, tables = "all")
geo_pod     <- import_pod_tables(cached = FALSE, geo = TRUE, tables = "all")
geo_travel  <- import_pod_travel_tables(cached = FALSE, geo = TRUE, tables = "all")

# Export to tab-delimited gzip (vroom_write's default; odsp's reader auto-detects).
vroom::vroom_write(tbls_pod, "data/tbl_pod.csv.gz")
# Export to compressed RDS.
readr::write_rds(tbls_travel, "data/tbl_pod_travel.rds", compress = "gz")
readr::write_rds(geo_pod, "data/geo_pod.rds", compress = "gz")
readr::write_rds(geo_travel, "data/geo_pod_travel.rds", compress = "gz")

# Then refresh the integrity manifest and paste the result into od_data_manifest()
# in odsp's R/cache.R:
#   Rscript build/manifest.R
