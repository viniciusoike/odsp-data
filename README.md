# odsp-data

Data assets for the [`odsp`](https://github.com/viniciusoike/odsp) R package.

This repository hosts the pre-built data files for the **São Paulo
Origin-Destination Survey** (*Pesquisa Origem-Destino* / POD, conducted by
Metrô-SP). The `odsp` package downloads these on demand and caches them locally
— they are intentionally kept out of the R package tarball.

## Releases

- **`data-2017`** — curated tables and travel tables for the 2017 survey:
  - `tbl_pod.csv.gz` — zone-level aggregated tables
  - `tbl_pod_travel.rds` — travel tables (motive, mode, OD, time)
  - `geo_pod.rds` — zone-level tables joined to geometry (`sf`)
  - `geo_pod_travel.rds` — travel tables joined to geometry (`sf`)

Future survey waves will be published as additional releases.

## Layout

- `data/` — the built artifacts (also published as the Release assets above).
- `build/` — the pipeline that produces them from the raw Metrô-SP database:
  - `build_pod_tables.R`, `build_pod_travel_table.R` — the table builders
    (`import_pod_*`, `pod_table_*`, `connect_pod_db`). These were relocated out
    of the `odsp` package so it stays a pure serving layer.
  - `run.R` — orchestrates the build and writes `data/`.
  - `manifest.R` — prints the integrity manifest to paste into `odsp`.

## Rebuilding (once per survey wave)

1. Place the raw SQLite database at `data-raw/DB_ORIGEM_DESTINO_SP` (not
   redistributed here).
2. From the repo root: `Rscript build/run.R` (needs `odsp` installed plus the
   build packages: DBI, RSQLite, dplyr, tidyr, purrr, stringr, janitor, sf,
   readr, vroom).
3. `Rscript build/manifest.R` and paste the output into `od_data_manifest()` in
   `odsp`'s `R/cache.R` — the md5s must match these files.
4. Upload the four `data/` files to a Release (`gh release create <tag>
   <files>`); the matching survey year gets its own tag.

## Source

Original survey: <https://www.metro.sp.gov.br/pesquisa-od/>. The raw microdata
is **not** redistributed here — see `read_od_microdata()` in `odsp`.
