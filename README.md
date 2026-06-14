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

## Source

Original survey: <https://www.metro.sp.gov.br/pesquisa-od/>. The raw microdata
is **not** redistributed here — see `read_od_microdata()` in `odsp`.
