#' Connect to the POD database
#'
#' Should be used together with the `pod_table_*` functions.
#'
#' @return A `SQLiteConnection` with the POD database
#' @keywords internal
#' @noRd
connect_pod_db <- function() {

  # Create a connection with the database
  DBI::dbConnect(
    RSQLite::SQLite(),
    dbname = file.path("data-raw", "DB_ORIGEM_DESTINO_SP")
  )

}

#' Replicate the POD tables
#'
#' Replicates the main POD tables and adds some auxiliary variables
#'
#' @param cached If `FALSE` creates the tables from scratch. If `TRUE` downloads
#' from Github repo.
#' @param geo If `TRUE` joins the table with the `zones` shape file.
#' @param tables String indicating which table to build. Options are `'demographic'`,
#' `'education'`, `'income'`, `'income_group'`, `'cars'`, `'jobs'`, `'jobs_sector'`,
#' or `'all'` (default).
#'
#' @return Either a `tibble` or a `sf` object with aggregated information for each
#' OD zone.
#' @keywords internal
#' @noRd
import_pod_tables <- function(cached = TRUE, geo = FALSE, tables = "all",
                              refresh = FALSE) {

  if (cached) {

    if (!identical(tables, "all")) {
      rlang::warn(
        "`tables` only applies when `cached = FALSE`; the full cached table is returned."
      )
    }

    if (geo) {

      dat <- readr::read_rds(od_cache_fetch("geo_pod.rds", refresh = refresh))
    } else {
      dat <- vroom::vroom(
        od_cache_fetch("tbl_pod.csv.gz", refresh = refresh),
        show_col_types = FALSE
      )
    }

    return(dat)

  }

  rlang::check_installed(
    c("DBI", "RSQLite", "tidyr", "purrr", "stringr", "janitor"),
    "to build the POD tables from the raw survey database."
  )

  # Subselect tables
  all_tables <- c(
    "overall", "demographic", "education", "income", "income_group",
    "cars", "jobs", "jobs_sector"
  )
  if (identical(tables, "all")) {
    name_tables <- all_tables
  } else {
    stopifnot(all(tables %in% all_tables))
    name_tables <- tables
  }

  # Create a connection with the database
  arquivo_db <- "data-raw/DB_ORIGEM_DESTINO_SP"
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = arquivo_db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Get all POD tables
  tbls <- suppressWarnings(lapply(name_tables, get_pod_table, con))

  # Join tables into a single table
  joined <- purrr::reduce(tbls, dplyr::full_join, by = "code_zone")
  # Multiplies share_* columns by 100
  out <- joined |>
    dplyr::mutate(dplyr::across(dplyr::contains("share"), ~.x * 100))

  # Joins the POD data to the zones shape file
  if (geo) {
    out <- dplyr::left_join(
      dplyr::select(zones, "code_zone"), out, by = "code_zone"
      )
    # Converts zero income zones to NA for better visualization
    out <- out |>
      dplyr::mutate(
        dplyr::across(dplyr::contains("income"), ~dplyr::na_if(.x, 0))
        )
  }

  out

}

#' @importFrom rlang .data
pod_table_overall <- function(con) {

  if (missing(con)) {
    con <- connect_pod_db()
  }

  overall <- dplyr::tbl(con, "dados_gerais_por_zona_de_pesquisa_2017")

  id_zones <- tidyr::as_tibble(sf::st_drop_geometry(zones))

  tab_overall <- overall |>
    dplyr::select(
      code_zone = .data$COD_ZONA,
      hh = .data$DOMICILIOS,
      fami = .data$FAMILIAS,
      pop = .data$POPULACAO,
      jobs = .data$EMPREGOS,
      cars = .data$AUTOMOVEIS
    ) |>
    dplyr::mutate(
      pop_household = .data$pop / .data$hh,
      car_rate = .data$cars / .data$fami
    ) |>
    dplyr::collect()

  tbl_overall <- id_zones |>
    dplyr::left_join(tab_overall, by = "code_zone") |>
    dplyr::mutate(
      pop_density = .data$pop / .data$area_ha,
      jobs_density = .data$jobs / .data$area_ha)

  return(tbl_overall)

}

#' @importFrom rlang .data
pod_table_demographic <- function(con) {

  if (missing(con)) {
    con <- connect_pod_db()
  }

  age <- dplyr::tbl(con, "populacao_por_faixa_etaria_e_zona_de_residencia_2017")

  new_names <- c(
    "code_zone" = "COD_ZONA",
    "age_group" = "FAIXA_ETARIA",
    "value" = "QTD"
  )

  age <- age |>
    dplyr::rename(dplyr::all_of(new_names)) |>
    dplyr::collect()

  df_label <- data.frame(
    age_group = unique(age$age_group),
    age_label = c(rep("young", 5), rep("young-adult", 2), rep("adult", 3), "elder"),
    age_label_ibge = c(rep("young", 4), rep("adult", 6), "elder")
  )

  tbl_age <- age |>
    dplyr::left_join(df_label, by = "age_group") |>
    dplyr::summarise(
      total = sum(.data$value), .by = c("code_zone", "age_label_ibge")
      ) |>
    dplyr::group_by(.data$code_zone) |>
    dplyr::mutate(share = .data$total / sum(.data$total)) |>
    dplyr::ungroup() |>
    tidyr::pivot_wider(
      names_from = "age_label_ibge",
      values_from = c("total", "share")
    ) |>
    dplyr::mutate(
      aging_index = .data$total_elder / .data$total_young,
      dry = .data$total_adult / .data$total_young,
      dre = .data$total_adult / .data$total_elder,
      dependency_ratio = .data$dry + .data$dre
    )

  return(tbl_age)

}

#' @importFrom rlang .data
pod_table_education <- function(con) {

  if (missing(con)) {
    con <- connect_pod_db()
  }

  educ <- dplyr::tbl(con, "populacao_por_grau_de_instrucao_e_zona_de_residencia_2017")

  new_names <- c(
    "code_zone" = "COD_ZONA",
    "educ_level" = "GRAU_INSTRUCAO",
    "value" = "QTD"
  )

  educ <- educ |>
    dplyr::rename(dplyr::all_of(new_names)) |>
    dplyr::collect() |>
    dplyr::mutate(educ_level = janitor::make_clean_names(.data$educ_level))

  df_label <- data.frame(
    educ_level = unique(educ$educ_level),
    educ_label = c(
      "Analfabetos ou Fund. Inc.", "Fundamental Comp.", "Fundamental Comp.",
      "M\u00e9dio Completo", "Superior Completo"
    ),
    educ_code = paste0("educ_", c("analf", "fund", "fund", "medio", "superior"))
  )

  tbl_educ <- educ |>
    dplyr::left_join(df_label, by = "educ_level") |>
    dplyr::summarise(total = sum(.data$value), .by = c("code_zone", "educ_code")) |>
    dplyr::group_by(.data$code_zone) |>
    dplyr::mutate(share = .data$total / sum(.data$total)) |>
    tidyr::pivot_wider(
      id_cols = "code_zone",
      names_from = "educ_code",
      values_from = c("total", "share")
    )

  return(tbl_educ)

}

#' @importFrom rlang .data
pod_table_income <- function(con) {

  income <- dplyr::tbl(con, "renda_total_renda_media_familiar_renda_per_capita_e_renda_mediana_familiar_por_zona_de_residencia_2017")

  new_names <- c(
    "code_zone" = "COD_ZONA",
    "income_group" = "TIPO_RENDA",
    "value" = "QTD"
  )

  tbl_income <- income |>
    dplyr::rename(dplyr::all_of(new_names)) |>
    dplyr::collect() |>
    dplyr::mutate(
      name = stringr::str_replace(.data$income_group, "Familiar...3", "income_avg"),
      name = stringr::str_replace(.data$name, "Familiar...5", "income_med"),
      name = stringr::str_replace(.data$name, "Per Capita", "income_pc"),
      name = factor(.data$name),
      value_ipc = .data$value * ipc_fipe_adjust
    ) |>
    tidyr::pivot_wider(
      id_cols = "code_zone",
      names_from = "name",
      values_from = "value_ipc"
    )

  return(tbl_income)

}

#' @importFrom rlang .data
pod_table_income_group <- function(con) {

  income <- dplyr::tbl(con, "populacao_por_faixa_de_renda_familiar_mensal_e_zona_de_residencia_2017")

  new_names <- c(
    "code_zone" = "COD_ZONA",
    "income_group" = "FAIXA_RENDA",
    "total" = "QTD"
  )

  income <- income |>
    dplyr::rename(dplyr::all_of(new_names)) |>
    dplyr::collect() |>
    dplyr::group_by(.data$code_zone) |>
    dplyr::mutate(share = .data$total / sum(.data$total)) |>
    dplyr::ungroup()

  df_label <- data.frame(
    income_group = unique(income$income_group),
    income_label_wage = paste0(c("Ate 2", "2 a 4", "4 a 8", "8 a 12", "Mais de 12"), " s.m."),
    income_label = c("low_income", "medium_low_income", "medium_income", "medium_high_income", "high_income")
  )

  income_group <- dplyr::left_join(income, df_label, by = "income_group")

  tbl_income_group <- income_group |>
    tidyr::pivot_wider(
      id_cols = "code_zone",
      names_from = "income_label",
      values_from = c("total", "share")
    )

  return(tbl_income_group)

}

#' @importFrom rlang .data
pod_table_cars <- function(con) {

  cars <- dplyr::tbl(con, "familias_por_numero_de_automoveis_particulares_e_zona_de_residencia_2017")

  new_names <- c(
    "code_zone" = "COD_ZONA",
    "n_cars" = "NUMERO_AUTOMOVEIS",
    "total" = "QTD"
  )

  cars <- cars |>
    dplyr::collect() |>
    dplyr::rename(dplyr::all_of(new_names))

  tbl_cars <- cars |>
    dplyr::filter(.data$n_cars != "...6") |>
    dplyr::group_by(.data$code_zone) |>
    dplyr::mutate(
      n_cars = dplyr::if_else(
        stringr::str_detect(.data$n_cars, "Nenhum"),
        0,
        as.numeric(stringr::str_sub(.data$n_cars, 1, 1))
      ),
      share = .data$total / sum(.data$total)
    ) |>
    dplyr::ungroup() |>
    tidyr::pivot_wider(
      id_cols = "code_zone",
      names_from = "n_cars",
      values_from = c("total", "share"),
      names_prefix = "car_"
    )

  return(tbl_cars)

}

#' @importFrom rlang .data
pod_table_jobs <- function(con) {

  jobs <- dplyr::tbl(con, "empregos_por_classe_de_atividade_e_zona_de_emprego_2017")

  new_names <- c(
    "code_zone" = "COD_ZONA",
    "jobs_group" = "EMPREGOS_POR_CLASSE_ATIVIDADE",
    "value" = "QTD"
  )

  # Rename variables and compute zone proportions #
  jobs <- jobs |>
    dplyr::rename(dplyr::all_of(new_names)) |>
    dplyr::collect()

  jobs <- jobs |>
    dplyr::mutate(value = as.numeric(.data$value)) |>
    dplyr::group_by(.data$code_zone) |>
    dplyr::mutate(share = .data$value / sum(.data$value)) |>
    dplyr::ungroup()

  # Broader employment groups #
  df_label <- data.frame(
    jobs_group = unique(jobs$jobs_group),
    jobs_label = c(
      "agriculture", rep("industry", 2), "commerce", rep("services", 8), "pub_adm", "other"
    )
  )

  # Aggregate into broader groups #
  tbl_jobs <- jobs |>
    dplyr::left_join(df_label, by = "jobs_group") |>
    dplyr::summarise(
      total = sum(.data$value), .by = c("code_zone", "jobs_label")
    ) |>
    dplyr::group_by(.data$code_zone) |>
    dplyr::mutate(share = .data$total / sum(.data$total)) |>
    dplyr::ungroup() |>
    tidyr::pivot_wider(
      id_cols = "code_zone",
      names_from = "jobs_label",
      values_from = c("total", "share"),
      names_prefix = "jobs_"
    )

  return(tbl_jobs)

}

#' @importFrom rlang .data
pod_table_jobs_sector <- function(con) {

  jobs_sector <- dplyr::tbl(con, "empregos_por_setor_de_atividade_e_zona_de_emprego_2017")

  new_names <- c(
    "code_zone" = "COD_ZONA",
    "sector" = "EMPREGO_POR_SETOR_ATIVIDADE",
    "total" = "QTD"
    )

  jobs_sector <- jobs_sector |>
    dplyr::collect() |>
    dplyr::rename(dplyr::all_of(new_names))

  tbl_jobs_sector <- jobs_sector |>
    dplyr::group_by(.data$code_zone) |>
    dplyr::mutate(
      sector = dplyr::case_when(
        sector == "Secund\u00e1rio" ~ "secondary",
        sector == "Terci\u00e1rio" ~ "tertiary",
        sector == "Outros" ~ "sector_other"
      ),
      share = .data$total / sum(.data$total)
    ) |>
    tidyr::pivot_wider(
      id_cols = "code_zone",
      names_from = "sector",
      values_from = c("total", "share"),
      names_prefix = "jobs_"
    )

  return(tbl_jobs_sector)

}

get_pod_table <- function(table, ...) {

  f <- paste0("pod_table_", table)
  rlang::exec(f, !!!list(...))

}

# Income deflator --------------------------------------------------------------
# IPC-FIPE price-level adjustment applied to nominal income in
# pod_table_income() (Apr-2018 to Jan-2021). Moved here from the package's
# R/utils.R when the build pipeline left odsp.
ipc_fipe_adjust <- 1.1609
