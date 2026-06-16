#' Import POD travel tables
#'
#' @inheritParams import_pod_tables
#'
#' @return A named `list`
#' @keywords internal
#' @noRd
import_pod_travel_tables <- function(cached = TRUE, geo = FALSE, tables = "all",
                                     refresh = FALSE) {

  all_tables <- c("motive", "mode", "od", "time")

  # Select tables
  if (identical(tables, "all")) {
    name_tables <- all_tables
  } else {
    stopifnot(all(tables %in% all_tables))
    name_tables <- tables
  }

  if (cached) {

    if (geo) {

      dat <- readr::read_rds(od_cache_fetch("geo_pod_travel.rds", refresh = refresh))
    } else {
      dat <- readr::read_rds(od_cache_fetch("tbl_pod_travel.rds", refresh = refresh))
    }

    # The cached object is a named list; honour the requested subset.
    keep <- intersect(paste0("travel_", name_tables), names(dat))
    if (length(keep) > 0) dat <- dat[keep]

    return(dat)

  }

  rlang::check_installed(
    c("DBI", "RSQLite", "tidyr", "purrr", "stringr", "janitor"),
    "to build the POD travel tables from the raw survey database."
  )

  # Create a connection with the database
  arquivo_db <- "data-raw/DB_ORIGEM_DESTINO_SP"
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = arquivo_db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  name_tables <- paste0("travel_", name_tables)

  # Get all POD tables
  tbls <- suppressWarnings(lapply(name_tables, get_pod_table, con))
  names(tbls) <- name_tables

  # Joins the POD data to the zones shape file. Each element of `tbls` is itself
  # a list of tibbles, so recurse into the leaves and join geometry wherever a
  # `code_zone` column is present.
  if (geo) {

    add_geo_dimension <- function(x) {
      if (is.data.frame(x)) {
        if ("code_zone" %in% names(x)) {
          dplyr::left_join(
            dplyr::select(zones, "code_zone"), x, by = "code_zone"
          )
        } else {
          x
        }
      } else if (is.list(x)) {
        lapply(x, add_geo_dimension)
      } else {
        x
      }
    }

    out <- lapply(tbls, add_geo_dimension)

  } else {
    out <- tbls
  }

  out

}

#' @importFrom rlang .data
pod_table_travel_motive <- function(con) {

  tbl_motive <- dplyr::tbl(
    con, "viagens_diarias_atraidas_por_motivo_e_zona_de_destino_2017"
  )

  new_names <- c(
    "code_zone" = "COD_ZONA",
    "motive" = "VIAGENS_ATRAIDAS_POR_MOTIVO",
    "value" = "QTD"
  )

  tbl_motive <- tbl_motive |>
    dplyr::rename(dplyr::all_of(new_names)) |>
    dplyr::collect()

  df_cat <- tibble::tibble(
    motive = unique(tbl_motive$motive)
  )

  df_cat <- df_cat |>
    dplyr::mutate(
      cat = stringr::str_remove(.data$motive, " .+"),
      cat = ifelse(cat == "Procurar", "Trabalho", cat),
      cat = ifelse(cat == "Assuntos", "Outros", cat),
      subcat = ifelse(
        .data$cat %in% c("Compras", "Lazer", "Refei\u00e7\u00e3o"),
        "Lazer/Compras",
        .data$cat
        )
    )

  # Motive for travels in groups
  tbl_motive_group <- tbl_motive |>
    dplyr::left_join(df_cat, by = "motive") |>
    dplyr::summarise(total = sum(.data$value), .by  = c("code_zone", "cat")) |>
    dplyr::group_by(.data$code_zone) |>
    dplyr::mutate(share = 100 * .data$total / sum(.data$total)) |>
    dplyr::ungroup() |>
    dplyr::rename(dplyr::all_of(
      c("motive_subgroup" = "cat", "ntravel" = "total")
    ))

  # Motive for travels in more condensed groups
  tbl_submotive <- tbl_motive |>
    dplyr::left_join(df_cat, by = "motive") |>
    dplyr::summarise(total = sum(.data$value), .by = c("code_zone", "subcat")) |>
    dplyr::group_by(.data$code_zone) |>
    dplyr::mutate(share = 100 * .data$total / sum(.data$total)) |>
    dplyr::ungroup() |>
    dplyr::rename(dplyr::all_of(
      c("motive_group" = "subcat", "ntravel" = "total")
    ))

  # Join all tables into single table
  tbl_motive <- tbl_motive |>
    dplyr::left_join(df_cat, by = "motive") |>
    dplyr::rename(dplyr::all_of(
      c("motive_group" = "subcat", "motive_subgroup" = "cat", "ntravel" = "value")
    )) |>
    dplyr::select(dplyr::all_of(
      c("code_zone", "motive", "motive_group", "motive_subgroup", "ntravel")
    ))

  # Return a grouped wide table with totals and shares
  tbl_motive_grouped <- tbl_submotive |>
    dplyr::rename(dplyr::all_of(c("total" = "ntravel"))) |>
    tidyr::pivot_wider(
      id_cols = "code_zone",
      names_from = "motive_group",
      values_from = c("total", "share")
    )

  tbl_motive_grouped <- janitor::clean_names(tbl_motive_grouped)

  return(list(motive = tbl_motive, motive_grouped = tbl_motive_grouped))

}

#' @importFrom rlang .data
pod_table_travel_mode <- function(con, grouped = FALSE) {

  tbl_mode <- dplyr::tbl(
    con,
    "viagens_diarias_atraidas_por_modo_principal_e_zona_de_destino_2017"
    )

  new_names <- c(
    "code_zone" = "COD_ZONA",
    "mode" = "VIAGENS_ATRAIDAS_MODO_PRINCIPAL",
    "value" = "QTD"
  )

  tbl_mode <- tbl_mode |>
    dplyr::collect() |>
    dplyr::rename(dplyr::all_of(new_names)) |>
    dplyr::mutate(value = as.numeric(.data$value))

  df_cat <- tibble::tibble(
    mode = unique(tbl_mode$mode),
    group = c(rep("Coletivo", 5), rep("Individual", 8), "Outros")
  )

  x <- c("Metr\u00f4", "Trem", "\u00d4nibus", "autom\u00f3vel", "T\u00e1xi", "moto", "Bicicleta", "A p\u00e9")
  x <- paste(x, collapse = "|")

  df_cat <- df_cat |>
    dplyr::mutate(
      cat = stringr::str_to_title(stringr::str_extract(.data$mode, x)),
      cat = ifelse(is.na(.data$cat), "Outros", .data$cat),
      pt = ifelse(stringr::str_detect(.data$mode, "Metr\u00f4|Trem|\u00d4nibus"), 1, 0))

  tbl_mode_grouped <- tbl_mode |>
    dplyr::left_join(df_cat, by = "mode") |>
    dplyr::summarise(ntravel = sum(.data$value), .by = c("code_zone", "cat")) |>
    dplyr::group_by(.data$code_zone) |>
    dplyr::mutate(share = 100 * .data$ntravel / sum(.data$ntravel)) |>
    dplyr::ungroup() |>
    dplyr::arrange(dplyr::desc(.data$share)) |>
    dplyr::arrange(.data$code_zone)

  tbl_mode_pt <- tbl_mode |>
    dplyr::left_join(df_cat, by = "mode") |>
    dplyr::summarise(ntravel = sum(.data$value), .by = c("code_zone", "pt")) |>
    dplyr::group_by(.data$code_zone) |>
    dplyr::mutate(share = 100 * .data$ntravel / sum(.data$ntravel)) |>
    dplyr::ungroup() |>
    tidyr::pivot_wider(
      id_cols = "code_zone",
      names_from = "pt",
      values_from = c("ntravel", "share")) |>
    dplyr::mutate(index_pt = .data$ntravel_1 / .data$ntravel_0 * 100) |>
    dplyr::rename(dplyr::all_of(
      c(
        "travel_non_pt" = "ntravel_0",
        "travel_pt" = "ntravel_1",
        "share_travel_non_pt" = "share_0",
        "share_travel_pt" = "share_1"
        )
      )
    )

  new_names <- c(
    "transport_mode" = "mode",
    "transport_mode_group" = "cat",
    "transport_type" = "group",
    "ntravel" = "value"
  )

  tbl_mode <- tbl_mode |>
    dplyr::left_join(df_cat, by = "mode") |>
    dplyr::mutate(
      transport_active = dplyr::if_else(
        .data$mode %in% c("A p\u00e9", "Bicicleta"),
        "Ativo",
        dplyr::if_else(.data$mode == "Outros", NA_character_, "Passivo"))
    ) |>
    dplyr::rename(dplyr::all_of(new_names)) |>
    dplyr::select(dplyr::all_of(
      c(
        "code_zone",
        "transport_mode",
        "transport_mode_group",
        "transport_type",
        "transport_active",
        "ntravel")
    ))

  out <- list(
    mode = tbl_mode,
    mode_pt = tbl_mode_pt,
    mode_grouped = tbl_mode_grouped
    )

  if (grouped) {

    tbl_mode_grouped <- tbl_mode_grouped |>
      dplyr::rename(dplyr::all_of(c("total" = "ntravel"))) |>
      tidyr::pivot_wider(
        id_cols = "code_zone",
        names_from = "cat",
        values_from = c("total", "share")
      )

    tbl_active <- tbl_mode |>
      dplyr::filter(!is.na(.data$transport_active)) |>
      dplyr::summarise(
        total = sum(.data$ntravel, na.rm = TRUE),
        .by = c("code_zone", "transport_active")
      ) |>
      dplyr::group_by(.data$code_zone) |>
      dplyr::mutate(share = .data$total / sum(.data$total)) |>
      dplyr::ungroup()

    tbl_active <- tbl_active |>
      tidyr::pivot_wider(
        id_cols = "code_zone",
        names_from = "transport_active",
        values_from = c("total", "share")
      )

    out <- list(mode = tbl_mode_grouped, pt = tbl_mode_pt, active = tbl_active)
    out <- purrr::map(out, janitor::clean_names)
    out <- purrr::reduce(out, dplyr::full_join, by = "code_zone")

  }

  return(out)

}

#' @importFrom rlang .data
pod_table_travel_od <- function(con) {

  tbl_mtravel <- dplyr::tbl(
    con,
    "viagens_diarias_totais_por_zonas_de_origem_e_destino_2017"
    )

  new_names <- c(
    "origin" = "COD_ZONA_ORIGEM",
    "destination" = "COD_ZONA_DESTINO",
    "value" = "QTD")

  tbl_mtravel <- tbl_mtravel |>
    dplyr::collect() |>
    dplyr::rename(dplyr::all_of(new_names))

  tbl_destination <- tbl_mtravel |>
    dplyr::summarise(travels_destination = sum(.data$value), .by = "origin")

  tbl_attraction <- tbl_mtravel |>
    dplyr::summarise(travels_attracted = sum(.data$value), .by = "destination")

  tbl_main_origins <- tbl_mtravel |>
    dplyr::group_by(.data$destination) |>
    dplyr::mutate(rank = rank(-.data$value)) |>
    dplyr::ungroup() |>
    dplyr::filter(.data$rank <= 10) |>
    dplyr::arrange(.data$destination)

  tbl_main_destinations <- tbl_mtravel |>
    dplyr::group_by(.data$origin) |>
    dplyr::mutate(rank = rank(-.data$value)) |>
    dplyr::ungroup() |>
    dplyr::filter(rank <= 10) |>
    dplyr::arrange(.data$origin)

  new_names <- c(
    "code_zone_origin" = "origin",
    "code_zone_destination" = "destination",
    "ntravel" = "value"
  )

  tbl_origin_destination <- tbl_mtravel |>
    dplyr::rename(dplyr::all_of(new_names))

  out <- list(
    origin_destination = tbl_origin_destination,
    main_origins = tbl_main_origins,
    main_destinations = tbl_main_destinations
  )

  return(out)

}

#' @importFrom rlang .data
pod_table_travel_time <- function(con, grouped = FALSE) {

  tbl_time <- dplyr::tbl(
    con,
    "tempo_medio_das_viagem_produzidas_por_tipo_de_viagem_e_zona_de_origem_2017"
    )

  new_names <- c(
    "code_zone" = "COD_ZONA",
    "transport_mode" = "TEMPO_MEDIO_VIAGEM",
    "travel_time" = "QTD"
  )

  tbl_time <- tbl_time |>
    dplyr::collect() |>
    dplyr::rename(dplyr::all_of(new_names))

  if (grouped) {

    tbl_time <- tbl_time |>
      tidyr::pivot_wider(
        id_cols = "code_zone",
        names_from = "transport_mode",
        names_prefix = "avg_time_",
        values_from = "travel_time"
      )

    tbl_time <- janitor::clean_names(tbl_time)

    }

  return(tbl_time)

}

