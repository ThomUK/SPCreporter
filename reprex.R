cutoff_date <- lubridate::floor_date(Sys.Date(), unit = "month") - lubridate::as.period("1d")
cutoff_dttm <- (cutoff_date + 1) - lubridate::as.period("1s")
data_cutoff_dttm <- cutoff_dttm
def_start_date <- lubridate::as_datetime("2020-01-01")
bnet_start <- lubridate::as_datetime("2022-12-01")
m_prefix <- "fh_m"
prefix <- \(x, mp = m_prefix) paste0(mp, "_", x)


rep_config <- "Maternity_Services_Report" |>
  FH027MaternityReporting::get_report_config() |>
  dplyr::filter(if_any("ref", \(x) x %in% c("6", "132", "197")))

measure_config <- FH027MaternityReporting::get_measure_config() |>
  dplyr::filter(if_any("measure_prefix", \(x) x == m_prefix))


report_refs <- sort(unique(as.numeric(rep_config[["ref"]])))
completed_refs <- unique(c(
  57, 74, 85, 90, 118, 132, 172, 195:196, 203:205, # excel
  197, 250, # eobs
  75:77, 79:81, 84, 86:88, 104, 108, 194, # datix
  # 43, 48, 50, 53:55, 174, 320, # misc other long
  53:55, # hie
  # 45, 200, # kilometrics
  33, 40, 62, 63 # medway
))
bnet_refs <- prefix(setdiff(report_refs, completed_refs))

bnet_config <- rep_config |>
  dplyr::filter(if_any("measure_id", \(x) x %in% bnet_refs))


bnet_data <- DataHoover::dh_build_report_dataset(
  measure_config = measure_config,
  report_config = bnet_config,
  start_dttm = bnet_start,
  end_dttm = cutoff_dttm,
  output_format = "long"
)

bnet_event_refs <- prefix(c(
  2:3, 8, 11, 13, 15, 31, 37, 39, 161:162, 178, 180, 207:212, 222:223,
  257, 261:262, 265:267, 269:270, 273:281, 311, 316:317
))

bnet_data_out <- bnet_data[["event_lists"]] |>
  purrr::keep_at(as.character(bnet_event_refs)) |>
  purrr::map(\(x) DataHoover::dh_aggregate(x, bnet_start, cutoff_dttm, "month")) |>
  purrr::list_rbind() |>
  dplyr::bind_rows(bnet_data[["month"]]) |>
  dplyr::rename_with(tolower) |>
  dplyr::rename(ref = "measure_ref") |>
  # 'kilometrics':
  dplyr::mutate(
    across("value",
      \(x) dplyr::if_else(
        .data[["ref"]] %in% c("45", "200") & .data[["date"]] >= bnet_start,
        x * 1000,
        x
      )
    )
  )

cf_id_cols <- c("measure_prefix", "ref", "measure_name", "comment")
cf_cutoff <- lubridate::ymd("2022-11-01")

cf_refs <- c(2, 43, 45, 48, 50, 161, 174, 200, 267, 320)

xl6_data <- "Careflow Data" |>
  FH027MaternityReporting:::create_network_path() |>
  FH027MaternityReporting:::read_sheet("month") |>
  dplyr::mutate(across("ref", as.character)) |>
  dplyr::mutate(comment = "Read in from Careflow Data Excel file.") |>
  dplyr::mutate(measure_prefix = "fh_m", .before = "ref") |>
  dplyr::select(all_of(cf_id_cols) | matches("^\\d{4}\\-\\d{2}\\-\\d{2}$")) |>
  tidyr::pivot_longer(
    cols = !any_of(cf_id_cols),
    names_to = "date",
    names_transform = list(date = as.POSIXct)
  ) |>
  dplyr::filter(if_any("date", \(x) x <= cf_cutoff)) |>
  dplyr::filter(if_any("value", \(x) !is.na(x))) |>
  dplyr::filter(if_any("ref", \(x) x %in% as.character(cf_refs)))


 if ("132" %in% rep_config[["ref"]]) {
  xl2_data <- "Maternal Deaths" |>
    FH027MaternityReporting:::create_network_path() |>
    FH027MaternityReporting:::read_sheet("none") |>
    dplyr::mutate(Measure_Prefix = "fh_m", .before = "Measure_Ref") |>
    dplyr::mutate(across("Measure_Ref", as.character))
  assertthat::assert_that(identical(names(xl2_data), tb_cols))
} else {
  xl2_data <- NULL
}


eobs_st <- lubridate::as_datetime("2021-03-01")
eobs_refs <- prefix(c(197, 250))
# nums & denoms for M197 & M250:
eobs_event_refs <- prefix(251:254)
eobs_config <- rep_config |>
  dplyr::filter(if_any("measure_id", \(x) x %in% eobs_refs))


if (nrow(eobs_config) > 0) {
  eobs_data <- DataHoover::dh_build_report_dataset(
    measure_config = measure_config,
    report_config = eobs_config,
    start_dttm = eobs_st,
    end_dttm = cutoff_dttm,
    aggregation_levels = "month",
    output_format = "long"
  )
  eobs_data_out <- eobs_data[["event_lists"]] |>
  purrr::keep_at(eobs_event_refs) |>
  purrr::map(\(x) DataHoover::dh_aggregate(x, eobs_st, cutoff_dttm, "month")) |>
  purrr::list_rbind() |>
  dplyr::bind_rows(eobs_data[["month"]]) |>
  dplyr::rename_with(tolower) |>
  dplyr::rename(ref = "measure_ref")
  assertthat::assert_that(identical(names(eobs_data_out), pref_cols))
} else {
  eobs_data_out <- NULL
}

rare_refs <- rep_config |>
  dplyr::filter(if_any("spc_chart_type", \(x) x == "t")) |>
  dplyr::pull("ref") |>
  unique()
pref_rare_refs <- prefix(rare_refs)




rare_data <- list(
  bnet_data[["event_lists"]]
) |>
  purrr::list_flatten() |>
  purrr::keep_at(pref_rare_refs) |>
  purrr::list_rbind() |>
  dplyr::bind_rows(xl2_data) |>
  dplyr::filter(if_any("Measure_Ref", \(x) x %in% rare_refs))

if (nrow(rare_data) == 0) {
  rare_data <- tibble::tibble(
    measure_prefix = character(),
    ref = character(),
    measure_name = character(),
    comment = character(),
    event_date_or_datetime = as.Date(character())
  )
} else {
  rare_data <- rare_data |>
    dplyr::rename_with(tolower) |>
    dplyr::arrange(pick(c("measure_ref", "dttm"))) |>
    dplyr::select(all_of(c(
      "measure_prefix",
      ref = "measure_ref",
      "measure_name",
      "comment",
      "id",
      event_date_or_datetime = "dttm")
    )) |>
    dplyr::distinct()
}

main_data <- FH027MaternityReporting::get_main_data(report_config = rep_config)



mc_cols <- c("measure_prefix", "ref", "measure_name", "numerator_ref", "denominator_ref")
core_cols <- c("ref", "measure_name", "comment", "date", "value")
pref_cols <- c("measure_prefix", core_cols)
tb_cols <- c("Measure_Prefix", "Measure_Ref", "Measure_Name", "Comment", "ID", "DTTM")
ignore_refs <- c(
    "x", as.character(c(2, 43:44, 48:50, 53:57, 64, 74:77, 102, 132, 187, 320))
  )
da_id_cols <- c("measure_prefix", "ref", "measure_name", "comment")
da_cutoff <- lubridate::ymd("2022-11-01")


xl5_data <- "Division Analyst" |>
    FH027MaternityReporting:::create_network_path() |>
    FH027MaternityReporting:::read_sheet("month") |>
    dplyr::mutate(across("ref", as.character)) |>
    dplyr::filter(if_any("ref", \(x) !x %in% ignore_refs)) |>
    dplyr::mutate(measure_prefix = "fh_m", .before = "ref") |>
    dplyr::mutate(comment = "Read in from Division Analyst Excel file.") |>
    dplyr::select(all_of(da_id_cols) | matches("^[0-9]+$")) |>
    tidyr::pivot_longer(
      cols = !any_of(da_id_cols),
      names_to = "date",
      names_transform = list(
        date = \(x) as.Date(as.integer(x), origin = "1899-12-30")
      )
    ) |>
    dplyr::filter(if_any("date", \(x) x <= da_cutoff)) |>
    dplyr::filter(if_any("value", \(x) !is.na(x))) |>
    dplyr::filter(if_any("ref", \(x) x == "6"))


bnet_data <- DataHoover::dh_build_report_dataset(
    measure_config = measure_config,
    report_config = rep_config,
    start_dttm = bnet_start,
    end_dttm = cutoff_dttm,
    output_format = "long"
  )
bnet_data_out <- bnet_data[["month"]] |>
  dplyr::rename_with(tolower) |>
  dplyr::rename(ref = "measure_ref")


src_data <- list(
  xl5_data,  # DA spreadsheet
  bnet_data_out,
  eobs_data_out
) |>
  dplyr::bind_rows() |>
  dplyr::distinct()

src_data_wide <- src_data |>
  dplyr::select(all_of(pref_cols)) |>
  dplyr::distinct() |>
  dplyr::mutate(
    across("comment", \(x) stringr::str_flatten(unique(x), " / ")),
    .by = c("measure_prefix", "ref")
  ) |>
  dplyr::distinct() |>
  dplyr::arrange(date) |>
  tidyr::pivot_wider(names_from = "date") |>
  dplyr::arrange(as.numeric(grep("\\d+", .data[["ref"]], value = TRUE)))


n_months <- lubridate::interval(def_start_date, cutoff_date) |>
  lubridate::as.period("months") |>
  stringr::str_extract("^\\d+") |>
  as.numeric()
assertthat::assert_that(
  ncol(src_data_wide) == (n_months + 1) + (length(pref_cols) - 2)
)

main_data <- list(month = src_data_wide, events = rare_data)


main_bundle <- FH027MaternityReporting::generate_main_bundle(
  main_data = main_data,
  report_config = rep_config,
  measure_config = measure_config
)

FH027MaternityReporting::generate_main_report(
  data_bundle = main_bundle,
  report_config = rep_config,
  measure_config = measure_config
)


rbds <- main_bundle[["rebase_dates"]]
rbds

parse_rebase_dates(rbds)
rbds_dates <- purrr::map(rbds, parse_rebase_dates)
m_data <- main_bundle[["measure_data"]]

pull_closest_date <- function(date, dates_list) {
  if (is.null(date)) NA
  else {
    later_dates <- dates_list[dates_list >= date]
    if (length(later_dates)) min(later_dates) else date
  }
}

rbds_dates |>
  purrr::map2_dbl(m_data, \(x, y) pull_closest_date(x, y[["date"]])) |>
  lubridate::as_datetime() |>
  lubridate::as_date()



# --- 


main_data <- FH027MaternityReporting::get_main_data()

report_config <- "Maternity_Services_Report" |>
  FH027MaternityReporting:::get_report_config()
measure_config <- FH027MaternityReporting:::get_measure_config() |>
  dplyr::filter(if_any("measure_prefix", \(x) x == m_prefix))

data_cutoff_dttm <- (cutoff_date + 1) - lubridate::as.period("1s")

measure_data <- check_measure_data(main_data)
report_config <- check_report_config(report_config)
measure_config <- check_measure_config(measure_config)


e_data <- measure_data |>
  purrr::pluck("events")
a_data <- measure_data |>
  purrr::discard_at("events")
a_data <- check_a_data(a_data)
e_data <- check_e_data(e_data)
e_data_time_between <- process_event_data_t(e_data, data_cutoff_dttm)


a_data_df <- a_data |>
  dplyr::bind_rows(.id = "aggregation")

measure_data_long <- a_data_df |>
  lengthen_measure_data() |>
  dplyr::bind_rows(e_data_time_between)
check_dataset_is_complete(report_config, measure_data_long)

report_config |>
    dplyr::pull("ref") |>
    purrr::walk(\(x) check_measure_names(x, measure_data_long, measure_config))


nested_data1 <- report_config |>
    # use measure names from report_config not from measure_config
    dplyr::left_join(dplyr::select(measure_config, !"measure_name"), "ref") |>
    dplyr::mutate(
      measure_name = dplyr::case_when(
        spc_chart_type == "t" ~ paste(measure_name, "(time-between)"),
        TRUE ~ measure_name
      )
    )
nested_data2 <- nested_data1 |>
    dplyr::nest_join(
      measure_data_long,
      by = c("ref", "aggregation"),
      name = "measure_data"
    ) |>

    # pull most recent date from each data frame in the measure_data column
    dplyr::mutate(
      data_cutoff_dttm = as.POSIXct(data_cutoff_dttm),
      last_date = purrr::map_vec(.data[["measure_data"]], \(x) max(x[["date"]], na.rm = TRUE))
    )
nested_data3 <- nested_data2 |>
    # pull most recent data point from each data frame in the measure_data column
    dplyr::mutate(
      last_data_point = purrr::map_vec(.data[["measure_data"]], \(x) {
        dplyr::slice_max(x, order_by = x[["date"]], n = 1)[["value"]]
      }
      )
    )
nested_data3 |>
    dplyr::mutate(
      across("improvement_direction",
             \(x) dplyr::case_when(
               .data[["spc_chart_type"]] == "t" & x == "decrease" ~ "increase",
               # a rather unlikely situation
               .data[["spc_chart_type"]] == "t" & x == "increase" ~ "decrease",
               TRUE ~ x))
      ) |>
    dplyr::mutate(
      across("unit",
             \(x) if_else(.data[["spc_chart_type"]] == "t", "days", x))
      ) |>
    dplyr::mutate(
      across("target",
             \(x) if_else(.data[["spc_chart_type"]] == "t", NA, x))
      ) |>
    dplyr::mutate(
      across("last_data_point", \(x) dplyr::case_when(
        is.na(x) ~ NA_character_,
        x == Inf ~ NA_character_,
        unit == "%" ~ paste0(round(x * 100, 1), "%"),
        unit == "decimal" ~ as.character(round(x, 2)),
        unit == "days" ~ paste0(x, "d"),
        TRUE ~ as.character(round(x))))
      ) |>
    dplyr::mutate(
      target_text = get_target_text(
        .data[["target"]],
        .data[["improvement_direction"]],
        .data[["unit"]]
        ),
      updated_to = get_updatedto_text(
        .data[["last_date"]],
        .data[["aggregation"]]
        )
      )



main_bundle <- FH027MaternityReporting::generate_main_bundle()

View(main_bundle)


