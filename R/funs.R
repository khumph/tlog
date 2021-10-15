pct_1rm <- function(reps, rpe) {
  x <- 0.725 + 0.0275 * rpe - 0.0275 * reps
  x <- replace(x, rpe == 10 & reps == 1, 1)
  x <- replace(x, rpe == 10 & reps == 2, 0.98)
  return(x)
}

process_data <- function(data) {
  data$pct1rm <- round(pct_1rm(data$reps, data$rpe), 2)
  data$e1rm <- round(data$weight / data$pct1rm)
  data$variation <- replace(data$variation, is.na(data$variation), "")
  data$lift_full <- paste(data$variation, data$lift)
  data$lift_full <- tools::toTitleCase(data$lift_full)
  return(data)
}

get_metrics <- function(data, date_range) {
  data <- dplyr::filter(data, date >= date_range[1], date <= date_range[2],
                        !is.na(weight) | !is.na(time) | !is.na(rpe))
  data <- dplyr::group_by(data, date, lift_full)
  data$tonnage <- data$weight * data$reps
  data <- dplyr::summarise(
    data,
    e1rm = max(e1rm),
    tonnage = sum(tonnage),
    volume = sum(reps),
    intensity = mean(pct1rm),
    load = unique(rpe * time)
  )
  data <- tidyr::gather(data, param, value, -date, -lift_full)
  data <- dplyr::filter(data, !is.na(value))
  return(data)
}

get_raw_data <- function(url, metric) {
  googlesheets4::gs4_deauth()
  if (metric == "dur") {
    sheet <-"log"
    dat <- googlesheets4::read_sheet(as_sheets_id(url), sheet = sheet)
  } else {
    sheet <- "lift"
    dat <- process_data(googlesheets4::read_sheet(googlesheets4::as_sheets_id(url),
                            sheet = sheet, col_types = "Dccdidididic"))
  }
  return(dat)
}

select_data <- function(raw_data, metric, date_range) {
  if (metric == "dur") {
    dat <- raw_data() %>%
      dplyr::filter(!is.na(date)) %>%
      dplyr::mutate(date = lubridate::force_tz(date, "America/Los_Angeles"),
             iso_wk = lubridate::isoweek(date),
             yr_wk = paste0(lubridate::year(date), "-", iso_wk)) %>%
      dplyr::group_by(yr_wk) %>%
      dplyr::mutate(wk_st = min(date)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(wk_st >= date_range[1], wk_st <= date_range[2])
  } else {
    dat <- get_metrics(raw_data, date_range) %>%
      dplyr::group_by(date) %>%
      dplyr::filter(param == metric)
  }

  return(dat)
}

make_plot <- function(data, metric) {
  if (metric == "dur") {
    out <- data %>%
      dplyr::group_by(wk_st) %>%
      dplyr::summarise(value = round(sum(dur, na.rm = T)/60, 1),
                       value_p = round(sum(dur_p, na.rm = T)/60, 1)) %>%
      plotly::plot_ly(x = ~wk_st, y = ~value_p, name = "Planned", type = "bar",
                      alpha = 0.5, height = 800) %>%
      plotly::layout(legend = list(orientation = "h"),
                     yaxis = list(title = "Total duration (hours)"),
                     xaxis = list(title = "Week"))
    out <- out %>% plotly::add_trace(x = ~wk_st, y = ~value, name = "Actual")
  } else {
    out <- data %>%
      dplyr::filter(param == metric) %>%
      dplyr::group_by(lift_full) %>%
      plotly::plot_ly(x = ~date, y = ~value, color =~lift_full, type = "scatter",
                      mode = "lines+markers", height = 800) %>%
      plotly::layout(legend = list(title = list(text="Lift"), orientation = "h"))
  }
  return(out)
}

write_est_weights <- function(est, url, date_range) {
  email <- "khumph2@gmail.com"
  googlesheets4::gs4_auth(email = email)
  raw_data <- process_data(googlesheets4::read_sheet(googlesheets4::as_sheets_id(url), sheet = "lift", col_types = "Dccdidddiddc"))
  selected_data <- get_metrics(raw_data, date_range)
  dat <- raw_data %>% dplyr::filter(date >= Sys.Date())
  in_e1rm <- selected_data %>%
    dplyr::filter(param == "e1rm", lift_full %in% unique(dat$lift_full)) %>%
    dplyr::group_by(lift_full) %>%
    dplyr::summarise(ee1rm = min(value))
  out <- dplyr::left_join(dat, in_e1rm, by = 'lift_full') %>%
    dplyr::mutate(weight_p = dplyr::if_else(is.na(weight_p),
                              (round(pct_1rm(reps_p, rpe_p) * ee1rm / 2.5) * 2.5),
                              weight_p),
           date = as.Date(date)) %>%
    dplyr::select(date, lift, variation, weight, reps, rpe, time, weight_p, reps_p,
           rpe_p, time_p, notes) %>%
    dplyr::group_by(date) %>%
    do(tibble::add_row(., .before = 0)) %>%
    dplyr::ungroup()

  dat_keep <- raw_data %>% filter(date < Sys.Date())
  empty_rows <- which(is.na(raw_data$date))
  empty_rows_in_fut <- empty_rows >= which.max(raw_data$date[raw_data$date < Sys.Date()])
  empty_rows_in_past <- empty_rows[!empty_rows_in_fut]
  rows_to_del <- (empty_rows_in_past - seq_along(empty_rows_in_past)) + 2

  start <- nrow(dat_keep) + 1
  end <- nrow(raw_data) - (length(rows_to_del))
  bounds_to_del <- c(start, end) + 1

  ss <- googlesheets4::as_sheets_id(url, sheet = "lift")
  for (row in rows_to_del) {
    googlesheets4::range_delete(ss, sheet = "lift", range = googlesheets4::cell_rows(row))
  }
  googlesheets4::range_delete(ss, sheet = "lift", range = googlesheets4::cell_rows(bounds_to_del))
  googlesheets4::sheet_append(ss, out, sheet = "lift")
}

