hurdat_atl_url <- "https://www.aoml.noaa.gov/hrd/hurdat/hurdat2.html"
hurdat_pac_url <- "https://www.aoml.noaa.gov/hrd/hurdat/hurdat2-nepac.html"

download_newest_hurdat2 <- function(basin) {
  hurdat2_url <- ifelse(basin == "AL", hurdat_atl_url, hurdat_pac_url)
  hurdat2_url_split <- strsplit(hurdat2_url, "/", fixed = TRUE)
  hurdat2_html_file <- hurdat2_url_split[[1]][length(hurdat2_url_split[[1]])]
  hurdat2_csv_file <- sub("html", "csv", hurdat2_html_file, fixed = TRUE)

  exposhur_dir <- file.path(tempdir(), "exposhur")
  if (!dir.exists(exposhur_dir)) {
    dir.create(exposhur_dir)
  }

  hurdat2_csv_path <- file.path(exposhur_dir, hurdat2_csv_file)
  if (!file.exists(hurdat2_csv_path)) {
    hurdat2_html_path <- file.path(exposhur_dir, hurdat2_html_file)
    download.file(hurdat2_url, hurdat2_html_path, quiet = TRUE)

    hurdat2_html <- readChar(
      hurdat2_html_path,
      file.info(hurdat2_html_path)$size
    )
    hurdat2_csv_text <- regmatches(
      hurdat2_html,
      m = regexpr("<pre>((.|\n)*)</pre>", hurdat2_html)
    )
    hurdat2_csv_text <- gsub("<pre>|</pre>", "", hurdat2_csv_text)

    write(hurdat2_csv_text, hurdat2_csv_path)
  }

  hurdat2_csv_path
}
#' Download HURDAT2 Best Track
#'
#' @description
#' Downloads most recent HURDAT2 file from NOAA
#' [HURDAT2 HTTPS Server](https://www.nhc.noaa.gov/data/hurdat/)
#'
#' @param storm_id Storm name in all caps, dash, year (EX: "HARVEY-2017")
#' @param path Path to download files into (if not provided,
#' files will be download into the `tempdir()`)
#'
#' @return HURDAT2 best track for a given storm
#' @export
#'
#' @examples
#' \dontrun{
#' # get an individual huricane's data by name
#' get_hurdat2(basin = "AL", storm_id = "HARVEY-2017")
#'
#' # get an individual huricane's data by ATCF ID
#' get_hurdat2(storm_id = "AL092017")
#'
#' # get all data for a basin
#' get_hurdat2(basin = "AL")
#' }
get_hurdat2 <- function(basin = c("AL", "EP"), storm_id) {
  rlang::arg_match(basin, value = c("AL", "EP"))
  if (!missing(storm_id)) {
    check_string(storm_id)
    if (grepl("(AL|EP)[0-9]{6}", storm_id)) {
      if (
        !identical(basin, c("AL", "EP")) &&
          !identical(basin, substr(storm_id, 1, 2))
      ) {
        cli::cli_abort(c(
          "!" = "{.arg basin} specified must match that of the {.arg storm_id}."
        ))
      } else {
        basin <- substr(storm_id, 1, 2)
      }
    } else {
      if (grepl("[A-Za-z]-[0-9]{4}", storm_id)) {
        if (length(basin) != 1) {
          cli::cli_abort(c(
            "!" = "{.arg basin} must be specified if using NAME-YEAR {.arg storm_id}."
          ))
        }
      } else {
        cli::cli_abort(c(
          "x" = "{.arg storm_id} must be specified using a valid ATCF ID or the storm's NAME-YEAR.",
          "i" = "{.val {storm_id}} is not a valid ID."
        ))
      }
    }
  } else if (!identical(basin, "AL") && !identical(basin, "EP")) {
    cli::cli_abort(c(
      "!" = "A single {.arg basin} must be specified if no {.arg storm_id} is."
    ))
  }

  hurdat2 <- download_newest_hurdat2(basin) |>
    read_hurdat2() |>
    trim_whitespace() |>
    process_headers() |>
    process_fields()

  if (!missing(storm_id)) {
    if (grepl("(AL|EP)[0-9]{6}", storm_id)) {
      hurdat2 <- hurdat2[hurdat2$storm_id == storm_id, ]
      if (nrow(hurdat2) == 0) {
        cli::cli_abort(c(
          "!" = "Cannot find {.arg storm_id} {.val {storm_id}} within the {.val {basin}} basin dataset."
        ))
      }
    } else {
      id_split <- strsplit(storm_id, "-", fixed = TRUE)[[1]]
      name <- id_split[1]
      year <- id_split[2]
      hurdat2 <- hurdat2[
        format(hurdat2$datetime, "%Y") == year & hurdat2$name == toupper(name),
      ]
      if (nrow(hurdat2) == 0) {
        cli::cli_abort(c(
          "!" = "Cannot find {.arg storm_id} {.val {storm_id}} within the {.val {basin}} basin dataset."
        ))
      }
    }
  }

  hurdat2
}

read_hurdat2 <- function(hurdat2_csv_path) {
  hurdat2 <- read.csv(hurdat2_csv_path, header = FALSE)

  colnames(hurdat2) <- c(
    "date",
    "time",
    "record_id",
    "system_status",
    "lat",
    "lon",
    "max_sust_wind",
    "min_pressure",
    "ne_34kt_radii",
    "se_34kt_radii",
    "sw_34kt_radii",
    "nw_34kt_radii",
    "ne_50kt_radii",
    "se_50kt_radii",
    "sw_50kt_radii",
    "nw_50kt_radii",
    "ne_64kt_radii",
    "se_64kt_radii",
    "sw_64kt_radii",
    "nw_64kt_radii",
    "max_wind_radii"
  )

  hurdat2
}

trim_whitespace <- function(hurdat2) {
  char_cols <- sapply(hurdat2, is.character)
  hurdat2[char_cols] <- lapply(hurdat2[char_cols], trimws)
  hurdat2 <- hurdat2[hurdat2$date != "", ]

  hurdat2
}

process_headers <- function(hurdat2) {
  header_rows <- grepl("(AL|EP)[0-9]{6}", hurdat2$date)
  hurdat2$storm_id <- ifelse(header_rows, hurdat2$date, NA)
  hurdat2$storm_id <- fill_downward(hurdat2$storm_id)
  hurdat2$name <- ifelse(header_rows, hurdat2$time, NA)
  hurdat2$name <- fill_downward(hurdat2$name)
  hurdat2 <- hurdat2[!header_rows, ]

  relocate_after(hurdat2, c("storm_id", "name"))
}

process_fields <- function(hurdat2) {
  hurdat2$datetime <- paste0(hurdat2$date, hurdat2$time)
  hurdat2$datetime <- as.POSIXct(
    hurdat2$datetime,
    tz = "UTC",
    format = "%Y%m%d%H%M"
  )
  hurdat2[c("date", "time")] <- NULL
  hurdat2 <- relocate_after(hurdat2, "datetime", "name")

  lat_len <- nchar(hurdat2$lat)
  lat_mag <- as.numeric(substr(hurdat2$lat, 1, lat_len - 1))
  lat_dir <- substr(hurdat2$lat, lat_len, lat_len)
  lat_sign <- 2 * as.numeric(lat_dir == "N") - 1
  hurdat2$lat <- lat_mag * lat_sign
  lon_len <- nchar(hurdat2$lon)
  lon_mag <- as.numeric(substr(hurdat2$lon, 1, lon_len - 1))
  lon_dir <- substr(hurdat2$lon, lon_len, lon_len)
  lon_sign <- 2 * as.numeric(lon_dir == "E") - 1
  hurdat2$lon <- lon_mag * lon_sign

  int_cols <- sapply(hurdat2, is.integer)
  hurdat2[int_cols] <- lapply(hurdat2[int_cols], function(int_col) {
    ifelse(int_col == -999, NA, int_col)
  })

  hurdat2[["max_sust_wind"]] <- units::set_units(
    hurdat2[["max_sust_wind"]],
    "international_knot"
  )
  hurdat2[["min_pressure"]] <- units::set_units(
    hurdat2[["min_pressure"]] / 1000,
    "bar"
  )
  radii_cols <- grep("radii$", colnames(hurdat2), value = TRUE)
  hurdat2[radii_cols] <- lapply(hurdat2[radii_cols], function(col) {
    units::set_units(col, "nautical_miles")
  })

  sf::st_as_sf(hurdat2, coords = c("lon", "lat"), crs = 4326)
}
