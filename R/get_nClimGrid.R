nClimGrid_url <- "https://www.ncei.noaa.gov/data/nclimgrid-daily/access/grids/"

download_nClimGrid <- function(year, months) {
  file_names <- paste0("ncdd-", year, months, "-grd-scaled.nc")

  file_paths <- file.path(tempdir(), file_names)

  requests <- file_names[!file.exists(file_paths)] |>
    lapply(function(file_name) {
      httr2::request(nClimGrid_url) |>
        httr2::req_url_path_append(
          regmatches(file_name, m = regexpr("\\d{4}", file_name))
        ) |>
        httr2::req_url_path_append(file_name)
    })

  resp <- requests |>
    httr2::req_perform_sequential(
      paths = file_paths[!file.exists(file_paths)],
      progress = TRUE
    )

  return(file_paths)
}

#' Download and Filter Weather Data
#'
#' @description
#' Downloads NOAA [nClimGrid-Daily](https://www.ncei.noaa.gov/products/land-based-station/nclimgrid-daily)
#' data for the specified interval.
#'
#' @param interval Dates to get weather data for
#' @param path Path to download files into (if not provided,
#' files will be download into the `tempdir()`)
#'
#' @return terra raster of climate data over the interval specified
#' @export
#'
#' @examples
#' \dontrun{
#' harvey_eaglei <- get_eaglei(
#'   interval = interval(harvey_start, harvey_end + days(5)),
#'   path = "raw-data/eaglei"
#' )
#' }
get_nClimGrid <- function(start, end) {
  year <- format(start, "%Y")
  months <- unique(format(c(start, end), "%m"))

  file_paths <- download_nClimGrid(year, months)

  combined_rasters <- lapply(file_paths, terra::rast) |>
    do.call(what = c)

  combined_rasters[[
    terra::time(combined_rasters) >= as.Date(start) &
      terra::time(combined_rasters) <= as.Date(end)
  ]]
}
