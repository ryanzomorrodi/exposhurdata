#' Create wind radii features
#'
#'
#'
#' @export
st_wind_radii <- function(x, ne_dist, se_dist, sw_dist, nw_dist, ...) {
  lapply(1:nrow(x), function(i) {
    points <- rbind(
      get_radii_points(x[i, ], ne_dist, "NE", ...),
      get_radii_points(x[i, ], se_dist, "SE", ...),
      get_radii_points(x[i, ], sw_dist, "SW", ...),
      get_radii_points(x[i, ], nw_dist, "NW", ...)
    )

    rbind(
      points[c("circle_x", "circle_y")],
      points[1, c("circle_x", "circle_y")]
    ) |>
      as.matrix() |>
      list() |>
      sf::st_polygon()
  }) |>
    sf::st_sfc(crs = 4326)
}

get_radii_points <- function(x, dist, direction, ...) {
  distances <- x[[dist]]
  distances[is.na(distances)] <- 0

  circle = sf::st_buffer(
    x = x,
    dist = distances,
    ...
  )

  x_coord <- sf::st_coordinates(x) |>
    as.data.frame()
  names(x_coord)[names(x_coord) == "X"] <- "center_x"
  names(x_coord)[names(x_coord) == "Y"] <- "center_y"

  circle_coords <- sf::st_coordinates(circle) |>
    as.data.frame()
  names(circle_coords)[names(circle_coords) == "X"] <- "circle_x"
  names(circle_coords)[names(circle_coords) == "Y"] <- "circle_y"
  circle_coords <- circle_coords[c("circle_x", "circle_y")]

  merged <- cbind(circle_coords, x_coord)
  if (direction == "NE") {
    merged <- merged[
      merged$circle_x >= merged$center_x & merged$circle_y >= merged$center_y,
    ]
    merged[order(merged$circle_x, -merged$circle_y), ]
  } else if (direction == "NW") {
    merged <- merged[
      merged$circle_x <= merged$center_x & merged$circle_y >= merged$center_y,
    ]
    merged[order(merged$circle_x, merged$circle_y), ]
  } else if (direction == "SE") {
    merged <- merged[
      merged$circle_x >= merged$center_x & merged$circle_y <= merged$center_y,
    ]
    merged[order(-merged$circle_x, -merged$circle_y), ]
  } else if (direction == "SW") {
    merged <- merged[
      merged$circle_x <= merged$center_x & merged$circle_y <= merged$center_y,
    ]
    merged[order(-merged$circle_x, merged$circle_y), ]
  }
}
