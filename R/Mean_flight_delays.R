


#' Visualize Mean Flight Delays by Airport
#'
#' Shows the mean flight delays for different destination
#' airports in the United States using data from the \pkg{nycflights13} package.
#' It combines information from the \code{flights} and \code{airports} datasets,
#' calculates the mean arrival delay for each destination airport, and plots
#' these delays by location.
#'
#' @return
#' A \pkg{ggplot2} plot showing airports positioned by their geographic
#' coordinates.
#'
#' @examples
#' \dontrun{
#' visualize_airport_delays()
#' }
#'
#' @import dplyr
#' @import ggplot2
#' @import nycflights13
#' @export

visualize_airport_delays <- function() {
  flights %>%
    dplyr::group_by(dest) %>%
    dplyr::summarise(mean_delay = mean(arr_delay, na.rm = TRUE)) %>%
    dplyr::inner_join(airports, by = c("dest" = "faa")) %>%
    ggplot2::ggplot(ggplot2::aes(x = lon, y = lat, color = mean_delay)) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_color_viridis_c(option = "plasma") +
    ggplot2::labs(
      title = "Mean Flight Arrival Delays by Airport",
      x = "Longitude",
      y = "Latitude",
      color = "Mean Delay (minutes)"
    ) +
    ggplot2::theme_minimal()
}



