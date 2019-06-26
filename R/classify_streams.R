#' Classify streams as _wet_ or _dry_
#'
#' Classify streams as _wet_ or _dry_ using dissimilarity of the time series data.
#'
#' @param data Formatted data object.
#' @param centers The computed centers from the `compute_centers` function.
#' @param method The classification method to employ. Currently defaults to use the composite ross method.
#' @param lables Logical. Whether to include the labels "Wet" and "Dry" to the data depending on if the computed numerical values are less than or greater than one, respectively.
#' @return Returns the numeric classification values or, if labels = TRUE, the labels "Dry" or "Wet" depending on if the numeric values are greater than 1 or less than 1, respectively.
#'
#' @examples
#'
#' #' # Start by formatting the WWC data
#' formatted_wwc <- format_data(data = wwc,
#'                              date_var = "date_time",
#'                              date_start = "2015-04-01",
#'                              date_end = "2015-08-30",
#'                              site_id_var = "site_id",
#'                              sensor_type_var = "sensor_type",
#'                              resistor_level = "Resistor",
#'                              water_temp_level = "Water temp",
#'                              sensor_reading_var = "sensor_reading")
#'
#' # Then compute the centers of the wet and dry streams
#' centers <- compute_centers(data = formatted_wwc,
#'                            daily_mean_resistor = "daily_mean_resistor")
#'
#' # Finally, classify the streams and compute their numerical values where values greater than one indicate the stream was dry and values less than one indicate the stream was wet.
#' classify_streams(data = formatted_wwc, centers = centers)
#'
#' # Alternatively, you can include the "Wet" and "Dry" labels directly by setting labels = TRUE
#' classify_streams(data = formatted_wwc, centers = centers, labels = TRUE)
#'
#' @export

classify_streams <- function(data, centers, method = composite_ross, labels = FALSE) {
  classification_vals <- method(data = data, daily_mean_resistor = daily_mean_resistor,
                                dry_centers = centers$dry_center, wet_centers = centers$wet_center)
  if (labels) {
    classification_vals <- ifelse(classification_vals > 1, "Dry", "Wet")
  }
  return(classification_vals)
}
