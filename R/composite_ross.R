composite_ross <- function(data, daily_mean_resistor, dry_centers, wet_centers) {
  # Use these medoids to test the classification on the full and spring/summer datasets
  matrix_dat <- as.matrix(data[, 4:27])
  # datstatus <- data[, daily_mean_resistor] >= 0

  # Build the Ross classifier. (Dist == Distances)
  sax_dry_dist <- saxvec(x = dry_centers, m = matrix_dat)
  sax_wet_dist <- saxvec(x = wet_centers, m = matrix_dat)

  cid_dry_dist <- cidvec(x = dry_centers, m = matrix_dat)
  cid_wet_dist <- cidvec(x = wet_centers, m = matrix_dat)

  composite_dry_dist <- sax_dry_dist + log(cid_dry_dist + 1)
  composite_wet_dist <- sax_wet_dist + log(cid_wet_dist + 1)

  composite_ross <- composite_wet_dist / composite_dry_dist
  return(composite_ross)
}
