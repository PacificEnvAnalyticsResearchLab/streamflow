test_data <- wwc[wwc$site_id == "12258",]
formatted_data <- format_data(data = test_data,
                              date_start = "2015-04-01",
                              date_end = "2015-08-30",
                              date_var = "date_time",
                              site_id_var = "site_id",
                              sensor_type_var = "sensor_type",
                              resistor_level = "Resistor",
                              water_temp_level = "Water temp",
                              sensor_reading_var = "sensor_reading")
centers <- compute_centers(data = formatted_data, daily_mean_resistor = "daily_mean_resistor")
classifications <- classify_streams(data = formatted_data, centers = centers)

test_that("Ensure formatting function works", {
  expect_equal(object = dim(formatted_data), expected = c(148, 27))
  expect_equal(object = formatted_data$daily_mean_resistor[1], -84.771)
  expect_equal(object = as.numeric(formatted_data[1, 4:27]), c(-0.732, -1.67, -2.712, -3.271, -3.419, -4.137, -4.804, -5.264, -4.016, -2.566, -0.451, 1.886, 6.433, 10.222, 9.977, 4.895, 6.763, 5.975, 3.591, 2.262, -0.507, -1.584, -2.015, -2.363))
})

test_that("Ensure center computation works", {
  expect_equal(unname(centers$dry_center), c(5.949, 4.584, 3.958, 3.38, 3.063, 2.396, 1.859, 1.778, 3.327, 6.077, 8.866, 10.883, 15.247, 19.817, 23.521, 24.339, 19.341, 14.29, 12.389, 10.932, 8.593, 7.97, 7.745, 7.945))
  expect_equal(unname(centers$wet_center), c(12.001, 11.832, 11.734, 11.637, 11.565, 11.467, 11.419, 11.492, 11.565, 11.904, 14.433, 14.984, 15.223, 16.415, 17.653, 18.152, 17.796, 16.939, 15.342, 15.031, 14.625, 13.978, 13.449, 13.112))
})

test_that("Ensure classifications work", {
  expect_equal(round(classifications, 2), c(1.42, 1.49, 1.38, 1.48, 1.38, 1.17, 0.81, 1.44, 1.87, 1.39, 1.54, 1.91, 1.15, 1.4, 1.53, 1.69, Inf, 1.97, 1.65, 1.44, 1.83, 1.5, 1.42, 1.6, 1.7, 1.82, 1.46, 1.67, 1.99, 1.42, 1.3, 1.28, 1.25, 1.49, 1.4, 1, 1.53, 1.53, 1.4, 1.16, 1.33, 1.85, 1.1, 1.14, 0.81, 0.71, 0.77, 0.74, 0.47, 0.85, 0.67, 0.62, 0.74, 0.93, 0.89, 0.97, 0.77, 0.5, 0.45, 0.65, 0.77, 0.65, 0.65, 0.67, 0.65, 0.63, 0.62, 0.66, 0.77, 0.77, 0.66, 0.66, 0.59, 0.63, 0.56, 0.65, 0.53, 0, 0.37, 0.31, 0.38, 0.51, 0.6, 0.72, 0.87, 0.75, 0.56, 0.8, 0.72, 0.89, 0.98, 1.14, 1.08, 1.3, 0.91, 0.71, 1.17, 0.92, 0.69, 0.45, 0.45, 0.47, 0.48, 0.49, 0.43, 0.55, 0.39, 0.47, 0.55, 0.69, 0.47, 0.7, 0.67, 0.48, 0.44, 0.47, 0.54, 0.59, 0.85, 1.28, 0.57, 0.88, 1.13, 1.16, 1.18, 1.06, 1.21, 0.97, 1.18, 1.19, 1.2, 1.2, 1.2, 1.2, 1.32, 1.21, 1.21, 1.2, 1.21, 1.2, 1.2, 1.2, 1.21, 1.21, 1.19, 1.2, 1.2, 1.19))
})
