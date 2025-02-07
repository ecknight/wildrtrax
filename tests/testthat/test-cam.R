library(testthat)

Sys.setenv(WT_USERNAME = "guest", WT_PASSWORD = "Apple123")
wt_auth(force = TRUE)

################################### Camera Test suite

test_that("Downloading CAM report", {
  abmi_amph_cam <- wt_download_report(391, 'CAM', c('megadetector'), FALSE)
  expect_true(!is.null(abmi_amph_cam))
})

test_that("Downloading CAM report", {
  suppressMessages(abmi_amph_cam <- wt_download_report(391, 'CAM', c('tag','image_set'), FALSE))
  expect_true(length(abmi_amph_cam) == 2)
})

# Test other reports
test_that("Downloading CAM report", {
  abmi_amph_cam <- wt_download_report(391, 'CAM', c('project','location', 'image_set','image_report','megaclassifier','megadetector'), FALSE)
  expect_true(!is.null(abmi_amph_cam))
})


test_that("Individual detections", {
  abmi_amph_cam <- wt_download_report(391, 'CAM', 'main', FALSE)
  ind_detections <- wt_ind_detect(abmi_amph_cam, threshold = 60, units = "minutes", datetime_col = image_date_time, remove_human = TRUE, remove_domestic = TRUE)
  expect_true(nrow(abmi_amph_cam) > nrow(ind_detections))

})

test_that("Summarise cam", {
  abmi_amph_cam <- wt_download_report(391, 'CAM', 'main', FALSE)
  ind_detections <- wt_ind_detect(abmi_amph_cam, threshold = 10, units = "minutes", datetime_col = image_date_time, remove_human = TRUE, remove_domestic = TRUE)
  summary <- wt_summarise_cam(detect_data = ind_detections, raw_data = abmi_amph_cam, time_interval = "month", variable = "detections", output_format = "wide")
})

test_that("Summarise cam", {

  df <- wt_download_report(
    project_id = 391,
    sensor_id = "CAM",
    report = "main",
    weather_cols = FALSE
  )

  ind_detec <- wt_ind_detect(x = df, threshold = 30, units = "minutes")

  summary <- wt_summarise_cam(
    detect_data = ind_detec,
    raw_data = df,
    time_interval = "month",
    variable = "counts",
    output_format = "long")

  ind_detec.focal <- ind_detec |>
    filter(location=="OGW-ABMI-1057-71-11" & species_common_name=="Moose")
  summary.focal <- summary |>
    filter(location=="OGW-ABMI-1057-71-11" & species_common_name=="Moose",!value==0) |>
    select(value)

  expected <- ind_detec.focal |>
    mutate(month = month(start_time, label = F)) |>
    group_by(month) |>
    summarise(total_count=sum(max_animals)) |>
    select(total_count)

  expected
  summary.focal

  expect_true(identical(sort(expected$total_count), sort(summary.focal$value)))
})
