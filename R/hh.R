library(lubridate)
library(dplyr)
library(readr)

# Read and preprocess the deployments data
deployments <- read_csv("/users/alexandremacphail/desktop/JEM_Cameras_ARUs_Deployment_2024_JLO2.csv") |>
  mutate(location = paste0(Site, "-CA", Quadrant)) |>
  select(location, `ARU File Prefix`, `Field Date`, `ARU Time Set`) |>
  distinct() |>
  mutate(
    match_name = location == `ARU File Prefix`,
    visit_date_time = force_tz(ymd_hms(paste0(`Field Date`, " ", `ARU Time Set`)), tzone = "America/Denver")
  ) |>
  select(location, visit_date_time, match_name)

# Read and preprocess the BADR data
badr24 <- wt_audio_scanner("/Volumes/BUpublic/ABMI/ARU/BADR/2024/V1/19-2G2", file_type = "all", extra_cols = FALSE)

# Processing pipeline
locs_to_process <- badr24 |>
  mutate(recording_date_time = force_tz(recording_date_time, tzone = "America/Denver")) |>
  arrange(recording_date_time) |>
  group_by(location) |>
  slice_head(n = 1) |>
  select(location, recording_date_time) |>
  inner_join(deployments, by = "location") |>
  mutate(diff = as.numeric(difftime(visit_date_time, recording_date_time, units = "mins"))) |>
  filter(between(diff,-45,45) | match_name == FALSE) |>
  pull(location)

badr24 |>
  filter(location %in% locs_to_process) |>
  mutate(
    hour = hour(recording_date_time),
    typ = case_when(
      hour %in% 4:7 ~ "Dawn",
      hour %in% 20:23 ~ "Dusk",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(typ), julian %in% c(135:190)) |>
  group_by(location, typ) |>
  group_modify(~ {
    if (.y$typ == "Dawn") {
      slice_sample(.x, n = 4)
    } else if (.y$typ == "Dusk") {
      slice_sample(.x, n = 2)
    } else {
      .x
    }
  }) |>
  ungroup() %>%
  map(.x = .$file_path, .f = ~file.copy(.x, to = "/users/alexandremacphail/desktop/badr24"))


# Read and preprocess the BADR data
badr24 <- wt_audio_scanner("/Volumes/BUpublic/ABMI/ARU/BADR/2024/V1/ftplink/4-2G1", file_type = "all", extra_cols = FALSE)
badr24_1 <- wt_audio_scanner("/Volumes/BUpublic/ABMI/ARU/BADR/2024/V1/ftplink/8-1A1", file_type = "all", extra_cols = FALSE)
badr24_2 <- wt_audio_scanner("/Volumes/BUpublic/ABMI/ARU/BADR/2024/V1/ftplink/8-1B1", file_type = "all", extra_cols = FALSE)
badr24_3 <- wt_audio_scanner("/Volumes/BUpublic/ABMI/ARU/BADR/2024/V1/ftplink/8-2A1", file_type = "all", extra_cols = FALSE)
badr24_4 <- wt_audio_scanner("/Volumes/BUpublic/ABMI/ARU/BADR/2024/V1/ftplink/8-2B1", file_type = "all", extra_cols = FALSE)
badr24_5 <- wt_audio_scanner("/Volumes/BUpublic/ABMI/ARU/BADR/2024/V1/ftplink/8-2C1", file_type = "all", extra_cols = FALSE)
badr24_6 <- wt_audio_scanner("/Volumes/BUpublic/ABMI/ARU/BADR/2024/V1/ftplink/8-2C2", file_type = "all", extra_cols = FALSE)
badr24_7 <- wt_audio_scanner("/Volumes/BUpublic/ABMI/ARU/BADR/2024/V1/ftplink/8-2E2", file_type = "all", extra_cols = FALSE)
badr24_8 <- wt_audio_scanner("/Volumes/BUpublic/ABMI/ARU/BADR/2024/V1/ftplink/8-2E3", file_type = "all", extra_cols = FALSE)
badr24_9 <- wt_audio_scanner("/Volumes/BUpublic/ABMI/ARU/BADR/2024/V1/ftplink/8-2F3", file_type = "all", extra_cols = FALSE)
badr24_10 <- wt_audio_scanner("/Volumes/BUpublic/ABMI/ARU/BADR/2024/V1/ftplink/8-2F4", file_type = "all", extra_cols = FALSE)

allbadr <- bind_rows(badr24, badr24_1, badr24_2, badr24_3, badr24_4, badr24_5, badr24_6, badr24_7, badr24_8, badr24_9, badr24_10)

# Processing pipeline
locs_to_process <- allbadr |>
  mutate(recording_date_time = force_tz(recording_date_time, tzone = "America/Denver")) |>
  arrange(recording_date_time) |>
  group_by(location) |>
  slice_head(n = 1) |>
  select(location, recording_date_time) |>
  inner_join(deployments, by = "location") |>
  mutate(diff = as.numeric(difftime(visit_date_time, recording_date_time, units = "mins"))) |>
  filter(between(diff,-45,45) | match_name == FALSE) |>
  pull(location)

allbadr |>
  filter(location %in% locs_to_process, grepl('^8-2E3|^8-2F3|^8-2F4',location)) |>
  mutate(
    hour = hour(recording_date_time),
    typ = case_when(
      hour %in% 4:7 ~ "Dawn",
      hour %in% 20:23 ~ "Dusk",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(typ), julian %in% c(135:190)) |>
  group_by(location, typ) |>
  group_modify(~ {
    if (.y$typ == "Dawn") {
      slice_sample(.x, n = 4)
    } else if (.y$typ == "Dusk") {
      slice_sample(.x, n = 2)
    } else {
      .x
    }
  }) |>
  ungroup() %>%
  map(.x = .$file_path, .f = ~file.copy(.x, to = "/users/alexandremacphail/desktop/badr24"))

