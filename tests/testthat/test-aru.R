library(testthat)

Sys.setenv(WT_USERNAME = "guest", WT_PASSWORD = "Apple123")
wt_auth(force = TRUE)
cypress_hills <- wt_download_report(620, 'ARU', 'main', FALSE)

################################### ARU Test suite

test_that("Authentication works correctly", {
  expect_true(!is.null(wt_get_projects(sensor = 'ARU')))
  })

test_that("Downloading ARU report", {
  expect_true(!is.null(cypress_hills))
})

test_that("Testing a Private Project", {
  expect_error(wt_download_report(1373, 'ARU', 'main', FALSE)==0)
})

test_that("Downloading ARU as PC report", {
  cypress_hills_as_pc <- wt_download_report(620, 'PC', 'main', FALSE)
  expect_true(!is.null(cypress_hills_as_pc))
})

test_that("Attempting PC as ARU report", {
  expect_true(nrow(wt_download_report(887, 'ARU', 'main', FALSE))==0)
})

test_that("Tidying species zero-filling true", {
  cypress_hills_tidy <- wt_tidy_species(cypress_hills, remove = c("abiotic"), zerofill = T)
  expect_true(nrow(cypress_hills_tidy) < nrow(cypress_hills))
})

test_that("Tidying species zero-filling false", {
  cypress_hills_tidy_f <- wt_tidy_species(cypress_hills, remove = c("mammal", "abiotic", "amphibian","unknown"), zerofill = F)
  expect_true(nrow(cypress_hills_tidy_f) < nrow(cypress_hills))
})

test_that("Replacing TMTT", {
  cypress_hills_tidy <- wt_tidy_species(cypress_hills, remove = c("mammal", "abiotic", "amphibian", "unknown"), zerofill = T)
  cypress_hills_tmtt <- wt_replace_tmtt(cypress_hills_tidy, calc = "round") |>
    select(individual_count) |>
    distinct()
  expect_true(!('TMTT' %in% cypress_hills_tmtt))

})

test_that('Making wide', {
  cypress_hills_tidy <- wt_tidy_species(cypress_hills, remove = c("mammal", "abiotic", "amphibian", "unknown"), zerofill = T)
  cypress_hills_tmtt <- wt_replace_tmtt(cypress_hills_tidy, calc = "round")
  cypress_hills_wide <- wt_make_wide(cypress_hills_tmtt, sound="all")
  expect_true(ncol(cypress_hills_wide) > ncol(cypress_hills_tmtt))
})

test_that('Getting QPAD offsets', {
  library(QPAD)
  cypress_hills_tidy <- wt_tidy_species(cypress_hills, remove = c("mammal", "abiotic", "amphibian", "unknown"), zerofill = T)
  cypress_hills_tmtt <- wt_replace_tmtt(cypress_hills_tidy, calc = "round")
  cypress_hills_wide <- wt_make_wide(cypress_hills_tmtt, sound = "all")
  cypress_hills_qpad <- wt_qpad_offsets(cypress_hills_wide, species = "all", version = 3, together = F)
  expect_true(ncol(cypress_hills_qpad) > 1)
})

test_that('Occupancy formatting', {
  cypress_hills_tidy <- wt_tidy_species(cypress_hills, remove = c("mammal", "abiotic", "amphibian"), zerofill = T)
  cypress_hills_tmtt <- wt_replace_tmtt(cypress_hills_tidy, calc = "round")
  occu <- wt_format_occupancy(cypress_hills_tmtt, species = "OVEN")
  expect_true(class(occu)[1] == 'unmarkedFrameOccu')
})

test_that('Classifier functions', {
  rep <- wt_download_report(620, 'ARU', c('main','ai'), F)
  #eval <- wt_evaluate_classifier(rep, "recording", remove_species = TRUE, thresholds = c(10,99))
  #e1 <- wt_classifier_threshold(eval)
  add_sp <- wt_additional_species(rep, remove_species = TRUE, threshold = 0.8, resolution = "task")
  expect_true(!is.null(add_sp))
})

test_that('Add GRTS ID', {
  bats <- wt_download_report(685, 'ARU', 'location', F)
  grts <- wt_add_grts(bats, group_locations_in_cell = TRUE)
  expect_true(!is.null(grts))
})

test_that('Location distances', {
  locs <- wt_download_report(620, 'ARU', 'location', F)
  locs_dist <- wt_location_distances(locs)
  expect_true(!is.null(locs_dist))
})

# ##wt_audio_scanner
# test_that('Scanner', {
#   url <- 'https://raw.githubusercontent.com/ABbiodiversity/wildRtrax-assets/main/ABMI-1046-NW_20240313_110010.wav'
#   req <- request(url) |>
#     req_perform()
#   file_path <- 'ABMI-1046-NW_20240313_110010.wav'  # Define the path
#   writeBin(req$body, file_path)
#   j <- wt_audio_scanner(".", file_type = "wav", extra_cols = F)
#   expect_true(nrow(j) == 1)
# })

# ##wt_run_ap
# test_that('AP', {
#   url <- 'https://raw.githubusercontent.com/ABbiodiversity/wildRtrax-assets/main/ABMI-1046-NW_20240313_110010.wav'
#   req <- request(url) |>
#     req_perform()
#   file_path <- 'ABMI-1046-NW_20240313_110010.wav'  # Define the path
#   writeBin(req$body, file_path)
#   j <- wt_audio_scanner(".", file_type = "wav", extra_cols = F)
#   wt_run_ap(j, output_dir = getwd(), path_to_ap = "/users/alexandremacphail/APN/AnalysisPrograms")
# })

##wt_glean_ap
##wt_chop

