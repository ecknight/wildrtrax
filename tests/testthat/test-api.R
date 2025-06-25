library(testthat)

aoi <- list(
  c(-112.85438, 57.13472),
  c(-113.14364, 54.74858),
  c(-112.69368, 52.34150),
  c(-112.85438, 57.13472),
  c(-112.85438, 57.13472)
)

bad_aoi <- list(
  c(-112.85438, 53.13472),
  c(-113.14364, 54.74858),
  c(-112.69368, 52.34150),
  c(-112.854385, 57.13472),
  c(-112.85438, 57.13472)
)

test_that("Download without authentication or boundary; single-species", {
  expect_true(!is.null(wt_dd_summary(sensor = 'ARU', species = 'White-throated Sparrow', boundary = NULL)))
})

test_that("Download without authentication or boundary; single-species", {
  expect_true(!is.null(wt_dd_summary(sensor = 'ARU', species = 'White-throated Sparrow', boundary = aoi)))
})

test_that("Download without authentication, bad boundary; single-species", {
  expect_error(!is.null(wt_dd_summary(sensor = 'ARU', species = 'White-throated Sparrow', boundary = bad_aoi)))
})

test_that("Download without authentication, boundary; multiple single-species", {
  expect_true(!is.null(wt_dd_summary(sensor = 'ARU', species = c('White-throated Sparrow','Hermit Thrush'), boundary = aoi)))
})

Sys.setenv(WT_USERNAME = "guest", WT_PASSWORD = "Apple123")
wt_auth(force = TRUE)

test_that("Download with authentication with boundary; single-species", {
  expect_true(!is.null(wt_dd_summary(sensor = 'ARU', species = 'White-throated Sparrow', boundary = aoi)))
})

test_that("Download with authentication, bad boundary; single-species", {
  expect_error(wt_dd_summary(sensor = 'ARU', species = 'White-throated Sparrow', boundary = bad_aoi))
})

test_that("Download with authentication with boundary; multiple species logged in", {
  expect_true(!is.null(wt_dd_summary(sensor = 'ARU', species = c('White-throated Sparrow','Townsend\'s Warbler'), boundary = aoi)))
})

# test_that("Test project-species", {
#   expect_true(!is.null(wt_get_project_species(2460)))
# })

test_that("Get functions", {
  expect_true(!is.null(wt_get_recordings('GUEST')))
  expect_true(!is.null(wt_get_project_species(3286)))
})

# Possible test but not necessary due to length it takes to run
# test_that("Timeout test", {
#   expect_true(!is.null(wt_download_report(197, 'CAM', 'main', F, max_seconds = 3000)))
# })

test_that("Get functions for all API combinations with specific project restrictions", {

  # Set environment variables and authenticate
  Sys.setenv(WT_USERNAME = "guest", WT_PASSWORD = "Apple123")
  wt_auth(force = TRUE)

  # Test for each API
  expect_no_error(wt_get_sync(api = "download-location-by-org-id", organization = 5205))
  expect_no_error(wt_get_sync(api = "download-visits-by-org-id", organization = 5205))
  expect_no_error(wt_get_sync(api = "download-equipment-by-org-id" = organization = 5205))
  #expect_no_error(wt_get_sync(api = "download-location-equipment-by-org-id" = organization = 5205))
  expect_no_error(wt_get_sync(api = "download-location", option = "columns", project = 2))
  expect_no_error(wt_get_sync(api = "download-tasks-by-project-id", option = "columns", project = 2))
  expect_no_error(wt_get_sync(api = "download-tags-by-project-id", option = "columns", project = 2))
  # Uncomment if testing camera tasks is applicable
  # expect_no_error(wt_get_sync_columns(api = "download-camera-tasks-by-project-id", option = "columns"))
  expect_no_error(wt_get_sync(api = "download-camera-tags-by-project-id", option = "columns", project = 252))
  expect_no_error(wt_get_sync(api = "download-point-count-by-project-id", option = "columns", project = 252))

})


