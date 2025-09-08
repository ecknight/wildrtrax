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

# test_that("Download without authentication or boundary; single-species", {
#   expect_true(!is.null(wt_dd_summary(sensor = 'ARU', species = 'White-throated Sparrow', boundary = NULL)))
# })
#
# test_that("Download without authentication or boundary; single-species", {
#   expect_true(!is.null(wt_dd_summary(sensor = 'ARU', species = 'White-throated Sparrow', boundary = aoi)))
# })
#
# test_that("Download without authentication, bad boundary; single-species", {
#   expect_error(!is.null(wt_dd_summary(sensor = 'ARU', species = 'White-throated Sparrow', boundary = bad_aoi)))
# })
#
# test_that("Download without authentication, boundary; multiple single-species", {
#   expect_true(!is.null(wt_dd_summary(sensor = 'ARU', species = c('White-throated Sparrow','Hermit Thrush'), boundary = aoi)))
# })

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

# Possible test but not necessary due to length it takes to run
# test_that("Timeout test", {
#   expect_true(!is.null(wt_download_report(197, 'CAM', 'main', F, max_seconds = 3000)))
# })

test_that("Get functions for all API combinations with specific project restrictions", {
  # Set environment variables and authenticate
  Sys.setenv(WT_USERNAME = "guest", WT_PASSWORD = "Apple123")
  wt_auth(force = TRUE)

  # Test for each API using pseudonyms
  expect_no_error(wt_get_sync(api = "organization_locations", organization = 5205))
  #expect_no_error(wt_get_sync(api = "organization_visits", organization = 5205))
  #expect_no_error(wt_get_sync(api = "organization_equipment", organization = 5205))
  #expect_no_error(wt_get_sync(api = "organization_location_equipment", organization = 5205))
  #expect_no_error(wt_get_sync(api = "organization_task_creator", organization = 5205))
  expect_no_error(wt_get_sync(api = "organization_image_summary", organization = 5205))
  #expect_no_error(wt_get_sync(api = "project_locations", option = "columns", project = 2))
  #expect_no_error(wt_get_sync(api = "project_aru_tasks", option = "columns", project = 2))
  #expect_no_error(wt_get_sync(api = "project_aru_tags", option = "columns", project = 2))
  #expect_no_error(wt_get_sync(api = "project_camera_tasks", option = "columns", project = 252))
  #expect_no_error(wt_get_sync(api = "project_camera_tags", option = "columns", project = 220))
  #expect_no_error(wt_get_sync(api = "project_point_counts", option = "columns", project = 804))
  #expect_no_error(wt_get_sync(api = "project_species", project = 2))
})



