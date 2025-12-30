library(testthat)

Sys.setenv(WT_USERNAME = "guest", WT_PASSWORD = "Apple123")
wt_auth(force = TRUE)
bats <- wt_download_report(685, 'ARU', 'location')
locs <- wt_download_report(620, 'ARU', 'location')

test_that('Add GRTS ID', {
  grts <- wt_add_grts(bats, group_locations_in_cell = TRUE)
  expect_true(!is.null(grts))
})

test_that('Location distances', {
  locs_dist <- wt_location_distances(locs)
  expect_true(!is.null(locs_dist))
})
