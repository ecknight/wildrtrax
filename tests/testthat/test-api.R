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
  expect_true(!is.null(wt_get_locations('GUEST')))
  expect_true(!is.null(wt_get_visits('GUEST')))
  expect_true(!is.null(wt_get_recordings('GUEST')))
  expect_true(!is.null(wt_get_project_species(3286)))
})

# Possible test but not necessary due to length it takes to run
# test_that("Timeout test", {
#   expect_true(!is.null(wt_download_report(197, 'CAM', 'main', F, max_seconds = 3000)))
# })

.wt_api_pr_dev <- function(path, ...) {
  # Check if authentication has expired
  if (.wt_auth_expired()) {
    stop("Please authenticate with wt_auth().", call. = FALSE)
  }

  # Generate user agent
  u <- .gen_ua()

  # Convert ... into a list for the request body
  request_body <- list(...)

  # Construct the request
  r <- request("https://dev-api.wildtrax.ca") |>
    req_url_path_append(path) |>
    req_headers(
      Authorization = paste("Bearer", ._wt_auth_env_$access_token),
      `Content-Type` = "application/json"
    ) |>
    req_user_agent(u) |>
    req_body_json(request_body) |>  # Pass the request body as JSON
    req_method("POST") |>
    req_perform()

  # Handle errors
  if (resp_status(r) >= 400) {
    error_message <- tryCatch(
      resp_body_json(r)$message,
      error = function(e) "Unknown error"
    )
    stop(sprintf(
      "Request failed [%s]: %s",
      resp_status(r),
      error_message
    ), call. = FALSE)
  }

  # Parse and return the response as JSON
  return(resp_body_json(r))
}

# Set up the users and their corresponding passwords
users <- c("agmacpha@ualberta.ca", "guest")
passwords <- c("", "Apple123")

# Initialize an empty list to store the results
all_results <- list()

# Loop through the users
for (i in seq_along(users)) {

  # Set environment variables for the current user
  Sys.setenv(WT_USERNAME = users[i], WT_PASSWORD = passwords[i])

  # Authenticate for the current user
  wt_auth(force = TRUE)

  # Fetch projects
  r <- .wt_api_pr_dev(
    path = "/bis/get-projects",
    sensorIds = list("ARU"),
    orderBy = "id",
    orderDirection = "desc",
    limit = 1e9,
    page = 1,
    isProjectMember = NULL
  )

  # Clean the response
  r <- clean_api_response(r)

  # Convert results to tibble
  results_tibble <- map_dfr(r$results, as_tibble)

  # Lookup table for status
  lookup_table <- tibble(
    id = c(1, 2, 3, 4, 5, 6),
    type = c("Published - Public", "Active", "Published - Private",
             "Published - Map+Report Only", "Test Only", "Published - Map Only")
  )

  # Extract the user's name, or use the email as a placeholder
  user_name <- users[i]  # You can replace this with the actual extraction logic if needed

  # Add a new column with the user's name
  user_results <- results_tibble |>
    inner_join(lookup_table, by = c("statusId" = "id")) |>
    mutate(name = user_name) |>  # Add the user name column
    select(name, isAdmin, isProjectMember, type, canRequestAccess, canUserOpenProjectLink, grantedProjectAccess) |>
    group_by(type) |>
    slice(1) |>
    ungroup()

  # Append the results for this user to the list
  all_results <- append(all_results, list(user_results))
}

# Combine all the user results into a single tibble
final_results_tibble <- bind_rows(all_results)

# View the final combined results
print(final_results_tibble)

