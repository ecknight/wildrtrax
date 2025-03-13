library(httr2)

wt_civ <- function(project_id) {

  # Check if authentication has expired:
  if (.wt_auth_expired())
    stop("Please authenticate with wt_auth().", call. = FALSE)

  r <- .wt_api_gr(
    path = "/bis/get-camera-image-view",
    sortAsc = "true",
    limit = 60,
    sort = "date",
    pudId = 64066,
    page = 64,
    triggerModeId = c(2, -1),
    megaDetectorThreshold = 0,
    isVerified = NULL,
    useSeriesView = "true",
    .multi = TRUE
  )

  if (r$status_code == 403) {
    stop("Permission denied: You do not have access to request this data.", call. = FALSE)
    return(NULL)
  }

  x <- resp_body_json(r)
}
