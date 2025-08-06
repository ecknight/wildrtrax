#' Authenticate into WildTrax
#'
#' @description Obtain Auth0 credentials using WT_USERNAME and WT_PASSWORD stored as environment variables
#'
#' @param force Logical; whether or not the force re-authentication even if token has not expired. Defaults to FALSE.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Authenticate first:
#' wt_auth(force = FALSE)
#' }
#'

wt_auth <- function(force = FALSE) {

  if (!exists("._wt_auth_env_"))
    stop("Cannot find the correct environment.", call. = TRUE)

  if (force || .wt_auth_expired())
    .wt_auth()

  invisible(NULL)

}

#' Get a project summary from WildTrax
#'
#' @description Obtain a table listing projects that the user is able to download data for
#'
#' @param sensor Can be one of "ARU", "CAM", or "PC"
#'
#' @import dplyr
#' @importFrom httr2 resp_body_json
#
#' @export
#'
#' @examples
#' \dontrun{
#' # Authenticate first:
#' wt_auth()
#' wt_get_projects(sensor_id = "ARU")
#' }
#'
#' @return A data frame listing the projects that the user can download data for, including: project name, id, year, number of tasks, a geographic bounding box and project status.
#'

wt_get_projects <- function(sensor) {

  sens <- c("PC", "ARU", "CAM")

  # Stop call if sensor id value is not within range of possible values
  if(!sensor %in% sens) {
    stop("A valid value for sensor_id must be supplied. See ?wt_get_download_summary for a list of possible values", call. = TRUE)
  }

  r <- .wt_api_pr(
    path = "/bis/get-projects",
    sensorId = sensor
  )

  if(is.null(r)) {stop('No response was returned or the response was NULL.')}

  statuses <- resp_body_json(r)$options$statusIds |>
    map_dfr(as_tibble)

  orgs <- resp_body_json(r)$options$organizationIds |>
    map_dfr(as_tibble)

  projects <- as_tibble(do.call(rbind, resp_body_json(r)$results)) |>
    unnest(everything()) |>
    rename(project_id = id) |>
    rename(organization_id = organizationId) |>
    rename(project = fullNm) |>
    filter(sensorId == sensor) |>
    rename(project_sensor = sensorId) |>
    inner_join(statuses, by = c("statusId" = "id")) |>
    rename(project_status = type) |>
    inner_join(orgs, by = c("organization_id" = "id")) |>
    rename(organization_name = name) |>
    rename(project_creation_date = creationDate) |>
    rename(project_due_date = dueDate) |>
    rename(task_count = tasks) |>
    rename(tasks_completed = tasksCompleted) |>
    select(organization_id, organization_name, project_sensor, project_id, project, project_status, project_creation_date, project_due_date, task_count, tasks_completed) |>
    distinct()

  return(projects)

}

#' Download formatted reports from WildTrax
#'
#' @description Download various ARU, camera, or point count data from projects across WildTrax
#'
#' @param project_id Numeric; the project ID number that you would like to download data for. Use `wt_get_download_summary()` to retrieve these IDs.
#' @param sensor_id Character; Can either be "ARU", "CAM", or "PC".
#' @param reports Character; The report type to be returned. Multiple values are accepted as a concatenated string.
#' @param weather_cols Logical; Do you want to include weather information for your stations? Defaults to TRUE.
#' @param max_seconds Numeric; Number of seconds to force to wait for downloads.
#' @details Valid values for argument \code{report} when \code{sensor_id} = "CAM" currently are:
#' \itemize{
#'  \item main
#'  \item project
#'  \item location
#'  \item image_report
#'  \item image_set_report
#'  \item tag
#'  \item megadetector
#'  \item definitions
#' }
#' @details Valid values for argument \code{report} when \code{sensor_id} = "ARU" currently are:
#' \itemize{
#'  \item main
#'  \item project
#'  \item location
#'  \item recording
#'  \item tag
#'  \item ai
#'  \item definitions
#' }
#' @details Valid values for argument \code{report} when \code{sensor_id} = "PC" currently are:
#' \itemize{
#'  \item main
#'  \item project
#'  \item location
#'  \item point_count
#'  \item definitions
#' }
#'
#' @import httr2 purrr dplyr
#' @importFrom readr read_csv col_character col_logical
#' @export
#'
#' @examples
#' \dontrun{
#' # Authenticate first:
#' wt_auth()
#' a_camera_project <- wt_download_report(
#' project_id = 397, sensor_id = "CAM", reports = c("tag", "image_set_report"),
#' weather_cols = TRUE)
#'
#' an_aru_project <- wt_download_report(
#' project_id = 47, sensor_id = "ARU", reports = c("main", "ai"),
#' weather_cols = TRUE)
#' }
#'
#' @return If multiple report types are requested, a list object is returned; if only one, a dataframe.
#'

wt_download_report <- function(project_id, sensor_id, reports, weather_cols = TRUE, max_seconds=300) {

  # Check if authentication has expired:
  if (.wt_auth_expired())
    stop("Please authenticate with wt_auth().", call. = FALSE)

  sens <- sensor_id

  # Check if the project_id is valid:
  i <- wt_get_projects(sensor_id) |>
    tibble::as_tibble() |>
    dplyr::select(project_id, project_sensor)

  sensor_value <- i |>
    dplyr::rename('id' = 1) |>
    dplyr::filter(id %in% project_id) |>
    dplyr::pull(project_sensor)

  if (!project_id %in% i$project_id) {
    stop("The project_id you specified is not among the projects you are able to download for.", call. = TRUE)
  }

  # Make sure report is specified
  if(missing(reports)) {
    stop("Please specify a report type (or multiple) using the `report` argument. Use ?wt_download_report to view options.",
         call. = TRUE)
  }

  # Allowable reports for each sensor
  cam <- c("main", "project", "location", "image_set_report", "image_report", "tag", "megadetector", "daylight_report", "definitions")
  aru <- c("main", "project", "location", "ai", "recording", "tag", "definitions")
  pc <- c("main", "project", "location", "point_count", "daylight_report", "definitions")

  # Check that the user supplied a valid report type depending on the sensor
  if(sensor_id == "CAM" & !all(reports %in% cam)) {
    stop("Please supply a valid report type. Use ?wt_download_report to view options.", call. = TRUE)
  }

  if(sensor_id == "ARU" & !all(reports %in% aru)) {
    stop("Please supply a valid report type. Use ?wt_download_report to view options.", call. = TRUE)
  }

  if(sensor_id == "PC" & !all(reports %in% pc)) {
    stop("Please supply a valid report type. Use ?wt_download_report to view options.", call. = TRUE)
  }

  # Prepare temporary file:
  tmp <- tempfile(fileext = ".zip")

  # tmp directory
  td <- tempdir()

  query_list <- list()

  r <- .wt_api_pr(
    path = "/bis/download-report",
    projectIds = project_id,
    sensorId = sensor_id,
    mainReport = if ("main" %in% reports) query_list$mainReport <- TRUE,
    projectReport = if ("project" %in% reports) query_list$projectReport <- TRUE,
    recordingReport = if ("recording" %in% reports) query_list$recordingReport <- TRUE,
    pointCountReport = if ("point_count" %in% reports) query_list$pointCountReport <- TRUE,
    locationReport = if ("location" %in% reports) query_list$locationReport <- TRUE,
    tagReport = if ("tag" %in% reports) query_list$tagReport <- TRUE,
    imageReport = if ("image_report" %in% reports) query_list$imageReport <- TRUE,
    imageSetReport = if ("image_set_report" %in% reports) query_list$imageSetReport <- TRUE,
    aiReport = if ("ai" %in% reports) query_list$aiReport <- TRUE,
    megaDetectorReport = if ("megadetector" %in% reports) query_list$megaDetectorReport <- TRUE,
    dayLightReport = if ("daylight_report" %in% reports) query_list$dayLightReport <- TRUE,
    includeMetaData = TRUE,
    splitLocation = TRUE,
    max_time = max_seconds
  )

  writeBin(httr2::resp_body_raw(r), tmp)

  # Unzip
  unzip(tmp, exdir = td)

  # Remove abstract file
  abstract <- list.files(td, pattern = "*_abstract.csv", full.names = TRUE, recursive = TRUE)
  file.remove(abstract)

  # Remove special characters from project names
  list.files(td, pattern = "*.csv", full.names = TRUE) %>% map(~ {
    directory <- dirname(.x)
    old_filename <- basename(.x)
    new_filename <- gsub("[:()?!~;/,]", "", old_filename)
    new_path <- file.path(directory, new_filename)
    file.rename(.x, new_path)
  })

  files.full <- list.files(td, pattern= "*.csv", full.names = TRUE)
  files.less <- basename(files.full)
  x <- purrr::map(.x = files.full, .f = ~ suppressWarnings(readr::read_csv(., show_col_types = FALSE, skip_empty_rows = TRUE, col_types = .wt_col_types, progress = FALSE))) %>%
    purrr::set_names(files.less)

  # Remove weather columns, if desired
  if(weather_cols) {
    x
  } else {
    x <- purrr::map(.x = x, .f = ~ (.x[, !grepl("^daily|^hourly", colnames(.x))]))
  }

  # Return the requested report(s)
  report <- paste(paste0("_",reports), collapse = "|")
  x <- x[grepl(report, names(x))]
  # Return a data frame if only 1 element in the list (i.e., only 1 report requested)
  if (length(x) == 1) {
    x <- x[[1]]
  } else {
    x
  }

  # Delete csv files
  file.remove(files.full)
  # Delete tmp
  file.remove(tmp)

  return(x)
}

#' Get the WildTrax species table
#'
#' @description Request for the WildTrax species table
#'
#'
#' @import purrr
#' @export
#'
#' @examples
#'  \dontrun{
#'  wt_species <- wt_get_species()
#'  }
#' @return A tibble of the WildTrax species table
#'

wt_get_species <- function(){

  # Check if authentication has expired:
  if (.wt_auth_expired())
    stop("Please authenticate with wt_auth().", call. = FALSE)

  spp <- .wt_api_gr(
    path = "/bis/get-all-species"
  )

  spp <- resp_body_json(spp)

  spp_table <- tibble(
    species_id = map_dbl(spp, ~ ifelse(!is.null(.x$id), .x$id, NA)),
    species_code = map_chr(spp, ~ ifelse(!is.null(.x$code), .x$code, NA)),
    species_common_name = map_chr(spp, ~ ifelse(!is.null(.x$commonName), .x$commonName, NA)),
    species_class = map_chr(spp, ~ ifelse(!is.null(.x$className), .x$className, NA)),
    species_order = map_chr(spp, ~ ifelse(!is.null(.x$order), .x$order, NA)),
    species_scientific_name = map_chr(spp, ~ ifelse(!is.null(.x$scientificName), .x$scientificName, NA))
  )

  return(spp_table)

}

#' Download media
#'
#' @description Download acoustic and image media in batch. Includes the download of tag clips and spectrograms for the ARU sensor.
#'
#' @param input The report data
#' @param output The output folder
#' @param type Either recording, image, tag_clip_spectrogram, tag_clip_audio
#'
#' @import dplyr tibble purrr
#' @importFrom httr2 request req_perform
#' @export
#'
#' @examples
#' \dontrun{
#' dat.report <- wt_download_report() |>
#' wt_download_media(output = "my/output/folder", type = "recording")
#' }
#'
#' @return An organized folder of media. Assigning wt_download_tags to an object will return the table form of the data with the functions returning the after effects in the output directory

wt_download_media <- function(input, output, type = c("recording","image", "tag_clip_audio","tag_clip_spectrogram")) {

  # Check if authentication has expired:
  if (.wt_auth_expired())
    stop("Please authenticate with wt_auth().", call. = FALSE)

  input_data <- input

  # Check if input_data is provided and in the correct format
  if (missing(input_data) || !is.data.frame(input_data) && !is.matrix(input_data)) {
    stop("Input data must be provided as a data frame or matrix.")
  }

  # Check if output directory exists
  if (!dir.exists(output)) {
    stop("Output directory does not exist.")
  }

  # Check if type is valid
  valid_types <- c("recording", "image", "tag_clip_audio","tag_clip_spectrogram")
  if (!type %in% valid_types) {
    stop("Invalid type. Valid types are 'recording', 'image', 'tag_clip_spectrogram', or 'tag_clip_audio'.")
  }

  download_file <- function(url, output_file) {
    req <- httr2::request(url)
    httr2::req_perform(req, path = output_file)
    return(paste("Downloaded to", output_file))
  }

  # Process based on type
  # Full recording
  if (type == "recording" & "recording_url" %in% colnames(input_data)) {
    output_data <- input_data |>
      mutate(
        file_type = sub('.*\\.(\\w+)$', '\\1', basename(recording_url)),
        clip_file_name = paste0(output, "/", location, "_", format(recording_date_time, "%Y%m%d_%H%M%S"), ".", file_type)
      ) %>%
      { purrr::map2_chr(.$recording_url, .$clip_file_name, download_file) }
    # Tag spectrograms
  } else if (type == "tag_clip_spectrogram" & "spectrogram_url" %in% colnames(input_data)) {
    output_data <- input_data |>
      mutate(
        detection_time = gsub("\\.", "_", as.character(detection_time)),
        clip_file_name = file.path(output, paste0(
          organization, "_", location, "_", format(recording_date_time, "%Y%m%d_%H%M%S"), "__",
          species_code, "__", individual_order, "__", detection_time, ".jpeg"
        ))
      ) %>%
      { purrr::map2_chr(.$spectrogram_url, .$clip_file_name, download_file) }
    # Tag spectrogram and tag clip
  } else if (all(c("spectrogram_url", "clip_url") %in% colnames(input_data)) & any(type %in% c("tag_clip_spectrogram", "tag_clip_audio"))) {
    output_data <- input_data |>
      mutate(
        detection_time = gsub("\\.", "_", as.character(detection_time)),
        audio_file_type = sub('.*\\.(\\w+)$', '\\1', clip_url),
        clip_file_name_spec = file.path(output, paste0(
          organization, "_", location, "_", format(recording_date_time, "%Y%m%d_%H%M%S"), "__",
          species_code, "__", individual_order, "__", detection_time, ".jpeg"
        )),
        clip_file_name_audio = file.path(output, paste0(
          organization, "_", location, "_", format(recording_date_time, "%Y%m%d_%H%M%S"), "__",
          species_code, "__", individual_order, "__", detection_time, ".", audio_file_type
        ))
      ) %>%
      {
        purrr::map2_chr(.$spectrogram_url, .$clip_file_name_spec, download_file)
        purrr::map2_chr(.$clip_url, .$clip_file_name_audio, download_file)
      }
    # Images
  } else if ("media_url" %in% colnames(input_data)){
    output_data <- input_data |>
      mutate(image_name = file.path(output,
                                    paste0(location, "_", format(image_date_time, "%Y%m%d_%H%M%S"), ".jpeg"))) %>%
      {
        print(paste("Media URL:", .$media_url))
        print(paste("Image Name:", .$image_name))
        purrr::map2_chr(.$media_url, .$image_name, download_file)
      }
  } else {
    stop("Required columns are either 'recording_url', 'media_url', 'spectrogram_url', or 'clip_url'. Use wt_download_report(reports = 'recording', 'image_report' or 'tag') to get the correct media.")
  }

  return(output_data)
}


#' Download data from Data Discover
#'
#' @description Download Data Discover results from projects across WildTrax
#'
#' @param sensor  The sensor you wish to query from either 'ARU', 'CAM' or 'PC'
#' @param species The species you want to search for (e.g. 'White-throated Sparrow'). Multiple species can be included.
#' @param boundary The custom boundary you want to use. Must be a list of at least four latitude and longitude points where the last point is a duplicate of the first, or an object of class "bbox" (as produced by sf::st_bbox)
#'
#' @import dplyr tibble httr2
#' @export
#'
#' @examples
#' \dontrun{
#'
#' aoi <- list(
#' c(-110.85438, 57.13472),
#' c(-114.14364, 54.74858),
#' c(-110.69368, 52.34150),
#' c(-110.85438, 57.13472)
#' )
#'
#' dd <- wt_dd_summary(sensor = 'ARU', species = 'White-throated Sparrow', boundary = aoi)
#' }
#'
#' @return A list of two tibbles one from the map and the other from the data

wt_dd_summary <- function(sensor = c('ARU','CAM','PC'), species = NULL, boundary = NULL) {

  if (!exists("access_token", envir = ._wt_auth_env_)) {
    message("Currently searching as a public user, access to data will be limited. Use wt_auth() to login.")
    tok_used <- NULL
  } else {
    message("Currently searching as a logged in user.")
    tok_used <- ._wt_auth_env_$access_token
  }

  # Generate user agent
  u <- .gen_ua()

  if(is.null(tok_used)) {
    #Provide non-login user a way to search species - limited by dd-get-species however
    ddspp <- request("https://dev-api.wildtrax.ca") |>
      req_url_path_append("/bis/dd-get-species") |>
      req_headers(
        Authorization = tok_used,
        Origin = "https://dev.wildtrax.ca/discover",
        Pragma = "no-cache",
        Referer = "https://dev.wildtrax.ca/discover"
      ) |>
      req_user_agent(u) |>
      req_body_json(list(sensorId = sensor)) |>
      req_perform()

    spp_t <- resp_body_json(ddspp)

    species_tibble <- purrr::map_df(spp_t, ~{
      commonName <- if ("commonName" %in% names(.x)) .x$commonName else NA
      speciesId <- if ("speciesId" %in% names(.x)) .x$speciesId else NA
      sciName <- if ("sciName" %in% names(.x)) .x$sciName else NA
      tibble(species_common_name = commonName, species_id = speciesId, species_scientific_name = sciName)
    })

    if (is.null(species)) {
      spp <- species_tibble$species_id
      if (length(spp) == 0) {
        stop("No species data available for the selected sensor.")
      }
    } else {
      spp <- species_tibble |>
        filter(species_common_name %in% species) |>
        pull(species_id)
      if (length(spp) == 0) {
        stop("No species were found.")
      }
    }
  } else {
    # User is logged in - gets the whole species table
    species_tibble <- wt_get_species()
    spp <- species_tibble |>
      filter(species_common_name %in% species) |>
      pull(species_id)
  }

  # Test for coordinate system
  if (inherits(boundary, "bbox")) {
    lat_valid <- boundary["ymin"] >= -90 & boundary["ymax"] <= 90
    long_valid <- boundary["xmin"] >= -180 & boundary["xmax"] <= 180
    if (!lat_valid | !long_valid) {
      stop("Coordinate system for boundary or bbox must be in latitude and longitude")
    }
  } else if (inherits(boundary, "list")) {
    lat_valid <- all(sapply(boundary, function(coord) coord[2] >= -90 & coord[2] <= 90))
    long_valid <- all(sapply(boundary, function(coord) coord[1] >= -180 & coord[1] <= 180))
    if (!lat_valid | !long_valid) {
      stop("Coordinate system for boundary or bbox must be in latitude and longitude")
    }
  }

  # Test for bbox
  if (inherits(boundary,"bbox")){
    boundary <- list(
      c(boundary['xmin'], boundary['ymin']),
      c(boundary['xmax'], boundary['ymin']),
      c(boundary['xmax'], boundary['ymax']),
      c(boundary['xmin'], boundary['ymax']),
      c(boundary['xmin'], boundary['ymin']) # Closing the polygon
    )
  }

  # Validate boundary if provided
  if (!is.null(boundary)) {
    # Check the number of vertices
    if (length(boundary) < 4) {
      stop("Error: Boundary must have at least four vertices.")
    }

    # Check for closure
    if (!identical(boundary[[1]], boundary[[length(boundary)]])) {
      # If the first and last points are not identical, check if they are 'close enough'
      if (!all(abs(unlist(boundary[[1]]) - unlist(boundary[[length(boundary)]])) < 1e-6)) {
        stop("Error: Boundary must form a closed polygon.")
      }
    }

    if (length(unique(boundary[-c(1, length(boundary))])) != length(boundary[-c(1, length(boundary))])) {
      stop("Error: Boundary contains duplicate vertices (excluding the first and last).")
    }

    # Check for valid coordinates
    if (!all(sapply(boundary, function(coord) all(is.numeric(coord) & length(coord) == 2)))) {
      stop("Error: Each coordinate pair must consist of valid latitude and longitude values.")
    }
  }

  # OK so here's da earth. Dat is a sweet earth you might say.
  full_bounds <- list(
    `_sw` = list(
      lng = -180.0,
      lat = -90
    ),
    `_ne` = list(
      lng = 180,
      lat = 90
    )
  )

  # Initialize lists to store results
  all_rpps_tibble <- list()
  all_result_tables <- list()

  # Iterate over each species
  for (sp in spp) {

    # Construct payload for lat-long summary
    payload_ll <- list(
      polygonBoundary = boundary,
      sensorId = sensor,
      speciesIds = list(sp)
    )

    if(is.null(boundary)) {payload_ll$polygonBoundary <- NULL}

    rr <- request("https://dev-api.wildtrax.ca") |>
      req_url_path_append("/bis/get-data-discoverer-long-lat-summary") |>
      req_headers(
        Authorization = tok_used,
        Origin = "https://dev.wildtrax.ca/discover",
        Pragma = "no-cache",
        Referer = "https://dev.wildtrax.ca/discover"
      ) |>
      req_user_agent(u) |>
      req_body_json(payload_ll) |>
      req_perform()

    # Construct payload for map-projects
    payload_mp <- list(
      isSpeciesTab = FALSE,
      sensorId = sensor,
      speciesIds = list(sp),
      polygonBoundary = boundary,
      bounds = full_bounds,
      zoomLevel = 20
    )

    rr2 <- request("https://dev-api.wildtrax.ca") |>
      req_url_path_append("/bis/get-data-discoverer-map-and-projects") |>
      req_headers(
        Authorization = tok_used,
        Origin = "https://dev.wildtrax.ca/discover",
        Pragma = "no-cache",
        Referer = "https://dev.wildtrax.ca/discover"
      ) |>
      req_user_agent(u) |>
      req_body_json(payload_mp) |>
      req_perform()

    # Extract content from the second request
    mapproj <- resp_body_json(rr2)

    # Extract features from the response
    features <- mapproj$map$features

    # Initialize empty vectors to store data
    count <- c()
    longitude <- c()
    latitude <- c()

    # Iterate over features and extract data
    for (feature in features) {
      count <- c(count, feature$properties$count)
      longitude <- c(longitude, feature$geometry$coordinates[[1]])
      latitude <- c(latitude, feature$geometry$coordinates[[2]])
    }

    back_species <- species_tibble |>
      filter(species_id %in% sp) |>
      select(species_common_name)

    # Create tibble for result table
    result_table <- tibble(
      species_common_name = back_species$species_common_name,
      count = count,
      longitude = longitude,
      latitude = latitude
    )

    rpps <- resp_body_json(rr)

    if(is.null(rpps)) {stop('Request was NULL.')}

    orgs <- map_chr(rpps$organizations, ~pluck(.x, "organizationName", .default = ""))
    counts <- map_dbl(rpps$projects, pluck, "count")
    projectNames <- map_chr(rpps$projects, ~pluck(.x, "projectName", .default = ""))
    projectIds <- map_int(rpps$projects, ~pluck(.x, "projectId", .default = NA_integer_))

    # Create tibble for project summary
    rpps_tibble <- tibble(
      projectId = projectIds,
      project_name = projectNames,
      species_id = sp,
      count = counts
    ) |>
      inner_join(species_tibble, by = "species_id") |>
      select(projectId, project_name, count, species_common_name, species_scientific_name) |>
      distinct()

    all_rpps_tibble[[length(all_rpps_tibble) + 1]] <- rpps_tibble
    all_result_tables[[length(all_result_tables) + 1]] <- result_table
  }

  combined_rpps_tibble <- bind_rows(all_rpps_tibble)
  combined_result_table <- bind_rows(all_result_tables)

  if (nrow(combined_rpps_tibble) == 0 | nrow(combined_result_table) == 0) {
    stop("No results were found on any of the search layers. Broaden your search and try again.")
  }

  return(list(
    "lat-long-summary" = combined_rpps_tibble,
    "map-projects" = combined_result_table
  ))
}

#' Batch download location photos
#'
#' @description Download location photos from an Organization
#'
#' @param organization The Organization from which photos are being downloaded
#' @param output Directory where location photos would be stored on your machine
#'
#' @import httr2
#' @import dplyr
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Authenticate first:
#' wt_auth()
#' wt_location_photos(organization = 'ABMI', output = NULL)
#' }
#'
#' @return A folder of photos or an object containing the download information for said photos
#'

wt_location_photos <- function(organization, output = NULL) {

  if (.wt_auth_expired())
    stop("Please authenticate with wt_auth().", call. = FALSE)

  org_numeric <- .get_org_id(organization)

  r <- .wt_api_gr(
      path = "/bis/get-location-image-summary",
      organizationId = org_numeric,
      sort = "locationName",
      order = "asc",
      limit = 1e9
    )

  photos <- resp_body_json(r)

  return(photos)

  }

#' Get column headers and data from WildTrax syncs and downloads
#'
#' @description Fetch column headers and data for syncs and downloads in WildTrax. You must specify at least one of `project` or `organization` depending on the API and at what level of the system you're looking for data for. Use `wt_download_report()`.
#'
#' @param api A string specifying the API to query. Must be one of:
#' \itemize{
#'   \item `"organization_locations"`
#'   \item `"organization_visits"`
#'   \item `"organization_equipment`"
#'   \item `"organization_deployments`"
#'   \item `"organization_recording_summary`"
#'   \item `"organization_image_summary`"
#'   \item `"project_locations"`
#'   \item `"project_species"`
#'   \item `"project_aru_tasks"`
#'   \item `"project_aru_tags"`
#'   \item `"project_camera_tasks"`
#'   \item `"project_camera_tags"`
#'   \item `"project_point_counts"`
#' }
#' @param option Choose between just returning column headers or the entire data frame itself
#' @param project The project id
#' @param organization The organization id
#'
#' @import httr2 tibble dplyr
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Authenticate first:
#' wt_auth()
#'
#' # Fetch column headers by organization
#' wt_get_sync("organization_locations")
#'
#' # Fetch column headers by project
#' wt_get_sync("project_locations")
#' }
#'
#' @return A tibble with column headers for the specified API call.

wt_get_sync <- function(api, option = c("columns", "data"), project = NULL, organization = NULL) {

  option <- match.arg(option)

  # Check authentication
  if (.wt_auth_expired()) {
    stop("Please authenticate with wt_auth().", call. = FALSE)
  }

  if (is.null(api) || api == "") {
    stop("The 'api' field is required.", call. = FALSE)
  }

  api_pseudonyms <- list(
    organization_locations = "get-location-summary", #JSON - POST
    organization_visits = "get-location-visits", #500 - POST
    organization_equipment = "get-equipment-summary", #TEXT - POST
    organization_deployments = "get-location-visit-equipment-summary", #JSON - POST
    organization_task_creator = "recording-task-creator-results", #JSON - POST
    organization_image_summary = "get-camera-pud-summary", #JSON - POST
    project_locations = "download-location", #CSV - GET
    project_aru_tasks = "download-tasks-by-project-id", #400 - GET
    project_aru_tags = "download-tags-by-project-id", #400 - GET
    project_camera_tasks = "download-camera-tasks-by-project-id", #404 - GET
    project_camera_tags = "download-camera-tags-by-project-id", #CSV - GET
    project_point_counts = "download-point-count-by-project-id", #CSV - MISSING
    project_species = "get-project-species-details" #400 - GET
  )

  api <- api_pseudonyms[[api]] %||% api

  api_defaults <- list(
    "get-location-summary" = list(organizationId = organization),
    "get-location-visits" = list(organizationId = organization),
    "get-equipment-summary" = list(organizationId = organization),
    "get-location-visit-equipment-summary" = list(organizationId = organization),
    "recording-task-creator-results" = list(organizationId = organization),
    "get-camera-pud-summary" = list(organizationId = organization),
    "download-location" = list(projectId = project),
    "download-tasks-by-project-id" = list(projectId = project),
    "download-tags-by-project-id" = list(projectId = project),
    "download-camera-tasks-by-project-id" = list(projectId = project),
    "download-camera-tags-by-project-id" = list(projectId = project),
    "download-point-count-by-project-id" = list(projectId = project),
    "get-project-species-details" = list(projectId = project)
  )

  if (!api %in% names(api_defaults)) {
    stop("API not recognized or defaults not defined for the provided API.", call. = FALSE)
  }

  api_params <- api_defaults[[api]]
  #api_params$limit <- 1e2
  api_path <- paste0("/bis/", api)

  print(api_params)

  print(paste('Calling...', api_path))

  response <- do.call(.wt_api_pr, c(list(path = api_path), api_params))
  content_type <- resp_content_type(response)
  message("Content-Type returned: ", content_type)

  if(content_type == "application/json") {

    json_data <- resp_body_json(response)
    results <- json_data$results
    if (length(results) == 0) stop("No location data returned")

    data <- as_tibble(do.call(rbind, results)) |>
      unnest(everything())

    print(data)

  } else if (content_type == "application/csv") {

    tmp_file <- tempfile(fileext = ".csv")
    writeBin(resp_body_raw(response), tmp_file)
    data <- read_csv(tmp_file, show_col_types = FALSE)

    print(data)

  } else if (content_type == "application/zip") {

    message("Do a zip thing")

  } else if (content_type == "text/plain") {

    message("Do a text thing")

  } else {

    message('Content type not recognized')

  }

  }
