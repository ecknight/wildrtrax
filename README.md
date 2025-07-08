# wildrtrax <img src="man/figures/logo.png" width="50%" align="right"/>

<!-- badges: start -->

[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable) [![CRAN status](https://www.r-pkg.org/badges/version/wildrtrax)](https://CRAN.R-project.org/package=wildrtrax) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.13952335.svg)](https://doi.org/10.5281/zenodo.13952335) [![Codecov test coverage](https://codecov.io/gh/ABbiodiversity/wildrtrax/graph/badge.svg)](https://app.codecov.io/gh/ABbiodiversity/wildrtrax)

<!-- badges: end -->

## Overview

`wildrtrax` (pronounced *wild-r-tracks*) is an R package containing functions to help manage and analyze environmental sensor data. It helps to simplify the entire data life cycle by offering tools for data pre-processing, wrangling, and analysis, facilitating seamless data transfer to and from [WildTrax](https://wildtrax.ca/). With `wildrtrax`, users can effortlessly establish end-to-end workflows and ensure reproducibility in their analyses. By providing a consistent and organized framework, the package promotes transparency and integrity in research, making it effortless to share and replicate results.

## Installation

You can install the most recent version of `wildrtrax` directly from this repository with:

``` r
# install.packages("remotes")
remotes::install_github("ABbiodiversity/wildrtrax")
```

The [development](https://github.com/ABbiodiversity/wildrtrax/tree/development) version of this package contains experimental features and recent fixes. It can be installed with:

``` r
remotes::install_github("ABbiodiversity/wildrtrax@development")
```

The development version of the package will be periodically merged and will be reflected in the [Changelogs](https://abbiodiversity.github.io/wildrtrax/news/index.html).

## Usage

All functions begin with a `wt_*` prefix. Column names and metadata align with the WildTrax infrastructure. The goal is to follow the work flow of pre-processing, linking with WildTrax, download and analysis.

### Discover Data

Explore data through [Data Discover](https://discover.wildtrax.ca/). No login required to access public data! Use `wt_auth()` to login to see data you have privileges and membership to.

``` r
library(wildrtrax)
library(sf)

# Search for public data without a boundary
wt_dd_summary(sensor = 'ARU', species = 'White-throated Sparrow')

# Apply an area of interest. Define a polygon or use a bbox from sf::st_bbox
my_aoi <- list(
  c(-113.96068, 56.23817),
  c(-117.06285, 54.87577),
  c(-112.88035, 54.90431),
  c(-113.96068, 56.23817)
)

wt_dd_summary(sensor = 'ARU', species = 'White-throated Sparrow', boundary = my_aoi)

# Alberta bounding box
abbox <- read_sf("...shp") |> # Shapefile of Alberta
  filter(Province == "Alberta") |>
  st_transform(crs = 4326) |> 
  st_bbox()

wt_dd_summary(sensor = "ARU", species = "White-throated Sparrow", boundary = abbox)
```

### Acoustic work flows

Download data and run and a single-season single-species occupancy analysis. Consult [APIs](https://abbiodiversity.github.io/wildrtrax/articles/apis.html) and Acoustic data wrangling for more information.

``` r
library(wildrtrax)
library(tidyverse)

# OAuth logins only. Google OAuth2 will be supported soon.
Sys.setenv(WT_USERNAME = "*****", WT_PASSWORD = "*****")

# Authenticate to WildTrax
wt_auth()

# Get a project id
projects <- wt_get_download_summary("ARU") |>
  filter(project == "Ecosystem Health 2023") |>
  select(project_id) |>
  pull()

# Download the main report
raw_data <- map_dfr(.x = projects, .f = ~wt_download_report(.x, "ARU", weather_cols = F, reports = "main"))

# Format to occupancy for OVEN
dat.occu <- wt_format_occupancy(raw_data, species="OVEN", siteCovs=NULL)

# Run the model
unmarked::occu(~ 1 ~ 1, dat.occu)
```

Conduct some pre-processing on various types of acoustic data. See more in [Acoustic pre-processing](https://abbiodiversity.github.io/wildrtrax/articles/acoustic-pre-processing.html).

``` r
library(wildrtrax)
library(tidyverse)

# Scan files and filter results
my_files <- wt_audio_scanner(path = ".", file_type = "wav", extra_cols = T) |>
              mutate(hour = as.numeric(format(recording_date_time, "%H"))) |>
              filter(julian == 176, hour %in% c(4:8))
              
# Run acoustic indices and LDFCs
wt_run_ap(x = my_files, output_dir = paste0(root, 'ap_outputs'), path_to_ap = '/where/you/store/AP')

wt_glean_ap(my_files, input_dir = ".../ap_outputs", purpose = "biotic")
```

Evaluate the performance of BirdNET on a project, and search for false negatives missed by human taggers. See [Classifiers Tutorial](https://abbiodiversity.github.io/wildrtrax/articles/classifiers-tutorial.html) for more information.

``` r
library(wildrtrax)
library(tidyverse)

# OAuth logins only. Google OAuth2 will be supported soon.
Sys.setenv(WT_USERNAME = "*****", WT_PASSWORD = "*****")

# Authenticate to WildTrax
wt_auth()

data <- wt_download_report(project_id = 1144, sensor_id = "ARU", reports = c("main", "birdnet"),  weather_cols = FALSE)
                           
eval <- wt_evaluate_classifier(data, resolution = "task", remove_species = TRUE, thresholds = c(10, 99))

threshold_use <- wt_classifier_threshold(eval)

# Find additional species
wt_additional_species(data, remove_species = TRUE, threshold = threshold_use, resolution="task")
```

### Camera work flows

The ultimate pipeline for your camera data work flows. See [Camera data wrangling](https://abbiodiversity.github.io/wildrtrax/articles/camera-data-wrangling.html) for more information.

``` r
library(wildrtrax)
library(tidyverse)

# OAuth logins only. Google OAuth2 will be supported soon.
Sys.setenv(WT_USERNAME = "*****", WT_PASSWORD = "*****")

# Authenticate to WildTrax
wt_auth()

# Get a project id
projects <- wt_get_download_summary("CAM") |>
  filter(project == "ABMI Ecosystem Health 2014") |>
  select(project_id) |>
  pull()

# Download data
raw <- map_dfr(.x = projects, .f = ~wt_download_report(.x, "CAM", weather_cols = F, reports = "main"))

# Get individual detections
individual_detections <- wt_ind_detect(raw, 30, "minutes")
```

### Ultrasonic work flows

Format tags from [Kaleidoscope](https://www.wildlifeacoustics.com/products/kaleidoscope-pro?token=Sz_0cuFdrlAp3tVX2sJzcZanTHahEguB) for a WildTrax project. Download data from a project into an [NABAT]() acceptable format.

``` r
library(wildrtrax)
library(tidyverse)

input <- ".../bat.txt" # Kaleidoscope output
output <- ".../bats" # A folder to store the tags

wt_kaleidoscope_tags(input, output, tz, freq_bump = T) # Add a frequency buffer to the tag, e.g. 20000 kHz

## Upload the tags to a WildTrax project, then authenticate to WildTrax
wt_auth()

# Get a project id
projects <- wt_get_download_summary("ARU") |>
  filter(project == "BATS & LATS") |>
  select(project_id) |>
  pull()

# Download the data
raw_data <- map_dfr(.x = projects, .f = ~wt_download_report(.x, "ARU", weather_cols = F, reports = "main"))

# Experimental
raw_data |>
    wt_format_data(format = 'NABAT')
```

### Point count work flows

Download combined and formatted acoustic and point count data sets together.

``` r
library(wildrtrax)
library(tidyverse)

# An ARU project
an_aru_project <- wt_download_report(project_id = 620, sensor_id = 'ARU', reports = "main", weather_cols = F)

# An ARU project as point count format
aru_as_pc <- wt_download_report(project_id = 620, sensor_id = 'PC', reports = "main", weather_cols = F)
```

## Issues

To report bugs, request additional features, or get help using the package, please file an [issue](https://github.com/ABbiodiversity/wildrtrax/issues).

## Contributors

We encourage ongoing contributions and collaborations to improve the package into the future. The [Alberta Biodiversity Monitoring Institute](https://abmi.ca) provides ongoing support, development and funding.

## License

This R package is licensed under [MIT license](https://github.com/ABbiodiversity/wildrtrax/blob/master/LICENSE)©2024 [Alberta Biodiversity Monitoring Institute](https://abmi.ca).

## Code of Conduct

Please note that `wildrtrax` is released with a [Contributor Code of Conduct](https://github.com/ABbiodiversity/wildRtrax?tab=coc-ov-file). By contributing to this project, you agree to abide by its terms.
