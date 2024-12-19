# wildrtrax <img src="man/figures/logo.png" width="50%" align="right"/>

<!-- badges: start -->

[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable) [![CRAN status](https://www.r-pkg.org/badges/version/wildrtrax)](https://CRAN.R-project.org/package=wildrtrax) [![Codecov test coverage](https://codecov.io/gh/ABbiodiversity/wildRtrax/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ABbiodiversity/wildRtrax?branch=main) [![R-CMD-check](https://github.com/ABbiodiversity/wildRtrax/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ABbiodiversity/wildRtrax/actions/workflows/R-CMD-check.yaml)

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

### Acoustic work flow

Download data and run and a single-season single-species occupancy analysis. Consult [APIs](https://abbiodiversity.github.io/wildrtrax/articles/apis.html) and [Acoustic data wrangling](https://abbiodiversity.github.io/wildrtrax/articles/acoustic-data-wrangling.html) for more information.

```         
library(wildrtrax)

# OAuth tokens only. Google OAuth2 will be supported soon
Sys.setenv(WT_USERNAME = "*****", WT_PASSWORD = "*****")
       
wt_auth()

projects <- wt_get_download_summary("ARU") |>
  filter(project == "ABMI Ecosystem Health 2023") |>
  select(project_id) |>
  pull()

data <- map_dfr(.x = projects, .f = ~wt_download_report(.x, "ARU", weather_cols = F, reports = "main")

dat.occu <- wt_format_occupancy(my_report, species="OVEN", siteCovs=NULL)
mod <- unmarked::occu(~ 1 ~ 1, dat.occu)
mod
```

Conduct some pre-processing

### Camera work flow

```         
library(wildrtrax)

Sys.setenv(WT_USERNAME = "*****", WT_PASSWORD = "*****")

wt_auth()


projects <- wt_get_download_summary("CAM") |>
  filter(project == "ABMI Ecosystem Health 2014") |>
  select(project_id) |>
  pull()

raw_data <- map_dfr(.x = projects, .f = ~wt_download_report(.x, "CAM", weather_cols = F, reports = "main")

summarised <- wt_ind_detect(raw_data, 30, "minutes") |>
              wt_summarise_cam(raw_data, "day", "detections", "long")
```

### Ultrasonic work flow

Coming soon!

### Point count work flow

Coming soon!

## Issues

To report bugs, request additional features, or get help using the package, please file an [issue](https://github.com/ABbiodiversity/wildrtrax/issues).

## Contributors

We encourage ongoing contributions and collaborations to improve the package into the future. The [Alberta Biodiversity Monitoring Institute](https://abmi.ca) provides ongoing support, development and funding.

## License

This R package is licensed under [MIT license](https://github.com/ABbiodiversity/wildrtrax/blob/master/LICENSE)©2024 [Alberta Biodiversity Monitoring Institute](https://abmi.ca).

## Code of Conduct

Please note that `wildrtrax` is released with a [Contributor Code of Conduct](https://github.com/ABbiodiversity/wildRtrax?tab=coc-ov-file). By contributing to this project, you agree to abide by its terms.
