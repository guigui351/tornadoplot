
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tornado - Plot

<!-- badges: start -->
<!-- badges: end -->

Tornado Plots are a special type of Bar chart, where the data categories
are listed vertically instead of the standard horizontal presentation,
and the categories are ordered so that the largest bar appears at the
top of the chart, the second largest appears second from the top, and so
on. They are so named because the final chart visually resembles either
one half of or a complete tornado.

![tornadoplot App screenshot.](www/tornado.png)

## Installation

You can install the development version of tornadoplot like so:

``` r
devtools::install_github('guigui351/tornadoplot', ref="main")
```

and run it as follow:

``` r
library(torndadoplot)
library(tidyverse)
library(safetyData)

# Remove screen failures data
sdtm_dm <- safetyData:: sdtm_dm %>% filter (ARMCD != "Scrnfail") 

# settings for tornado plot
setting <-list(
  aes=list(id_col="USUBJID", bodsys_col="AEBODSYS", term_col="AEDECOD", severity_col="AESEV", serious_col="AESER"),
  dm=list(id_col="USUBJID", treatment_col="ARM")
)

# params to be loaded for the tornado plot / mandatory except if it runs with SafetyGraphics
params <- list(data = list(dm = sdtm_dm, aes = safetyData::sdtm_ae), settings = setting)

# standalone tornadoplot / params parameter automatically called by the run_app function
tornadoplot::run_app()
```

Or run in safetyGraphics:

``` r
library(torndadoplot)
library(safetyGraphics)
library(tidyverse)

# Load standard graphics from safetyCharts + Tornado plot
charts<-c(
    safetyGraphics::makeChartConfig(),
    safetyGraphics::makeChartConfig(packages="torndadoplot")
)

# List of stm data to be used in the app
sdtm <- list(dm = sdtm_dm, aes = safetyData::sdtm_ae)

# Make metadata
meta_charts <- makeMeta(charts)

# Initialize SafetyGraphics app.
safetyGraphics::safetyGraphicsApp(domainData = sdtm, charts=charts, meta = meta_charts)
```
