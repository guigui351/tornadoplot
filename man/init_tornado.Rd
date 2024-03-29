% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/init_tornado.R
\name{init_tornado}
\alias{init_tornado}
\title{Initialize Settings for Adverse Event Tornado plot widget}
\usage{
init_tornado(
  data,
  settings,
  ref_arm = "Treatment",
  comp_arm = "Placebo",
  threshold = 1
)
}
\arguments{
\item{data}{named list of data}

\item{settings}{named list of settings}
}
\value{
returns list with data and settings used for fct_tornadoplot
}
\description{
Initialize Settings for Adverse Event Tornado plot widget
}
\examples{

init_tornado(params$data, settings=params$settings, ref_arm= "Treatment", comp_arm = "Placebo", threshold = 1)
}
