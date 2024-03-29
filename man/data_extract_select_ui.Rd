% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_extract_select_module.R
\name{data_extract_select_ui}
\alias{data_extract_select_ui}
\title{Returns a \code{shiny.tag.list} object with the UI for a \code{select_spec} object}
\usage{
data_extract_select_ui(select, id = "select")
}
\arguments{
\item{select}{(\code{select_spec}) A definition of a select spec element.
Setting \code{\link[=select_spec]{select_spec()}} with \code{ordered = TRUE} makes this selector responsive
to the variable selection order.}

\item{id}{(\code{character(1)}) The shiny \code{inputId} of the element.}
}
\value{
\code{shiny.tag.list} with the UI.
}
\description{
Returns a \code{shiny.tag.list} object with the UI for a \code{select_spec} object
}
\keyword{internal}
