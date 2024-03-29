% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_extract_filter_module.R
\name{data_extract_filter_srv}
\alias{data_extract_filter_srv}
\title{Handles events emitted from the UI generated by \code{data_extract_filter_ui}}
\usage{
data_extract_filter_srv(id, datasets, filter)
}
\arguments{
\item{id}{(\code{character}) id string.}

\item{datasets}{(\verb{named list}) a list of reactive \code{data.frame} type objects.}

\item{filter}{(\code{filter_spec}) the filter generated by a call to \code{\link[=filter_spec]{filter_spec()}}.}
}
\value{
\code{NULL}, invisibly.
}
\description{
Handles events emitted from the UI generated by \code{data_extract_filter_ui}
}
\note{
This shiny module server updates the values of the \code{vals}
\code{\link[teal.widgets:optionalSelectInput]{teal.widgets::optionalSelectInput()}} widget.
It's responsible for setting the initial values and the subsequent updates to
the \code{vals} widget based on the input of the \code{col} widget.
}
\keyword{internal}
