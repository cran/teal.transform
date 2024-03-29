% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/call_utils.R
\name{call_condition_choice}
\alias{call_condition_choice}
\title{Choices condition call}
\usage{
call_condition_choice(varname, choices)
}
\arguments{
\item{varname}{(\code{name} or \code{call} or \code{character(1)})
name of the variable.}

\item{choices}{(\code{vector})
\code{varname} values to match using the \code{==} (single value) or \code{\%in\%} (vector)
condition.}
}
\value{
\code{call}.
}
\description{
Compose choices condition call from inputs.
}
\details{
\code{choices} can be vector of any type but for some output might be converted:
\itemize{
\item \code{factor} call is composed on choices converted to \code{character};
\item \code{Date} call is composed on choices converted to \code{character} using
\code{format(choices)};
\item \code{POSIXct}, \code{POSIXlt} call is composed on choices converted to \code{character} using
\code{format(choices)}.
}

One has to be careful here as formatted date-time variable might loose
some precision (see \code{format} argument in \code{\link[=format.POSIXlt]{format.POSIXlt()}} and output call
could be insufficient for exact comparison. In this case one should specify
\verb{varname = trunc(<varname>)} and possibly convert \code{choices} to \code{character}).
}
\keyword{internal}
