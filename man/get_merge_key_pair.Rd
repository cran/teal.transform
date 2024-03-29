% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_merge_call.R
\name{get_merge_key_pair}
\alias{get_merge_key_pair}
\title{Gets keys vector from keys list}
\usage{
get_merge_key_pair(selector_from, selector_to, key_from)
}
\arguments{
\item{selector_from}{(\code{list}) of \code{data_extract_srv} objects.}

\item{selector_to}{(\code{list}) of \code{data_extract_srv} objects.}

\item{key_from}{(\code{character}) keys used in the first selector while joining.}
}
\value{
\code{character} vector of selector keys.
}
\description{
Gets keys vector from keys list
}
\details{
This function covers up to now 4 cases:
\itemize{
\item Dataset without parent: Primary keys are returned;
\item Dataset source = dataset target:
The primary keys subtracted of all key columns that get purely filtered.
This means just one value would be left after filtering inside this column
Then it can be taken out;
\item Target \code{dataname} is parent foreign keys;
\item Any other case foreign keys;
}
}
\keyword{internal}
