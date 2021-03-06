% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tbl_scidb.R
\name{slice_.tbl_scidb}
\alias{slice_.tbl_scidb}
\title{Select SciDB subarrays by coordinates}
\usage{
\method{slice_}{tbl_scidb}(.data, ..., .dots = list())
}
\description{
This method is mapped to the SciDB \code{between} operator. Each argument
specifies a range of SciDB array coordinates in the order of the listed
SciDB array dimensions. Use \code{NA} or missing ranges to include everything.
}
\examples{
\dontrun{
library(scidb.dplyr)
db <- scidbconnect()
x <- tbl(as.scidb(db, matrix(1:20, nrow=5))) # (a 5 x 4 matrix)
# Slice rows zero to three in the first dimension, all of the 2nd dimension:
slice(x, 0:3)

# Slice rows zero to three in the 1st array dimension, 1 to 2 in the 2nd:
slice(x, 0:3, 1:2)
}
}

