% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataset.R
\name{read.dataset}
\alias{read.dataset}
\title{Read a dataset from file}
\usage{
read.dataset(dataset, i = NULL, cache = TRUE, suffix = ".dat")
}
\arguments{
\item{dataset}{The name of the dataset. This will be stored in
files called \code{<dataset>-0000i<suffix>} where \code{i} is the
index of the dataset}

\item{i}{The index of the dataset}

\item{cache}{If \code{TRUE}, cache data}

\item{suffix}{The suffix of the data files}
}
\value{
A data frame containing the dataset
}
\description{
The first time this data is read it is loaded from file, and then cached.
On subsequent calls, it is retreived from the cache
}
\author{
David Sterratt
}

