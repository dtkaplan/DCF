\name{nci60}
\alias{nci60}
\alias{nci60expression}
\alias{nci60cellLine}
\docType{data}
\title{Data from the NCI60 gene expression set.}
\description{The data come from a National Cancer Institute study of gene expression in cell lines drawn from various sorts of cancer.

There are two original files: 

The expression data, \code{nci60expression} is a dataframe of 41078 gene probes (rows) and 60 cell lines (columns).  The first column, \code{Probe} gives the name of the Agilent microarray proble

\code{nci60cellLine} contains a row for each of the cell line columns
in \code{nci60expression}
}
\usage{data("nci60cellLine")}
\format{
  See the description
}
\details{}
\source{
DRAFT: Get the web address for the NCI60 data
}
\references{
DRAFT: Some papers describing the data?
}
\examples{
data(nci60cellLine) 
}
\keyword{datasets}
