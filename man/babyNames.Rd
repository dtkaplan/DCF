\name{babyNames}
\alias{babyNames}
\alias{babyNamesYearly}
\docType{data}
\title{
Names of children as recorded by the US Social Security Administration, and related tabulations.
}
\description{
The US Social Security Administration provides yearly lists of names given to babies.  .  These data combine the yearly lists.

\code{babyNames} is the raw data from the SSA. The case is a year-name-sex, for example: Jane F 1922.  The \code{count} is the number of children of that sex given that name in that year.  Names assigned to fewer than five children of one sex in any year are not listed, presumably out of privacy concerns.
}
\usage{data(babyNames); data(babyNamesYearly)}
\format{
  \code{babyNames} consists of 1,792,091 entries, each of which has four variables:
  \describe{
    \item{\code{name}}{The name (as a factor)}
    \item{\code{sex}}{F or M}
    \item{\code{count}}{The number of babies given that name and of that sex.}
    \item{\code{year}}{Year of birth}
  }
  \code{babyNamesYearly} is a tabulation of \code{babyNames} that gives the total number of births of each sex and the number of unique names.  It is provided for convenience.
}
\details{
\code{babyNames} was tabulated with \code{dplyr} to produce \code{babyNamesYearly}.  \code{group_by(babyNames,year,sex) \%>\% summarize(nbirths=sum(count),nnames=length(unique(name)))}
}
\source{
The data were compiled from the Social Security Administration web site: \code{http://www.ssa.gov/oact/babynames/names.zip}.
}
\references{}
\examples{
data(babyNames)
str(babyNames)
}
\keyword{datasets}
