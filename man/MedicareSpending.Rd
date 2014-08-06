\name{MedicareSpending}
\alias{MedicareSpending}
\docType{data}
\title{Charges and Payments by Medicare}
\description{
These data, released in May 2013, describe how much hospitals charged Medicare for various patient procedures, how many were performed, and how much Medicare actually paid.}
\usage{data("MedicareSpending")}
\format{
  A data frame with 163065 observations on the following 11 variables.
  \describe{
    \item{\code{DRG.Definition}}{Direct Recovery Group description and code.  The DRG gives the procedure type.}
    \item{\code{Provider.Id}}{a numeric vector}
    \item{\code{Provider.Name}}{The hospital name.}
    \item{\code{Provider.Street.Address}}{}
    \item{\code{Provider.City}}{}
    \item{\code{Provider.State}}{Two letter abbreviation}
    \item{\code{Provider.Zip.Code}}{a numeric vector}
    \item{\code{Hospital.Referral.Region.Description}}{Gives both "city" and state.}
    \item{\code{Total.Discharges}}{a numeric vector}
    \item{\code{Average.Covered.Charges}}{a numeric vector}
    \item{\code{Average.Total.Payments}}{a numeric vector}
    }
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(MedicareSpending)
## maybe str(MedicareSpending) ; plot(MedicareSpending) ...
}
\keyword{datasets}
