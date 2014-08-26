\name{CountryData}
\alias{CountryData}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("CountryData")}
\format{
  A data frame on the following 76 variables for each of the Countries in the World. (256 countries are given.)
  \describe{
    \item{\code{country}}{Name of the country.}
    \item{\code{area}}{area (sq km), 2147}
    \item{\code{pop}}{number of people, 2119}
    \item{\code{growth}}{growth rate per year (\%),2002}
    \item{\code{birth}}{birth rate (#/1000), 2054}
    \item{\code{death}}{death rate (#/1000), 2066}
    \item{\code{netMigr}}{net migration per year (#/1000). Difference between incoming and outgoing. 2112}
    \item{\code{maternal}}{maternal deaths per 100000 live births. 2223}
    \item{\code{infant}}{infant deaths per 1000 live births. 2091}
    \item{\code{life}}{life expectancy (years), 2102}
    \item{\code{fert}}{children born/woman (#/person), 2127}
    \item{\code{health}}{health spending (\% of GDP), 2225}
    \item{\code{HIVrate}}{people with HIV (\% of population), 2155}
    \item{\code{HIVpeople}}{people with HIV (#), 2156}
    \item{\code{HIVdeath}}{deaths from HIV (#/year), 2157}
    \item{\code{obesity}}{fraction of population considered obese (\%), 2228}
    \item{\code{underweight}}{fraction of population considered underweight (\%), 2224}
    \item{\code{educ}}{education spending (\% of GDP), 2206}
    \item{\code{unemploymentYouth}}{youth unemployment (\%), 2229}
    \item{\code{GDP}}{Gross Domestic Product ($), 2001}
    \item{\code{GDPgrowth}}{growth in GDP (\%/year), 2003}
    \item{\code{GDPcapita}}{GDP per capita ($/person), 2004}
    \item{\code{saving}}{yearly money savings (\% of GDP), 2260}
    \item{\code{indProd}}{a numeric vector}
    \item{\code{labor}}{a numeric vector}
    \item{\code{unemployment}}{a numeric vector}
    \item{\code{family}}{a numeric vector}
    \item{\code{tax}}{a numeric vector}
    \item{\code{budget}}{a numeric vector}
    \item{\code{debt}}{a numeric vector}
    \item{\code{inflation}}{a numeric vector}
    \item{\code{discount}}{a numeric vector}
    \item{\code{lending}}{a numeric vector}
    \item{\code{narrow}}{a numeric vector}
    \item{\code{broad}}{a numeric vector}
    \item{\code{credit}}{a numeric vector}
    \item{\code{shares}}{a numeric vector}
    \item{\code{balance}}{a numeric vector}
    \item{\code{exports}}{a numeric vector}
    \item{\code{imports}}{a numeric vector}
    \item{\code{gold}}{a numeric vector}
    \item{\code{externalDebt}}{a numeric vector}
    \item{\samp{ homeStock}}{a numeric vector}
    \item{\samp{ abroadStock}}{a numeric vector}
    \item{\code{elecProd}}{a numeric vector}
    \item{\code{elecCons}}{a numeric vector}
    \item{\code{elecExp}}{a numeric vector}
    \item{\code{elecImp}}{a numeric vector}
    \item{\code{elecCap}}{a numeric vector}
    \item{\code{elecFossil}}{a numeric vector}
    \item{\code{elecNuc}}{a numeric vector}
    \item{\code{elecHydro}}{a numeric vector}
    \item{\code{elecRenew}}{a numeric vector}
    \item{\code{oilProd}}{a numeric vector}
    \item{\code{oilExp}}{a numeric vector}
    \item{\code{oilImp}}{a numeric vector}
    \item{\code{oilRes}}{a numeric vector}
    \item{\code{petroProd}}{a numeric vector}
    \item{\code{petroCons}}{a numeric vector}
    \item{\code{petroExp}}{a numeric vector}
    \item{\code{petroImp}}{a numeric vector}
    \item{\code{gasProd}}{a numeric vector}
    \item{\code{gasCons}}{a numeric vector}
    \item{\code{gasExp}}{a numeric vector}
    \item{\code{gasImp}}{a numeric vector}
    \item{\code{gasRes}}{a numeric vector}
    \item{\code{mainlines}}{a numeric vector}
    \item{\code{cell}}{a numeric vector}
    \item{\code{netHosts}}{a numeric vector}
    \item{\code{netUsers}}{a numeric vector}
    \item{\code{airports}}{a numeric vector}
    \item{\code{railways}}{a numeric vector}
    \item{\code{roadways}}{a numeric vector}
    \item{\code{waterways}}{a numeric vector}
    \item{\code{marine}}{a numeric vector}
    \item{\code{military}}{a numeric vector}
  }
}
\details{
These data were collected using the \code{CIAdata()} function.

THE DOCUMENTATION NEEDS COMPLETING.  See the output of \code{CIAdata() and Google "CIA Factbook NNNN"}, where NNNN is the 4-digit code used for indexing the data at the CIA.
}
\source{
From the CIA World Factbook, \url{https://www.cia.gov/library/publications/the-world-factbook/}  The lines to construct the file are in inst/datafixes.R
}
\references{
}
\examples{
data(CountryData)
## maybe str(CountryData) ; plot(CountryData) ...
}
\keyword{datasets}
