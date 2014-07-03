\name{NHANES}
\alias{NHANES}
\alias{smallNHANES}
\docType{data}
\title{
Body shape, mortality, and related measurements from NHANES 1999-2004.
}
\description{
These data were collected as part of the NHANES 1999-2004 study.
}
\usage{data(NHANES)}
\format{
  \code{NHANES} is a data frame with 31126 observations on the following 19 variables. 
  
  \code{smallNHANES} is a random sample of 500 of the observations, intended to let people prototype graphical displays, etc.
  \describe{
    \item{\code{sex}}{categorical}
    \item{\code{age}}{in years}
    \item{\code{pregnant}}{categorical}
    \item{\code{died}}{Whether the person is alive at the time of follow up.  Deaths are divided into cardiovascular and non-cardiovascular.}
    \item{\code{followup}}{months of follow up (for mortality data)}
    \item{\code{smoker}}{categorical}
    \item{\code{diabetic}}{categorical}
    \item{\code{height}}{height (meters)}
    \item{\code{weight}}{weight (kilograms)}
    \item{\code{BMI}}{body mass index}
    \item{\code{trunkfat}}{percent trunk fat}
    \item{\code{percentfat}}{percent fat}
    \item{\code{leanBMI}}{lean-tissue only BMI}
    \item{\code{framingham}}{Framingham risk score}
    \item{\code{HDL}}{HDL cholesterol}
    \item{\code{LDL}}{cholesterol}
    \item{\code{systolic}}{systolic blood pressure, mmHg}
    \item{\code{diastolic}}{diastolic blood pressure, mmHg}
    \item{\code{income}}{ratio of family income to poverty threshold. 5 is ratio greater than or equal to 5}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
These data were assempled from NHANES 1999-2004 sources by Dr. Nir Krakauer at City College of New York.
}
\references{Nir Y. Krakauer, Jesse C. Krakauer, "A New Body Shape Index Predicts Mortality Hazard Independently of Body Mass Index" PloS one, 7 (7) PMID: 22815707, 2012.
\url{http://www.plosone.org/article/info:doi/10.1371/journal.pone.0039504}
}
\examples{
data(NHANES)
str(NHANES)
nrow(NHANES)
data(smallNHANES)
nrow(smallNHANES)
}
\keyword{datasets}
