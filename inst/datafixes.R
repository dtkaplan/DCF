names(MedicareSpending) <- c("drgDefinition", "idProvider", 
                             "nameProvider", "addressProvider", 
                             "cityProvider", "stateProvider", 
                             "zipProvider", "referralRegion", 
                             "totalDischarges", "aveCharges", 
                             "avePayments", "drg")

MedicareProviders <- unique( MedicareSpending[,c(2,3,4,5,6,7,8)])
MedicareCharges <- MedicareSpending[,c(12,2,9,10,11)]

Codes <- CIAdata()$Code
CountryData <- CIAdata(Codes[1])
for (code in Codes[-1]) {
  thisVar <- CIAdata(code)
  CountryData <- merge(CountryData, thisVar, all=TRUE)
}

## NCI 60

newNames <- c("cellLine",        "tissue",          "age",             "sex",             "prior.treatment",
"epithelial" ,     "histology",       "source"  ,        "ploidy",          "p53",            
"mdr",             "doublingtime")
names(nci60cellLine) <- newNames

## Put the names to have a lower case first level

lowerFirst <- function( S ) {
  substr(S, 0,1) <- tolower(substr(S,0,1))
  return(S)
}
}