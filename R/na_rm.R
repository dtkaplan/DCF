#' mean that sets na.rm to TRUE by default
#' 
#' @details I don't want to have to force intro students to remember
#' na.rm=TRUE
#' 
#' @param na.rm the setting I want by default
#' @param dots everything to be passed downstream to mosaic::mean or base::mean
#' @param data dataset (if any) to draw the variable from.
#' 
#' 
make_na.rm_fun <- function(primary,alternative) {
  F <- function(...,data=NULL, 
                ..fun..= 
                  ifelse(require(mosaic),
                         primary, alternative),
                na.rm = getOption("na.rm", TRUE)) {
    dots <- list(...)
    args <- list(dots, data=data, na.rm=na.rm)
    do.call(..fun.., args )
  }
  return(F)
}
#' @export
range <- make_na.rm_fun( mosaic::range, base::range )
#' @export
min <- make_na.rm_fun( mosaic::min, base::min )
#' @export
max <- make_na.rm_fun( mosaic::max, base::max )
#' @export
var <- make_na.rm_fun( mosaic::var, stats::var )
#' @export
median <- make_na.rm_fun( mosaic::median, stats::median )
#' @export
IQR <- make_na.rm_fun( mosaic::IQR, stats::IQR )
#' @export
sum <- make_na.rm_fun( mosaic::sum, base::sum )
#' @export
prod <- make_na.rm_fun( mosaic::prod, base::prod )
#' @export
fivenum <- make_na.rm_fun( mosaic::fivenum, stats::fivenum )
#' @export
var <- make_na.rm_fun( mosaic::var, stats::var )
# favstats is defined only in mosaic, so don't need it here
#' @export
cov <- function(x,y=NULL, use="pairwise.complete.obs", ... ){
   ..fun.. <- ifelse(require(mosaic), mosaic::cov, stats::cov )
   ..fun..( x, y, use=use, ...)
}
#' @export
cor <- function(x,y=NULL, use="pairwise.complete.obs", ... ){
  ..fun.. <- ifelse(require(mosaic), mosaic::cor, stats::cor )
  ..fun..( x, y, use=use, ...)
}

#' @export
mean <- function(...,data=NULL, 
                 ..fun..= 
                   ifelse(require(mosaic),
                          mosaic::mean, base::mean),
                 na.rm = getOption("na.rm", TRUE)) {
  dots <- list(...)
  x <- dots[[1]] # just the value
  if (length(dots) > 1)
    args <- list(x=x,dots[[2:length(dots)]],data=data,na.rm=na.rm)
  else 
    args <- list(x=x, data=data, na.rm=na.rm)
  do.call(..fun.., args )
}
#' @export
sd <- function(...,
               ..fun..= 
                 ifelse(require(mosaic),
                        mosaic::mean, base::mean),
               data=NULL, na.rm=getOption("na.rm", TRUE)) {
  dots <- list(...)
  x <- dots[[1]] # just the value
  if (length(dots) > 1)
    args <- list(x=x,dots[[2:length(dots)]],data=data,na.rm=na.rm)
  else 
    args <- list(x=x, data=data, na.rm=na.rm)
  do.call(..fun.., args )
}