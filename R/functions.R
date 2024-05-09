#' Convert km to miles
#'
#' This function interprets a numerical value in terms of the distance
#' unit kilometers and converts it to miles.
#'
#' @usage km_to_miles(km)
#' @param km Distance in km
#' @return Distance in miles
#' @author Fateema Bazzi\cr Politecnico di Milano\cr Maintainer: Fateema
#' Bazzi\cr E-Mail: <fateemahani.bazzi@@mail.polimi.it>
#' @references \url{https://en.wikipedia.org/wiki/Mile}\cr
#' @seealso \code{\link{miles_to_km}}\cr
#' \code{\link{miles_to_km2}}\cr
#' @examples
#'
#' km_to_miles(1)
#'
#' @export
km_to_miles <- function(km) {
  # check this is numeric
  if (!is.numeric(km)) {
    stop("argument is not numeric!")
  }
  return( 0.621371 * km )
}



#' Convert miles to km
#'
#' This function interprets a numerical value in terms of the distance
#' unit miles and converts it to kilometers.
#'
#' @usage miles_to_km(miles)
#' @param miles Distance in miles
#' @return Distance in kilometers
#' @author Fateema Bazzi\cr Politecnico di Milano\cr Maintainer: Fateema
#' Bazzi\cr E-Mail: <fateemahani.bazzi@@mail.polimi.it>
#' @references \url{https://en.wikipedia.org/wiki/Mile}\cr
#' @seealso \code{\link{km_to_miles}}\cr
#' \code{\link{miles_to_km2}}\cr
#' @examples
#'
#' miles_to_km(1)
#'
#' @export
miles_to_km <- function(miles) {
  # check this is numeric
  if (!is.numeric(miles)) {
    stop("argument is not numeric!")
  }
  return( miles/0.621371 )
}



#' Convert miles to km
#'
#' This function interprets a numerical value in terms of the distance
#' unit miles and converts it to kilometers.
#'
#' @usage miles_to_km2(miles)
#' @param miles Distance in miles
#' @return Distance in kilometers
#' @author Fateema Bazzi\cr Politecnico di Milano\cr Maintainer: Fateema
#' Bazzi\cr E-Mail: <fateemahani.bazzi@@mail.polimi.it>
#' @references \url{https://en.wikipedia.org/wiki/Mile}\cr
#' @seealso \code{\link{km_to_miles}}\cr
#' \code{\link{miles_to_km}}\cr
#' @examples
#'
#' miles_to_km2(1)
#'
#' @importFrom measurements conv_unit
#' @export
miles_to_km2 <- function(miles) {
  # check this is numeric
  if (!is.numeric(miles)) {
    stop("argument is not numeric!")
  }
  return( measurements::conv_unit(miles, "mi", "km") )
}
