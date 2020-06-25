#' Weighted Linear Calibration from instrumental data
#'
#' @param signal attribute
#' @param RSD attribute
#' @param conc attribute
#' @param element character
#' @param conc_unit character
#' @param signal_unit character
#'
#' @return
#' @export
#'
#' @examples calibrate(ICPMS$signal, ICPMS$RSD, ICPMS$conc, "Pb", "um", "ppb")
#'

calibrate <- function(signal, RSD, conc, element, conc_unit, signal_unit){
  w <- 1/(signal*RSD)^2
  model <- stats::lm(signal ~ conc, weights= w)

  slope <- model$coefficients[2]
  intercept <- model$coefficients[1]
  slope_STD <- summary(model)$coefficients[2,2]
  intercept_STD <- summary(model)$coefficients[1,2]

  plot(signal ~ conc,
       xlab= paste("Concentration of ", element, conc_unit),
       ylab= signal_unit)+
    graphics::abline(model, col="red")+
    graphics::title( paste("Calibration for", element))

  cat("Concentration", conc_unit, "=", slope, stringi::stri_escape_unicode("\u241"), slope_STD,"(x) + (", intercept, stringi::stri_escape_unicode("\u241"), intercept_STD,")")
  equation <- data.frame(element, slope, slope_STD, intercept, intercept_STD)
  }


#This function will calibrate data from an instrument (example: ICPMS)
#(weighted linear calibration)
#signal, RSD, conc should be columns of data (ex. ICPMS$signal)
#element should be what the instrument is testing for the concentration of as a character (ex. "Pb")
#units should be reported as characters (ex. "(ppb)")

#function returns a data frame that lists the element, slope, slope_STD, intercept, and intercept_STD
#function also plots a linear calibration curve
