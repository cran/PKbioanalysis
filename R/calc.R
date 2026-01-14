#' Normalize the area of a compound by the internal standard
#' @param c1_area numeric
#' @param IS_area numeric
#' @details We here assume the IS concentration is constant across all samples
#' @noRd
normalizeIS <- function(c1_area, IS_area) {
  checkmate::assertNumeric(c1_area, finite = TRUE)
  checkmate::assertNumeric(IS_area, finite = TRUE)

  c1_area / IS_area
}

response_factor <- function(peak_area, conc) {
  checkmate::assertNumeric(peak_area, finite = TRUE)
  checkmate::assertNumeric(conc, finite = TRUE)
  peak_area / conc
}

RRF <- function(RF1, RF2) {
  checkmate::assertNumeric(RF1, finite = TRUE)
  checkmate::assertNumeric(RF2, finite = TRUE)
  RF1 / RF2
}


#' Calculate Coefficient of variation
#' @param x vector
#' @param percent To return the value as percentage
#'
#' @details A simple calculation of the coefficient of variation (CV) is done
#' as the standard deviation divided by the mean. By default, the result is in percentage.
#' @return numeric
#' @export
cv <- function(x, percent = TRUE) {
  p <- stats::sd(x) / mean(x)
  p * ifelse(percent, 100, 1)
}

accuracy <- function(x, y, percent = TRUE) {
  p <- x / y 
  p * ifelse(percent, 100, 1)
}



#' Calculate relative error (deviation) between actual and predicted concentration
#' @param actual numeric
#' @param predicted numeric
#' @return numeric
#' @author Omar Elashkar
#' @noRd
rel_deviation <- function(x, y, percent = TRUE) {
  p <- (x - y) / y
  p * ifelse(percent, 100, 1)
}

mape <- function(x, y, percent = TRUE) {
  p <- mean(abs(rel_deviation(x, y, percent = FALSE)), na.rm = TRUE) 
  p * ifelse(percent, 100, 1)
}

mae <- function(x, y) {
  p <- mean(abs(x - y))
  p
}

mse <- function(x, y) {
  p <- mean((x - y)^2)
  p 
}

rmse <- function(x, y) {
  p <- sqrt(mse(x, y))
  p
}

#' Calculate residual sum of squares
#' @param actual numeric
#' @param predicted numeric
#' @return numeric
#' @author Omar I. Elashkar
#' @noRd
rss <- function(x, y) {
  sum((x - y)^2)
}

#' Calculate residual standard error
#' @param residuals numeric
#' @return numeric
#' @author Omar I. Elashkar
#' @noRd
# https://stackoverflow.com/questions/71545329/inconsistence-with-rs-residual-standard-error-in-lm-in-case-of-wls
rse <- function(residuals) {
  SSE <- sum(residuals)
}