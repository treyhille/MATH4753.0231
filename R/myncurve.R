#' @title This function plots a probability for a normal curve.
#'
#' @param mu Mean
#' @param sigma Standard Deviation
#' @param a Probability upper limit
#'
#' @return Returns a plot and numerical value for the probability of a given number of successes for a normal distribution.
#' @export
#'
#' @examples
#' \dontrun{myncurve(10, 4, 3)}

myncurve = function(mu, sigma, a)
{
  x <- NULL
  graphics::curve(stats::dnorm(x, mean = mu, sd = sigma), xlim = c(mu - (3*sigma), mu + (3*sigma)))
  xcurve <- seq((mu - (3*sigma)), a, length = 1000)
  ycurve <- stats::dnorm(xcurve, mu, sigma)
  graphics::polygon(c((mu - (3*sigma)), xcurve, a), c(0, ycurve, 0), col = "RED")
  area <- round(stats::pnorm(a, mu, sigma), 4)
  list(Area = area)
}
