#' mymaxlikg Creates a graphical likelihood for a probability.
#'
#' @param lfun A likelihood function, must be definied outside the mymaxlikg function.
#' @param theta A sequence between 0 and 1 representing the range in which the probability can realistically lie.
#' @param ... Extra parameters passed on to plot function.
#'
#' @return Returns a graphical likelihood of a probability based on a supplied likelihood function.
#' @importFrom graphics abline axis
#'
#' @export
#'
#' @examples
#' \dontrun{mymaxlikg(lfun = "logbin2", theta = seq(0,1,length=1000)}
mymaxlikg = function(lfun, theta, ...) {
  nth = length(theta)
  thmat = matrix(theta, nrow = nth, ncol = 1, byrow = TRUE)
  z = apply(thmat, 1, lfun)
  zmax = max(which(z == max(z)))
  plot(theta, exp(z), type = "l", ...)
  abline(v = theta[zmax], col = "Blue")
  axis(3, theta[zmax], round(theta[zmax], 4))
  theta[zmax]
}
