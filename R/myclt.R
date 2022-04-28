#' @title This function creates a random sample from a uniform distribution and creates plots based on the shape of the sample distribution.
#'
#' @param n Number of trials
#' @param iter Number of iterations
#' @param a A number, representing the minimum value of the sample distribution.
#' @param b A number, representing the maximum value of the sample distribution.
#'
#' @return Returns a plot of the shape of the sample distribution taken randomly from a uniform distribution.
#' @importFrom stats runif
#' @importFrom graphics hist
#' @export
#'
#' @examples
#'
#' @examples
#' \dontrun{myclt(n = 20, iter = 100000, a = 0, b = 10)}

myclt = function(n = 1, iter = 10000, a = 0, b = 5)
{
  y = runif(n*iter, a, b) #A
  data = matrix(y, nrow = n, ncol = iter, byrow = TRUE) #B
  sm = apply(data, 2, sum) #C
  hist(sm, freq = FALSE, main = "Distribution of the sum of uniforms")
}
