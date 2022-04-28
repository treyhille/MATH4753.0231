#' @title The bootstrap function creates a confidence interval based on a given data set.
#'
#' @param iter Number of iterations
#' @param x A dataframe or data set
#' @param fun Which type of function you would like to apply to the data, ex. "mean"
#' @param alpha A number, such that (1 - alpha) is the percentage confidence
#' @param cx A number, for applying text size
#' @param ... Extra graphical parameters passed on to the hist() function
#'
#' @return Returns a histogram of the distribution of the sampled data from the population. On the histogram is a labeled confidence interval.
#' @importFrom stats quantile
#' @importFrom graphics hist abline segments text
#' @export
#'
#' @examples
#' \dontrun{myboot2(iter = 10000, x = ddt$DDT, fun = "var", alpha = 0.2)}
myboot2 <- function(iter = 10000, x, fun = "mean", alpha = 0.05, cx = 1.5, ...)
{
  n = length(x)
  y = sample(x, n*iter, replace = TRUE)

  rs.mat = matrix(y, nrow = n, ncol = iter, byrow = TRUE)
  xstat = apply(rs.mat, 2, fun)
  ci = quantile(xstat, c(alpha/2, 1 - alpha/2))

  para = hist(xstat, freq = FALSE, las = 1,
              main = paste("Histogram of Bootstrap sample statistics", "\n", "alpha = ", alpha, " iter = ", iter, sep=""), ...)

  mat = matrix(x, nrow = length(x), ncol = 1, byrow = TRUE)
  pte = apply(mat, 2, fun)
  abline(v = pte, lwd = 3, col = "Black")
  segments(ci[1], 0, ci[2], 0, lwd = 4)
  text(ci[1], 0, paste("(", round(ci[1], 2), sep = ""), col = "Red", cex = cx)
  text(ci[2], 0, paste(round(ci[2], 2), ")", sep = ""), col = "Red", cex = cx)

  text(pte,max(para$density)/2,round(pte,2),cex=cx)

  invisible(list(ci=ci,fun=fun,x=x,xstat=xstat))
}
