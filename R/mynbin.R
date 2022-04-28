#' @title This function calculates probability of successes based on a negbin distribution.
#'
#' @param y Number of trials
#' @param r Number of successes
#' @param p Probability of success
#'
#' @return Returns the probability of the number of successes based on a negbin distribution.
#' @export
#'
#' @examples
#' \dontrun{mynbin(10, 3, 0.4))}

mynbin = function(y, r, p)
{
  choose(y-1, r-1) * p^r * (1-p)^(y-r)
}
