





#' margin of error
#'
#' @param n Total number of observations
#' @param margin.error The margin of error
#' @param p probabiliti
#' @param ci confidenz Intervall
#' @param alternative  alternative hypothesis
#'
#' @return
#' @export
#'
#' @examples
#' 
#' margin.error(margin.error = .025, p = .50)
#' margin.error(n = 1537, p = .50)
#' margin.error(margin.error = .025, n = 1536.584)
#' 
margin.error <- function(n = NULL,
                         margin.error = NULL,
                         p = NULL,
                         ci = 0.95,
                         alternative = c("two.sided", "less", "greater")) {
  if (sum(sapply(list(n, margin.error, p), is.null)) != 1)
    stop("exactly one of n, d, power, and sig.level must be NULL")
  alternative <- match.arg(alternative)
  
  if (alternative == "two.sided")
    zstar <- qnorm(1 - (1 - ci) / 2)
  else
    zstar <- qnorm(ci)
  
  if (is.null(n))
    n <-  ceiling(zstar ^ 2 * p * (1 - p) / margin.error ^ 2)
  else if (is.null(margin.error))
    margin.error = signif(zstar * sqrt(p * (1 - p) / n), 3)
  else  {
    z <-  (margin.error / zstar) ^ 2 * n
    x <- sqrt(abs(1 - 4 * z))
    
    p <-
      paste(formatC(c((1 - x) / 2, (1 + x) / 2), 3, format = "f"), collapse =
              ", ")
    
  }
  
  data.frame(
    variable = c("ci", "margin.error", "p", "n"),
    value = c(ci,
              margin.error,
              p,
              formatC(n, 0, format = "f"))
  )
  
}




