#' margin of error
#'
#' @param n Total number of observations
#' @param p probabiliti
#' @param ci confidenz Intervall
#' @param alternative  alternative hypothesis
#' @param margin.error ME
#' @param sd   Streuung für Mittelwert
#' @param names erste Spalte
#' @param type "proportion", "mean"
#' @param main text ausgabe
#' @param digits  digits
#'
#' @return data.frame
#' @export
#'
#' @examples
#' 
#' # haendische Kontrolle
#' 

#' z<- 1.96
#' p <- .5
#' n<- 100
#' round((me = z*(sqrt(p*(1-p) / n))),3)
#' 
#' round(  p + c(-me, me),3)
#' margin_error(margin.error =me, p = c(.5, .4) )
#'
#' margin_error(margin.error = .025, p = .50)
#' margin_error(n = 1537, p = .50)
#' margin_error(margin.error = .025, n = 1536.584)
#' 
#' #' margin_error(sd = 100, margin.error = 25)
#' margin_error(n = 62, margin.error = 25, type="mean")
#' margin_error(n = 62, sd = 100)
#' 
#' 
#' margin_error(sd = c(10, 15), margin.error = 2.5)
#' margin_error(margin.error = .025, n = 1536.584)
#' # 
#' 
#' margin_error(margin.error = .025, p = c(.50, .60))
#' margin_error(margin.error = .025, n = c(1500, 2000))
#'
margin_error <- function(n = NULL,
                         margin.error = NULL,
                         p = NULL,
                         ci = 0.95,
                         sd = NULL,
                         
                         alternative = c("two.sided", "less", "greater"),
                         names = NULL,
                         type =  c("proportion", "mean"),
                         main = "Point Estimate of Population Proportion",
                         digits=3) {
  type <- match.arg(type)
  if (type == "mean" | !is.null(sd)) {
  
    margin_error.mean(
      n = n,
      margin.error = margin.error,
      sd = sd,
      ci = ci,
      alternative = alternative,
      names = names,
      digits=digits
    )
  }
  else{
    cat("\n", main, "\n")
    if (sum(sapply(list(n, margin.error, p), is.null)) != 1)
      stop("exactly one of n, margin.error or p must be NULL")
    alternative <- match.arg(alternative)
    
    if (alternative == "two.sided")
      zstar <- qnorm(1 - (1 - ci) / 2)
    else
      zstar <- qnorm(ci)
    
    if (is.null(n)){
      n <-  zstar ^ 2 * p * (1 - p) / margin.error ^ 2
      p<- formatC(p, digits, format = "f")}
    else if (is.null(margin.error)){
      margin.error = signif(zstar * sqrt(p * (1 - p) / n), digits)
      p <- formatC(p, digits, format = "f")
      }
    else  {
      z <-  (margin.error / zstar) ^ 2 * n
      p <- sqrt(abs(1 - 4 * z))
      
      p1 <- formatC( (1 - p) / 2, digits, format = "f")
      p2 <- formatC( (1 + p) / 2, digits, format = "f")
      p <-   paste( p1,p2, sep= "/")
    }
    

    
    max_length <- max(c(length(p), length(margin.error), length(n)))
    data.frame (
      Szenaeio = if (is.null(names))
        seq_len(max_length)
      else
        names,
      CI = ci,
      margin.error = margin.error,
      p = p,
      # n= n,
      N = ceiling(n)
    )
  }
}



# Sampling Size of Population Mean
# https://www.r-tutor.com/elementary-statistics/interval-estimation/sampling-size-population-mean
margin_error.mean <- function(n = NULL,
                              margin.error = NULL,
                              sd = NULL,
                              ci = .95,
                              alternative = c("two.sided", "less", "greater"),
                              names = NULL,
                              main = "Sampling Size of Population Mean",
                              digits= 2) {
  cat("\n", main, "\n")
  
  if (sum(sapply(list(n, margin.error, sd), is.null)) != 1)
    stop("exactly one of n, sd,  or  margin.error must be NULL")
  alternative <- match.arg(alternative)
  
  if (alternative == "two.sided")
    zstar <- qnorm(1 - (1 - ci) / 2)
  else
    zstar <- qnorm(ci)
  
  
  
  if (is.null(n)) {
    n <-  zstar ^ 2 * sd ^ 2 / margin.error ^ 2
  }
  else if (is.null(sd)) {
    sd <- sqrt(n / zstar ^ 2 * margin.error ^ 2)
  }
  else if (is.null(margin.error)) {
    margin.error = sqrt(zstar ^ 2 * sd ^ 2 / n)
  }
  
  max_length <- max(c(length(sd), length(margin.error), length(n)))
  data.frame (
    Szenaeio = if (is.null(names))
      paste("Szenario", seq_len(max_length))
    else
      names,
    CI = ci,
    margin.error = margin.error,
    SD = sd,
    # n= n,
    N = ceiling(n)
  )
  
  
  
}


