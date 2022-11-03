

#' Convert between *d*, *r* and *Odds ratio*
#'
#' @param d Cohen's d
#' @param r Pearson's correlation r
#' @param r2 R-squared
#' @param f,f2 Cohen's f
#' @param eta2 eta
#' @param ods Odds ratio (OR)
#' @param log.ods Log odds ratio
#' @param digits digits
#'
#' @return vector
#'
#' @export
#' @examples
#'
#'  convert(d=.30, digits= 4)
#' convert(r=.14834)
#' convert(r2=.0220)
#' convert(f=.1500 )
#' convert(f2=.0225)
#' convert(eta2=.0220)
#' convert(ods=1.7231)
#' convert(log.ods=0.5441)

convert <- function(d = NULL,
                    r = NULL,
                    r2 = NULL,
                    f = NULL,
                    f2 = NULL,
                    eta2 = NULL,
                    ods = NULL,
                    log.ods = NULL,
                    digits = 2) {

  if (!is.null(d))            d <- d
  else if (!is.null(r))       d <- effectsize::r_to_d(r)
  else if (!is.null(r2))      d <- effectsize::r_to_d(sqrt(r2))
  else if (!is.null(f))       d <- f_to_d(f)
  else if (!is.null(f2))      d <- f_to_d(sqrt(f2))
  else if (!is.null(eta2))    d <- f_to_d(sqrt(effectsize::eta2_to_f2(eta2)))
  else if (!is.null(ods))     d <- effectsize::oddsratio_to_d(ods)
  else if (!is.null(log.ods)) d <- logoddsratio_to_d(log.ods)
  else
    return(NULL)

  round(
    c(
      d =  d,
      r = effectsize::d_to_r(d),
      r2 = effectsize::d_to_r(d) ^ 2,
      f = d_to_f(d),
      f2 = d_to_f2(d),
      eta2 = effectsize::f2_to_eta2(d_to_f2(d)),
      ods = effectsize::d_to_oddsratio (d),
      log.ods = d_to_logoddsratio(d)
    ),
    digits
  )


}




d_to_f <- function(d) {  d / 2  }
f_to_d <- function(f) {  f * 2  }
d_to_f2 <- function(d) {  (d / 2) ^ 2  }
f2_to_d  <- function(f2) {  f_to_d (sqrt(f2))  }
logoddsratio_to_d <- function(log.ods) {(log.ods * sqrt(3)) / pi    }
d_to_logoddsratio <- function(d) {  (d * pi) / sqrt(3)  }





