


#'  Drop-out rate
#'
#' @param n   is the sample size required
#' @param d   is the dropout rate
#'
#' @return vector
#' @export
#'
#' @examples
#'
#' drop_out_rate(100, 1/20)
drop_out_rate <- function(n, d=.10){
  ceiling( n/ (1-d))
}



#'  Unequal Treatment Allocation
#'
#' @param n  is the sample size required
#' @param k ratio
#'
#' @return vector
#' @export
#'
#' @examples
#'
#' unequal_treatment(n= 100, k =2/1)
#'
unequal_treatment <- function(n, k) {
  c(Control =  (0.5) * n * (1 + (1 / k)),
    Treat = (0.5) * n * (1 + k))

}



