#' Euclidean algorithm
#' Computes the greatest common divisor of two integers.
#'
#' @details
#' The algorithm repeatedly replaces the pair (a,b) with (b,r), where r is the
#' remainder of a divided by b, until b is zero. The remaining value of a is
#' then the greatest common divisor. the sing is removed at the end, the result
#' is always non-negative
#'
#' @param a A numeric scalar
#' @param b A numeric scalar.
#'
#' @return A numeric scalar: the greatest common divisor of \code{a} and
#' \code{b}
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
#'
#' @examples
#' euclidean(123612, 13892347912)
#' euclidean(100, 1000)
#'
#' @export



euclidean <- function(a,b){
  stopifnot(
    is.numeric(a) && length(a) == 1,
    is.numeric(b) && length(b) == 1
  )

  while(b != 0){
    temp <- b
    b <- a %% b
    a <- temp
  }

  abs(a)
}
