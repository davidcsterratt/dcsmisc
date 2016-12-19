##' Compute the number of necklaces of length \code{n} formed from an
##' alphabet with \code{k} symbols. See
##' \url{https://en.wikipedia.org/wiki/Necklace_(combinatorics)}.
##'
##' @title Number of necklaces
##' @param k Number of symbols in the alphabet
##' @param n Length of string
##' @return Number of necklaces
##' @author David Sterratt
##' @importFrom numbers eulersPhi
##' @export
necklace <- function(k, n) {
  d <- divisors(n)
  return(1/n*sum(sapply(d, numbers::eulersPhi) * k^(n/d)))
}

##' Find the integer divisors (including \code{1} and \code{n}) of \code{n}.
##' 
##' @title Find divisors
##' @param n Number to find divisors of
##' @return The divisors
##' @author David Sterratt
##' @export
divisors <- function(n) {
  ## FIXME: logic for n = 0 should be implemented
  return((1:n)[(n %% 1:n) == 0])
}
