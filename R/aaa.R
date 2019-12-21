
#' Named list.
#'
#' Convert a vector of values to symbolic named arguments to construct a list.
nlist <- function(values) {
  chr_vals <- as.character(values)
  symbols <- dplyr::syms(chr_vals)
  names <- zip(symbols, values)
  out <- list()

  for (nm in names) out[[nm[[1]]]] <- nm[[2]]

  as.list(out)
}



#' @export
chromatic <-
  c('A', 'Bb', 'B', 'C', 'C#', 'D', 'Eb', 'E', 'F', 'F#', 'G', 'Ab')


#' @export
chromatic.full <-
  c('A', 'A#', 'Bb', 'B', 'B#', 'Cb', 'C',
    'C#', 'Db', 'D', 'D#', 'Eb', 'E', 'E#',
    'Fb', 'F', 'F#', 'Gb', 'G', 'G#', 'Ab')


#' @export
thetas <- seq(0, 330, 30)



zip <- function(x, y) {
  purrr::map2(x, y, ~c(..1, ..2))
}


# TODO: add name support for x argument
unzip <- function(x, collapse = FALSE) {
  xs <- purrr::map_depth(l, 1, ~..1[[1]])
  ys <- purrr::map_depth(l, 1, ~..1[[2]])

  if (collapse)
    return(c(xs, ys))

  list(xs = xs, ys = ys)
}



#' Is x explicitly a list type
#'
#' @param x input objects
#' @examples
#' is_list(c(1,2,3))
#' is_list(list(99))
is_list <- function(x) is.vector(x) && !is.atomic(x)


#' Is x explicitly a vector with length(vector) > 1
is_vec  <- function(x) is.vector(x) & length(x) != 1L


#' Is x an (array, list, vector) with length(x) > 1
is_vec2 <- function(x) (is_list(x) & length(x) > 1L) | is_vec(x)


#' split and return integer and decimal portion of a numeric.
split_numeric <- function(x) {
  int <- trunc(x)
  dec <- x - int

  c(int, dec * sign(x))
}


#' Compute bth root of a
#' This is a so-called Vectorized function, i.e.
#' f(A,B) == c(f(A[1],B[1]), f(A[2],B[2]), ...) for vectors A, B
#' of equal length.
#'
#' This does *not* handle the case where b is the inverse of an integer
#' (e.g. nthroot(-1,1/3) ==? (-1)^3), since in general you cannot count
#' on 1/(1/int) being an integer.

#' If on the other hand you want the complex principal value of the nth
#' root, you can use (a+0i)^(1/b).
nroot <- function(a, b) {
  if (b %% 2 == 1 | a >= 0) return(sign(a) * abs(a)^(1 / b))
  NaN
}


#' Compute n primes
#'
#' Try to keep it under 5000, or your CPU might explode
#'
#' Microbenchmark for n_primes (microseconds):
#'      expr             min         lq       mean
#' n_primes(10)       72.402       75.441   82.7702
#' n_primes(100)     2687.685     2852.065  2945.43
#' n_primes(1000)   417199.246   417258.025 426916
#' n_primes(2000)  1860778.072  1924292.325 1992891
#' n_primes(3000)  4366437.459  4403578.398 4413822
#' n_primes(5000) 13054581.632 13071920.879 13494860
n_primes <- function(length.out) {
  n <- length.out
  i <- 3L
  count <- 2L
  primes <- vector("double", n)
  primes[1] <- 2

  while (count <= n) {

    for (c in 2:i) if (i %% c == 0) break

    if (c == i) {
      primes[count] <- i
      count <- count + 1L
    }
    i <- i + 1L
  }

  primes
}


#' Return both quotient and modulus
mod <- function(a, b) c(quotient = floor(a / b), modulo = a %% b)



#' Powers of 2 up to a given number.
#'
#' Takes log2 of n and calculates 2^x, where
#' x ranges from 1:log2(n)
pow2_up_to <- function(n) {
  x <- floor(log2(n))
  powers_of_2(x)
}


#' List powers of 2 up to a given exponent, x
powers_of_2 <- function(x) {
  x <- as.integer(x)
  l <- seq(1L, x)
  vapply(l, function(p) 2^p, 0)
}



#' Get all divisors of a given number
get_divisors <- function(x) {
  i   <- 1L
  j   <- 1L
  div <- c()
  while (i <= x) {
    if (x %% i == 0L) {
      div[j] <- i
      j <- j + 1L
    }
    i <- i + 1L
  }
  div
}


#' is x within 1 of y
#'
#' These two calls are equivalent
#' 3 $within1% 4
#' in_range(3, 4, 1)
#' @export
`%within1%` <- function(x, y) {
  in_range(x, y, 1L)
}


#' returns whether x is within a specific delta range of y.
#' @export
#' @examples
#'  in_range(4, 6, err = 2)
in_range <- function(x, y, err = 1) {
  ymax <- y + err
  ymin <- y - err

  ymin <= x & x <= ymax
}

