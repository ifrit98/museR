
#' @export
phi <- 1.61803398874989484820458683436563811772

#' @export
e <- 2.718281828459045235360287471352662497757247093


# enum(list(
#
# ), "parallel_minor")


tri <- triangle <- list(
  vertices = 3,
  edges = 3,
  angle = 60
)

sq <- square <- list(
  vertices = 4,
  edges = 4,
  angle = 90
)

pent <- pentagon <- list(
  vertices = 5,
  edges = 5,
  angle = 120
)

hex <- hexagon <- list(
  vertices = 6,
  edges = 6,
  angle = 150
)


#' Euler's relationship to Schlafli numbers for convex polytopes to find V
#' @export
vertices <- function(p, q) {
  return((4 * p) / (4 - (p - 2) * (q - 2)))
}

#' Euler's relationship to Schlafli numbers for convex polytopes to find E
#' @export
edges <- function(p, q) {
  (2 * p * q) / (4 - ((p - 2) * (q - 2)))
}

#' Uses Euler's relationship to Schlafli numbers for convex polytopes to find F
#' @export
faces <- function(p, q) {
  (4 * q) / (4 - ((p - 2) * (q - 2)))
}


arcsin <- asin

rad2deg <- function(r) (r / pi * 180) %% 360

deg2rad <- function(d) d * pi / 180

angular_defect <- function(p, q) 2 * pi - (q * pi) * (1 - (2 / p))

# dihedral_angle <-
  function(q, h) {
  x <- cos(pi / q) / sin(pi / h)

  atan(x) / 2
}


solid_angle <- function(dihedral_angle, q) q ^ dihedral_angle - (q - 2) * pi


is_integerish <- function(x, na.ok = FALSE)
    is.numeric(x) && all(x == suppressWarnings(as.integer(x)), na.rm = na.ok)

is_tensor <- function(x) inherits(x, "tensorflow.tensor")


#' @importFrom keras layer_lambda
#' @importFrom magrittr %<>%
#' @importFrom tensorflow tf
#' @export
layer_expand_dims <- function(object, axis = -1L) {
  axis %<>% as.integer()
  layer_lambda(object, function(x) {
    tf$expand_dims(x, axis = axis)
  })
}


#' @export
as_tensor <- function(x, dtype = NULL, coerce_to_mat = FALSE) {
  if (is.null(x))
    return(x)

  if (coerce_to_mat)
    x %<>% as.matrix()

  if (is_tensor(x) && !is.null(dtype))
    tf$cast(x, dtype = dtype)
  else {
    if (is_integerish(x) && isTRUE(dtype$is_integer))
      storage.mode(x) <- "integer"
    tf$convert_to_tensor(x, dtype = dtype)
  }
}



#' Named list.
#'
#' Convert a vector of values to symbolic named arguments to construct a list.
#' @export
nlist <- function(values) {
  chr_vals <- as.character(values)
  symbols <- dplyr::syms(chr_vals)
  names <- zip(symbols, values)
  out <- list()

  for (nm in names) out[[nm[[1]]]] <- nm[[2]]

  as.list(out)
}


# TODO: add support for rotations longer than `length(x)`
#' @export
rotate <- function(x, n) {
  stopifnot(n < length(x))
  if (n == 0) return(x)

  c(x[(n+1):length(x)], x[1:n])
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
#' @export
is_list <- function(x) is.vector(x) && !is.atomic(x)


#' Is x explicitly a vector with length(vector) > 1
#' @export
is_vec  <- function(x) is.vector(x) & length(x) != 1L


#' Is x an (array, list, vector) with length(x) > 1
#' @export
is_vec2 <- function(x) (is_list(x) & length(x) > 1L) | is_vec(x)


#' split and return integer and decimal portion of a numeric.
#' @export
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
#' @export
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
#' @export
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
#' @export
pow2_up_to <- function(n) {
  x <- floor(log2(n))
  powers_of_2(x)
}


#' List powers of 2 up to a given exponent, x
#' @export
powers_of_2 <- function(x) {
  x <- as.integer(x)
  l <- seq(1L, x)
  vapply(l, function(p) 2^p, 0)
}



#' Get all divisors of a given number
#' @export
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


#' @export
`%within2%` <- function(x, y) {
  in_range(x, y, 2L)
}


#' @export
within2 <- function(x, y) x %within2% y



`%within5%` <- function(x, y) {
  in_range(x, y, 2L)
}


`%within.1%` <- function(x, y) {
  in_range(x, y, 0.1)
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

