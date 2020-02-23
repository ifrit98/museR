example_ft <- function(t) e^(-t) * cos(5*t)

#' Naive fourier (magnitude plot) transform function to plot magnitude
fourier_magnitude <- function(ft = example_ft, w_size = 50, precision = 0.01) {
  ws <- seq(0, w_size, precision)

  f <- function(w, TM) {
    c_area <- integrate(function(t) ft(t) * cos(w*t), 0, 1)[[1]]
    s_area <- integrate(function(t) ft(t) * sin(w*t), 0, 1)[[1]]
    mag <- sqrt(c_area^2 + s_area^2) # plot @ w == 0

    mag
  }

  pts <- vapply(ws, f, 0)

  plot(pts)
}


# .integrate <-
  function() {
  library(zoo)

  x <- 1:10
  y <- 3*x+25
  id <- order(x)

  AUC <- sum(diff(x[id]) * zoo::rollmean(y[id],2))

}

#' Projection onto new basis
#' @export
new_coords <-
  function(x, y, theta) {
    .x <- x * cos(theta) - y * sin(theta)
    .y <- x * sin(theta) + y * cos(theta)
  }



#' @export
untwist <-
  function(k, x, y, f, N) {
    .x <- x * cos((-2 * pi * f * k) / N) - y * sin((-2 * pi * f * k) / N)
    .y <- x * sin((-2 * pi * f * k) / N) + y * cos((-2 * pi * f * k) / N)

    c(.x, .y)
  }


#' @export
untwist.loop <-
  function(X, Y, f, N) {
    .X <- vector(length = N)
    .Y <- vector(length = N)
    for (k in 1:N) {
      .x <- X[k] * cos((-2 * pi * f * k) / N) - y * sin((-2 * pi * f * k) / N)
      .y <- Y[k] * sin((-2 * pi * f * k) / N) + y * cos((-2 * pi * f * k) / N)
      .X[k] <- .x
      .Y[k] <- .y
    }
    tibble::tibble(x = .X, y = .Y)
  }


#' @export
untwist.vec <-
  function(X, Y, f, N) {

    .x <- x * cos((-2 * pi * f * k) / N) - y * sin((-2 * pi * f * k) / N)
    .y <- x * sin((-2 * pi * f * k) / N) + y * cos((-2 * pi * f * k) / N)

  }


#' @export
untwist.e <-
  function(X, Y, f, N) {

    z <- function(x, y) complex(real = x, imaginary = y)
    Z <- vector(length = N)

    for (k in seq(N))
      (Z[k] <- z(X[k], Y[k]) * exp(1)^exp(-2 * pi * 1i * f * k / N))

    Z
  }


#' @export
untwist.p <-
  function(X, Y, .F, N) {

    untwist.f <- purrr::partial(untwist.e, X = X, Y = Y, N = N)

    plots <- sapply(.F, function(f) {
      zs <- untwist.f(f = f)
      p <- ggplot2::qplot(Re(zs), Im(zs), geom = 'path')
      print(p)
      p
    })

    plots
  }


#' TODO: Add gganimate or googleVis to concat frames into a video
#' TODO: add circle diagrams for plotting (unit circle for tone diagrams)
#' Plot untwisting motion of a given frequency in the complex plane
#' USE Plotly for polar charts to mimic tone circles
#' @export
untwist.viz <-
  function(X, Y, .F, N = length(X)) {

    untwist.f <- purrr::partial(untwist.e, X = X, Y = Y, N = N)

    for (i in seq(length(.F))) {
      zs <- untwist.f(f = .F[i])
      print(
        ggplot2::qplot(Re(zs), Im(zs), geom = 'path') +
        ggplot2::ggtitle(paste("Frequency:", .F[i], "Hz")))
      invisible(readline(prompt = "Press [enter] to continue"))
    }
  }



# TODO: Find center of z axis (axis of rotational symmetry)
#' Untwist one (fourier component) for a specific frequency
#' @export
untwist.one <- function(freq, .rotations) {
  X <- Y <- 1:.rotations

  untwist.f <- purrr::partial(untwist.e, X = X, Y = Y, N = length(X))

  zs <- untwist.f(f = freq)

  ratio <-
    if(.rotations > freq)
      freq / .rotations
    else
      .rotations / freq

  print(
    ggplot2::qplot(Re(zs), Im(zs), geom = 'path') +
    ggplot2::ggtitle(paste("Frequency:", freq,
                           "Hz   Rotations:", .rotations,
                           "   Ratio:", ratio)))
}


#' Untwist around a given frequency by computing the number of rotations
#'
#' Inverse of untwist.
#' @export
untwist.ratio <- function(ratio, freq) {
  rotations <- ceiling(ratio * freq)
  untwist.one(freq, rotations)
}



#' Use untwist.one to iterate through a range of rotation values for a "moving
#' image" feel.
#' @export
untwist.range <- function(freq, rotation_range = seq(10, 250, 7)) {
  for (rotation in rotation_range) {
    untwist.one(freq, rotation)
    invisible(readline("Next..."))
  }
}


# TODO: recompute rotations for each frequency?
# (i.e. fourier component at different rotational speeds)
#' @export
demo_untwisting <- function(examples = 10, .rotations = 100) {

  k <- 1:examples
  x <- 1:.rotations
  y <- 1:.rotations

  df <- demo_df(examples)

  Z <- untwist.e(x, y, df$solfeg, .rotations)

  untwist.viz(x, y, df$solfeg)

  untwist.viz(x, y, df$mul17)

  untwist.viz(x, y, df$primes)

  untwist.viz(x, y, df$seq)

  # ggplot2::qplot(Re(Z), Im(Z), geom = 'path')

  f <- 512
  N <- 10
  untw <- purrr::partial(untwist, f = f, N = N)
  points <- purrr::pmap(list(k, x, y), untw)

  xs <- sapply(points, function(x) x[1])
  ys <- sapply(points, function(x) x[2])

  tbl <- tibble::tibble(x = xs, y = ys)
  ggplot2::ggplot(tbl) + ggplot2::geom_point(aes(x = x, y = y))

  tbl <- untwist.loop(x, y, f, N)
  ggplot2::ggplot(tbl) + ggplot2::geom_point(aes(x = x, y = y))

}


#' @export
demo_df <- function(n = 100) {
  library(tibble)

  .S  <- solfeggio_scale_n(n)
  .F  <- powers_of_2(n)
  .F2 <- seq(1, n*17, 17)
  .F3 <- n_primes(n)
  .F4 <- seq(n)

  tibble(pow2 = .F, mul17 = .F2, primes = .F3, seq = seq(n), solfeg = .S)
}
