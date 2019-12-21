# TODO:
# Add plotting in complex plane
# Complex Constellation plots?
# Tone circle plots with paths!
# Paths through complex plane (harmonic sequences, e.g. Roel's world)
# Tone circle diagarams with subset shapes in modulus space! (e.g.
# Augmented triad as equilateral triangle w 60 degree angles)



#' Return direct ratios of just intonation system
#' @export
just_intervals <- function(simplify = TRUE) {
  intervals <- list(
    m2 = 16 / 15, # m2
    M2 = 9 / 8, # M2
    m3 = 6 / 5, # m3
    M3 = 5 / 4, # M3
    P4 = 4 / 3, # P4
    tt = 7 / 5, # Tritone F#
    tt = 10 / 7, # Tritone Gb
    P5 = 3 / 2, # P5
    m6 = 8 / 5, # m6,
    M6 = 5 / 3, # M6
    m7 = 16 / 9, # m7
    M7 = 15 / 8, # M7
    oct = 2 / 1 # octave
  )

  if (simplify) return(unlist(intervals) %>% unname)

  intervals
}


#' Solfeggio frequencies in Hertz
#' @export
solfeggio_freqs <- function(simplify = FALSE, extended = FALSE) {
  solfeggio <- if (extended)
    list(
      a   = 174,
      b   = 285,
      ut  = 396,
      re  = 417,
      mi  = 528,
      fa  = 639,
      sol = 741,
      la  = 852,
      z   = 963
    )
  else
    list(
      ut  = 396,
      re  = 417,
      mi  = 528,
      fa  = 639,
      sol = 741,
      la  = 852
    )
  if (simplify) return(unlist(unname(solfeggio)))

  solfeggio
}


# WORK IN PROGRESS
# TODO: recycle to desired length (n)
# solfeggio_scale_n <-
  function(n, p = 174) {
  c(base, lag) %<-% solfeggio_ratios(return_both = TRUE)

  out <- vector("numeric", n)
  quo <- mod(n, length(base))[[1]]
  for (i in 1:quo) {
    s <- sapply(base, function(x) `*`(p, x))
    browser()
    out[i:i+quo] <- s # Indices mismatch
    p <- s[[length(s)]]
  }

  sapply(lag, function(x) `*`(p, x))
  }


#' Arbitrary length solfeggio scale
#' @export
solfeggio_scale_n <- function(n, p = 174) {
  lag_ratios <- solfeggio_ratios(return_both = TRUE)[[2]]
  lag_ratios <- lag_ratios[-1]

  c(reps, extra) %<-% mod(n + 1, length(lag_ratios))

  num_reps <- if (extra > 0) reps + 1 else reps

  f <- p
  .f <- f
  c <- 1
  freqs <- vector("list", length(lag_ratios) * num_reps)

  for (j in 1:num_reps) {
    for (i in 1:length(lag_ratios)) {
      f <- f * lag_ratios[[i]]
      freqs[[c]] <- f
      c <- c + 1
    }
  }

  out <- c(.f, freqs %>% unlist)
  out[1:n]
}



#' Get numeric ratio for solfeggio scale
#' @export
solfeggio_ratios <- function(extended = TRUE, return_both = FALSE) {
  tones  <- solfeggio_freqs(TRUE, extended)
  lagged <- dplyr::lag(tones)

  # If part of a 9-tone system (span 174-963Hz)
  base_ratios <-
    vapply(tones, function(x) x / tones[1], 0)

  # If lagged (ratio of x to x-1, sequential arbitrary span)
  lag_ratios <-
    purrr::map2(tones, lagged, function(x, y) x / y) %>% unlist

  lag_ratios[1] <- 1

  if (return_both)
    return(list(base_ratios, lag_ratios))

  base_ratios
}


#' Apply ratios to a given base frequency (Default: 174Hz)
#' @export
get_solfeggio_scale <-
  function(extended = TRUE, base = if (extended) 174 else 396) {
    tones <- solfeggio_ratios(extended)
    sapply(tones, function(x) `*`(base, x))
}


# TODO: Create function to apply weights to the walk (e.g. prefer 3rds)
#' Generate an n-tone random walk sequence of intervals (equal temperament)
#' @export
tone_random_walk <-
  function(n = 12,
           start_pitch = 440,
           length.out = 100,
           weights = NULL) {
    if (!is.null(weights))
      stopifnot(identical(length(weights), n))

    interval_set <- tone_ratios(n)

    seq    <- sample(interval_set, length.out, replace = TRUE)
    updown <- sample(c(1, -1), length.out, TRUE)

    seq <-
      purrr::map2(seq, updown, function(x, y) x * y) %>% unlist

    f <- function(p, x) {
      c(i, d) %<-% split_numeric(x)
      p <- p + p * i * d
      p
    }

    output <- vector("numeric", length.out+1)
    output[[1]] <- start_pitch
    current_pitch <- start_pitch

    for (i in 2:length.out) output[[i]] <- f(current_pitch, seq[i])

    output
  }


#' @export
swap_nms_vals <- function(x) {
  nms  <- names(x)
  vals <- unname(x)
  names(nms) <- vals
  l <- as.list(nms)
  l
}


#' @export
flip_names_and_values <- swap_nms_vals


# WIP
nearest_pitch <- function(f) {
  445 -> 440 -> A
  diffs <- map2((pitch.hertz.440 %>% rotate(1)), pitch.hertz.440, `-`)

  medians <-
    diffs %>%
    unname %>%
    unlist  %>% `/`(., 2) %>%
    `+`(., pitch.hertz.440 %>% unlist %>% unname)

  op <- pitch.hertz.440 %>% unlist %>% unname
  #  (p >= 440 & p <= (453))
  full <- JSGutils:::interleave2(op, medians)

  lapply(1:(length(full)-1), function(i) {
    print(sprintf("%f >= %f && %f <= %f", p, full[[i]], p, full[[i+1]]))
    (p >= full[[1]]) && (p < full[[2]])
  })



}


#' @export
hertz_to_pitch <- function(f) {
  within1 <- function(x) f %within1% x
  chromatic[sapply(pitch.hertz.440, within2)]
}


#' @export
pitch_to_hertz <- function(p) {
  pitch.hertz.440[[substitute(p)]]
}


#' d distance from root (origin)
#' n nth root (base of system e.g. 12)
#' @export
compute_nth_tone_ratio <- function(d, n) {
  e <- exp(1)
  ratio <- e^((d/n) * log(2))

  ratio
}


#' Return all n tone ratios of nth base
#' @export
tone_ratios <- function(n) {
  compute_n_ratio <- purrr::partial(compute_nth_tone_ratio, n = n)
  sapply(0:n, compute_n_ratio)
}


#' Inverse `tone_ratios()`
#' @export
tone_ratios.i <- function(n = 12) {
  rev(1 / tone_ratios(n))
}


tone_reference_ratios.i <- function(n = 12, base = 440) {
  ratios <- tone_ratios.i(n)
  c(ratios * base, base)
}


#' Return all 12 tone ratios
#' @export
twelve_tone_ratios <- function() {
  compute_12_ratio <- purrr::partial(compute_nth_tone_ratio, n = 12)
  sapply(1:12, compute_12_ratio)
}


#' Return 12-tone ratios based on a reference pitch
#' @export
twelve_tone_refernce_ratios <- function(base) {
  ratios <- twelve_tone_ratios()
  ratios * base
}


#' Return n-tone ratios based on a reference pitch
#' @export
tone_reference_ratios <- function(n, base) {
  ratios <- tone_ratios(n)
  ratios * base
}


#' Calculate the absoulte frequency based on the distance
#' to/from a given reference pitch
#' p reference pitch (usually 440)
#' d distance from reference pitch (in semitones)
#' @export
calc_12_tone_abs_freq <- function(d, p = 440) calc_n_tone_abs_freq(12, d, p)


#' Calculate the absoulte frequency based on the distance
#' to/from a given reference pitch
#' @param n base integer for n-tone system
#' @param d distance from reference pitch (in semitones)
#' @param p reference pitch (usually 440)
#' @export
calc_n_tone_abs_freq <- function(n, d, p = 440) p * (nroot(2, n))^(d)


#' Returns direct ratios of 12-tone equal temperament
#' @export
equal_temperament_intervals <- function() c(1L, twelve_tone_ratios())

