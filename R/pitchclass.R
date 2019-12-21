
#' Enum containing pitch angles for a chromatic tone circle (counter-clockwise)
#'
#' @details returned angles are in 30 degree increments along the polar axis
#' @export
PitchAngle.C <- function() {
  structure(
    list(
      "A" = 0,   "A#" = 210, "Bb" = 210,
      "B" = 60,  "Cb" = 60,  "B#" = 270,
      "C" = 270, "C#" = 120, "Db" = 120,
      "D" = 330, "D#" = 180, "Eb" = 180,
      "E" = 30,  "Fb" = 30,  "E#" = 240,
      "F" = 240, "F#" = 90,  "Gb" = 90,
      "G" = 300, "G#" = 150, "Ab" = 150
    ),
    class = c("PitchAngle_c", "enum", "list")
  )
}


#' Enum containing pitch angles, fourth-based tone circle (counter-clockwise)
#'
#' @details returned angles are in 30 degree increments along the polar axis
#' @export
PitchAngle.F <- function() {
  structure(
    list(
      "A" = 0,   "A#" = 30,  "Bb" = 30,
      "B" = 60,  "Cb" = 60,  "B#" = 90,
      "C" = 90,  "C#" = 120, "Db" = 120,
      "D" = 150, "D#" = 180, "Eb" = 180,
      "E" = 210, "Fb" = 210, "E#" = 240,
      "F" = 240, "F#" = 270, "Gb" = 270,
      "G" = 300, "G#" = 330, "Ab" = 330
    ),
    class = c("PitchAngle_f", "enum", "list")
  )
}



#' @export
Pitch.Symbol <- function(complete = FALSE) {
  structure(
    nlist( # nvec instead?
      if (complete) chromatic.full else chromatic),
    class = c("Pitch", "enum", "list"))
}

#' @export
pitch.symbol <- Pitch.Symbol()


#' initialize a pitch class enum with a base frequency (e.g. A == 440)
#' @export
Pitch.Hertz <- function(base = 440) {
  freqs   <- head(tone_ratios(12), -1)
  pitches <- lapply(freqs, rlang::as_function(~..1 * base))

  names(pitches) <- chromatic
  pitches[[rlang::sym("A2")]] <- base * 2

  structure(pitches, class = c("PitchHz", "enum", "list"))
}

#' @export
pitch.hertz.440 <- Pitch.Hertz()



#' @export
Pitch.Hertz.Int <- function(base = 440) {
  freqs   <- head(tone_ratios(12), -1)
  pitches <- lapply(freqs, rlang::as_function(~..1 * base))
  notes   <- as.list(chromatic)
  names(notes) <- lapply(pitches, round)

  structure(notes, class = c("PitchFreqInt", "enum", "list"))
}


#' @export
pitch.hertz.int <- Pitch.Hertz.Int()


#' Enum consisting of Just Intonation ratios
#' @export
JustIntonation <- function() {
  structure(
    list(
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
    ),
    class = c("JustIntonation", "enum", "list")
  )
}



TwelveTone <- R6::R6Class(
  "TwelveTone",
  public = list(
    pitches = NULL,
    freqs   = NULL,

    initialize = function() {
      pitches <- pitch.symbol
      freqs   <- pitch.hertz.440
    }
  )
)


#' Solfeggio intervals (Hz)
#' @export
Solfeggio <- function() {
  structure(
    list(
      do  = 174,
      ra  = 285,
      ut  = 396,
      re  = 417,
      mi  = 528,
      fa  = 639,
      sol = 741,
      la  = 852,
      ti  = 963
    ),
    class = c("Solfeggio", "enum", "list")
  )
}


# Note, changed to allow multiple returned values.
#' @export
`[.enum` <- function(x, i, upper = FALSE) {
  if (is.character(i) && upper)
    i <- toupper(i)
  class(x) <- "list"
  if (is_vec2(x))
    return(as.list(x)[i])
  names(as.list(x)[i])
}


#' @export
`[[.enum` <- function(x, i, exact = FALSE, upper = FALSE) {
  if (is.character(i) && upper)
    i <- toupper(i)
  class(x) <- "list"
  as.list(x)[[i, exact = exact]]
}


#' @export
`$.enum` <- function(x, name) {
  x[[name]]
}


