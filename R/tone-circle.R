


# Ratios?
# Build lego out of something else? (avoid maj/min/dim, etc?)
primary_sonorities <- list(

)

ToneCircle <- R6::R6Class(
  "ToneCircle",

  public = list(
   nodes = NULL,
   no_vertices = NULL,
   frequencies = NULL,
   intervals = NULL,
   ratios = NULL,
   n_tones = NULL,
   angle = NULL,
   total_angle = NULL,
   angle_type = NULL,
   rotation_angle = NULL,
   min_step_angle = NULL,
   freq_indices = NULL,
   step_size = NULL,
   step_freqs = NULL,
   step_ratio = NULL,
   base = NULL,
   pitches = NULL,
   pf = PitchAngle.F(),
   pc = PitchAngle.C(),

   scales = NULL,  # Associated scale possibilities (e.g. half-whole for 4-gon)

   # Categories for acute and obtuse angles: e.g. +90 is spiral outward,
   # -90 spiral inward... (total_angle / no_vertices > +-90 deg)

   primary_sonority = NULL,  # Strongest harmonic component

   compute_tone_angle = function() {
     180 * (self$no_vertices - 2) / self$no_vertices
   },

   set_base = function(base = 440) {
     self$base <- base
     self$compute_frequencies()
   },

   compute_frequencies = function() {
     self$frequencies <- self$base * self$ratios
   },

  initialize = function(v, base = 440, n_tones = 12, pitches = NULL) {
    self$no_vertices    <- v
    self$n_tones        <- n_tones
    self$nodes          <- vector("integer", v)
    self$pitches        <- pitches
    self$base           <- base
    self$angle          <- self$compute_tone_angle()
    self$rotation_angle <- self$angle * self$no_vertices
    self$total_angle    <- 180 * (self$n_tones - 2)
    self$min_step_angle <- 180 - self$total_angle / n_tones
    self$step_size      <- self$angle / self$min_step_angle + 1
    self$angle_type     <- self$get_angle_type()
    self$ratios         <- tone_ratios(self$n_tones)
    self$frequencies    <- self$ratios * base
    self$freq_indices   <- seq(1, self$n_tones, self$step_size)
    self$step_freqs     <- self$frequencies[self$freq_indices]
    self$step_ratio     <- self$step_freqs[2] / self$step_freqs[1]

  },

  is_angle_periodic = function() {
    self$angle %in% get_divisors(self$total_angle)
  },

  step_ratios = function() {
    x <- self$step_freqs
    y <- dplyr::lead(self$step_freqs)

    purrr::map2(x, y, function(x, y) {
      if (is.na(y)) return (0) else return(y - x)
    })
  },

  draw = function() {
    (plt <- plot_ly(
      type = "scatterpolar",
      mode = "lines"
    ) %>%
      add_trace(
        r = rep(1, length(self$nodes)),
        theta = pf[self$pitches] %>% unname(),
        name = paste(length(self$nodes), "tones"),
        fill = 'toself'
      ))
  },

  get_angle_type = function() {
    if (self$is_angle_periodic())
      # stretches beyond: by how much each time? (% percentage?)
      return("periodic")
    else
      return("non-periodic") # a circle (360 degrees)
  }

  # pitch_class_to_freq <- function(pc, n_tone = 12) {
  #   p <- symr(pc)
  # }
  #
  )
)


#' Create a new tone circle.
#'
#' @param v Vertices of the tone circle
#' @param base Base frequency for initial pitch (e.g. 440)
#' @param n_tones n-tone system.  (default 12 for 12-tone system)
#' @export
tone_circle <- function(v, base = 440, n_tones = 12) {
  ts <- ToneCircle$new(v, base, n_tones)
  ts
}



# TODO: Add functionality for wrap-around (e.g. +360 degrees)
#' Return angle value for pitch class on tone circle
#' @export
pitch_to_angle <- function(pitch, orientation = "chromatic") {

  classes <- c("A", "A#", "Bb", "B", "B#", "Cb", "C", "C#", "Db", "D", "D#",
               "Eb", "E", "E#", "Fb", "F", "F#", "Gb", "G", "G#", "Ab")

  stopifnot(pitch %in% classes, orientation %in% c("chromatic", "fifths"))

  if (orientation == "chromatic")
    switch(
      pitch,
      "A" = 0,   "A#" = 30,  "Bb" = 30,
      "B" = 60,  "Cb" = 60,  "B#" = 90,
      "C" = 90,  "C#" = 120, "Db" = 120,
      "D" = 150, "D#" = 180, "Eb" = 180,
      "E" = 210, "Fb" = 210, "E#" = 240,
      "F" = 240, "F#" = 270, "Gb" = 270,
      "G" = 300, "G#" = 330, "Ab" = 330
    )
  else
    switch(
      pitch,
      "A" = 0,   "A#" = 210, "Bb" = 210,
      "B" = 60,  "Cb" = 60,  "B#" = 270,
      "C" = 270, "C#" = 120, "Db" = 120,
      "D" = 330, "D#" = 180, "Eb" = 180,
      "E" = 30,  "Fb" = 30,  "E#" = 240,
      "F" = 240, "F#" = 90,  "Gb" = 90,
      "G" = 300, "G#" = 150, "Ab" = 150
    )
}



plotlys <- function() {
  library(plotly)
  pf <- PitchAngle.F()
  pc <- PitchAngle.C()
  # TODO: Link up pitch_to_angle() with theta = c() argument
  (dim <- plot_ly(
    type = "scatterpolar",
    mode = "lines"
  ) %>%
      add_trace(
        r = rep(1, 4),
        theta = pf[c('A', 'C', 'Eb', 'F#')] %>% unname(), # c(0, 90, 180, 270),
        name = "Diminished"
       # fill = 'toself' # 'tozeroy'
      ))

  (aug <- plot_ly(
    type = "scatterpolar",
    mode = "lines"
  ) %>%
    add_trace(
      r = rep(1, 3),
      theta = pf[c('A', 'C#', "F")] %>% unname(),
      name = "Augmented",
      fill = 'toself' # 'tozeroy'
    )) %>% layout(radialaxis = list(sector = list(0, 360),
                                    showline = FALSE,
                               showgrid = FALSE,
                               autorange = FALSE,
                               type = "category"))

  (pent <- plot_ly(
    type = "scatterpolar",
    mode = "lines"
  ) %>%
    add_trace(
      r = rep(1, 5),
      theta = pf[c('A', 'C', 'D', 'E', 'G')] %>% unname(),
      name = "Pentatonic",
      fill = 'toself' # 'tozeroy'
    ))

  (all <- plot_ly(
    type = "scatterpolar",
    mode = "lines"
  ) %>%
      add_trace(
        r = rep(1, 5),
        theta = pf[c('A', 'C', 'D', 'E', 'G')] %>% unname(),
        name = "Pentatonic",
        fill = 'toself' # 'tozeroy'
      ) %>%
      add_trace(
        r = rep(1, 3),
        theta = pf[c('A', 'C#', "F")] %>% unname(),
        name = "Augmented",
        fill = 'toself' # 'tozeroy'
      ) %>%
      add_trace(
        r = rep(1, 4),
        theta = pf[c('A', 'C', 'Eb', 'F#')] %>% unname(), # c(0, 90, 180, 270),
        name = "Diminished",
        fill = 'toself' # 'tozeroy'
      ))

}


# TODO: implement tonnetz grid and it's dual: "chicken-wire" torus
# TODO: implement function to create tone circle with overlapping sonorities.  I.e. dim,
# TODO: Infinite Cyclic graph for representation of tonnetz
#        (6 nodes, 7th note center of hexagon, or corner vertex of cube)
# <- THIS IS WHWY COMPUTER SCIENCE ALGORITHMS AND GRAPH THEORY ARE IMPORTANT!
f <- function() {


}







