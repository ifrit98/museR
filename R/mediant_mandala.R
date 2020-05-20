mediant_mandala <- function() {

library(magrittr)
library(tibble)
library(dplyr)
library(tensorflow)
tf$enable_eager_execution()

maj <- c('C', 'C#', 'D', 'Eb', 'E', 'F', 'F#', 'G', 'Ab', 'A', 'Bb', 'B')
pm  <- c('a', 'bb', 'b', 'c', 'c#', 'd', 'eb', 'e', 'f', 'f#', 'g', 'ab')
cpm <- c('e', 'f', 'f#', 'g', 'ab', 'a', 'bb', 'b', 'c', 'c#', 'd', 'eb')

random_pos <-
  function(size = 1)
    sample.int(n = 12, size = as.integer(size), replace = TRUE)

random_poss <-
  function(n = 10000) random_pos(size = n)

random_lvl <-
  function(size = 1)
    sample.int(3, size = as.integer(size), replace = TRUE) - 2

random_lvls <-
  function(n = 10000) random_lvl(size = n)


generate_data <- function(n = 10000) {
  tibble::tibble(lvl = random_lvls(n), pos = random_poss(n))
}


data <- preds <- generate_data()


LABELS <-
  c(pm, maj, cpm)


get_pos <- function() rep(1:12, 3)
get_lvl <- function() c(rep(-1, 12), rep(0, 12), rep(1, 12))

lbls <- table <- TABLE <-
  tibble(
    idx = 1:36-1,
    pos = get_pos(),
    lvl = get_lvl(),
    label = LABELS,
    type = c(rep("parallel_minor", 12), rep("major", 12), rep("counter_parallel_minor", 12))
  )


regressed_value_to_chord <- function(pos, lvl) {

  pos_logit <- table$pos == pos
  lvl_logit <- table$lvl == lvl

  idx <- which(lvl_logit & pos_logit)

  table$label[[idx]]
}


regressed_vector_to_chord <- function(positions, levels) {
  purrr::map2(
    data$pos,
    data$lvl,
    regressed_value_to_chord
  ) %>% unlist()
}

df_to_chord <- function(df) {
  regressed_vector_to_chord(df$pos, df$lvl)
}


get_index <- function(pos, lvl) {
  (lvl + 2) * pos
}

get_symbol <- function(idx) {
  table$label[[idx]]
}



labeled_data <- data %>%
  mutate(
    labels = df_to_chord(.)
  )
labeled_data




in_tensor <-
  deepR::as_tensor(tf$transpose(reticulate::tuple(data$pos, data$lvl)))


library(keras)

input <- layer_input(32)

output <- input %>%
  layer_dense(32) %>%
  layer_dense(16) %>%
  layer_dense(8) %>%
  layer_dense(4) %>%
  layer_dense(8) %>%
  layer_dense(16) %>%
  layer_dense(32)

model <- keras_model(input, output)

model %>% compile(
  optimizer = 'adam',
  loss = 'categorical_crossentropy'
)


# TODO: Given melody note, predict harmony underneath.  Train on midi?
# TODO: Midi format -> piano roll -> data.frame -> (x = melody_note, y = chord)



}

# TODO: Get labeled data into tensors: ((pos, lvl), label)
# TODO: Train on fake data to learn mediant mandala connections

