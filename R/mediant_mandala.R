mediant_mandala <- function() {


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


generate_data <- function(n = 10000)
  tibble::tibble(lvl = random_lvls(n), pos = random_poss(n))


data <- generate_data()


LABELS <-
  c(pm, maj, cpm)


get_pos <- function() rep(1:12, 3)
get_lvl <- function() c(rep(-1, 12), rep(0, 12), rep(1, 12))

lbls <- table <-
  tibble(
    pos = get_pos(),
    lvl = get_lvl(),
    label = LABELS,
    type = c(rep("parallel_minor", 12), rep("major", 12), rep("counter_parallel_minor", 12))
  )


regressed_values_to_chord <- function(pos, lvl) {

  pos_logit <- table$pos == pos
  lvl_logit <- table$lvl == lvl

  pos_logit == lvl_logit

  table$label[[which(lvl_logit & pos_logit)]]
}


labs <-
  purrr::map2(data$pos, data$lvls, regressed_values_to_chord) %>%
  unlist()

labeled_data <- data %>%
  mutate(
    labels = labs
  )
labeled_data

get_symbol_preds <- function(preds) {

}

}

# TODO: Get labeled data into tensors: ((pos, lvl), label)
# TODO: Train on fake data to learn mediant mandala connections
