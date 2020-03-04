function() {

library(readr)


data <- read_csv('data/data.csv', col_names = c('theta', 'x'))

# scale to (0, 1)
base <- data$theta / 360

ds <- list(base, data$x) %>%
  as_tensor() %>%
  tf$transpose()

}
