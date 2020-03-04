
function() {

  library(keras)
  library(tensorflow)

  input <- layer_input(shape = list(NULL, 2))


  features <- input %>%
    # layer_expand_dims() %>%
    layer_separable_conv_1d(16, 3) %>%
    layer_separable_conv_1d(32, 5) %>%
    layer_global_max_pooling_1d()


  base <- features %>% layer_dense(12, activation = 'softmax')
  ord  <- features %>% layer_dense(3, activation = 'softmax')

  base <- features %>% layer_dense(1, activation = 'sigmoid') # regressed values (0, 1)
  ord  <- features %>% layer_dense(1, activation = 'tanh') # (-1, 1)


  model <- keras_model(input, list(base, ord))

  model %>% compile(
    'adam',
    'mse',
    metrics = c('acc')
  )

  cat(model$count_params(), " parameters\n")

}
