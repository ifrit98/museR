
library(keras)
library(tensorflow)

input <- layer_input(list(8192))

input_complex <- tf$cast(input, tf$complex64)

signal <- input_complex %>%
  deepR::layer_transform_signal_for_dl()

features <- signal %>%
  layer_conv_1d(32, 3, activation = 'relu') %>%
  layer_conv_1d(64, 3, activation = 'relu') %>%
  layer_max_pooling_1d() %>%
  layer_conv_1d(128, 5, activation = 'relu') %>%
  layer_conv_1d(128, 5, activation = 'relu') %>%
  layer_global_max_pooling_1d()


note <- features %>%
  layer_dense(64, activation = 'relu') %>%
  layer_dense(12, activation = 'softmax', name = 'note')

mandala_lvl <- features %>%
  layer_dense(64, activation = 'relu') %>%
  layer_dense(1, activation = 'tanh', name = 'level')


model <-
  keras_model(input, list(note, mandala_lvl))

model %>%
  compile(loss = 'categorical_crossentropy', optimizer = 'adam', list('accuracy'))

model
