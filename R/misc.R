
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
