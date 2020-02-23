

#' @importFrom zeallot %<-%
#' @importFrom magrittr %<>%
#' @export
spectrogram <- function(sig, main) {
  pdf.options(width = 17, height = 11)
  py_spectrogram <- reticulate::import("scipy.signal", delay_load = TRUE)$spectrogram
  c(f, t, Sxx) %<-% py_spectrogram(sig, nperseg = as.integer(1024 / 4),
                                   hoverlap = 0L, return_onesided = FALSE,
                                   scaling = 'density')
  Sxx %<>% t()
  Sxx %<>% .[, order(f)]
  image(
    z = Sxx,
    x = seq(from = 1, to = 2000, length,out = nrow(Sxx)),
    y = sort(f),
    ylim = range(f),
    xlab = "Sig Window #",
    userRaster = TRUE,
    col = viridisLite::viridis(30),
    main = main
  )
}

