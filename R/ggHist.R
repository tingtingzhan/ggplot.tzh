

#' @title ggHist
#' 
#' @description ..
#' 
#' @param object ..
#' 
#' @param rel \link[base]{logical} scalar, whether a histogram with relative frequencies should be 
#' produced (default `FALSE`)
#' 
#' @param by_integer ..
#' 
#' @param ... ..
#' 
#' @details 
#' Function [ggHist] is the \CRANpkg{ggplot2} version of \link[graphics]{hist}
#' 
#' @examples 
#' ggHist(stack.loss)
#' ggHist(stack.loss, rel = TRUE)
#' 
#' # 'Date' object
#' 
#' EXP_Date1 = structure(c(15863, 15887, NA, 15873, 15885, 15878), class = 'Date')
#' EXP_Date2 = structure(c(15965, 15917, 15923, 15955, 15918, NA), class = 'Date')
#' stopifnot(is.double(EXP_Date1), !is.numeric(EXP_Date1))
#' print.default(EXP_Date1)
#' ggHist(EXP_Date1)
#' # ggBoxJitter(list(EXP_Date1, EXP_Date2))
#' 
#' # 'POSIXt' object
#' 
#' EXP_ct1 = c(Sys.time() + 3600 * rnorm(20))
#' typeof(EXP_ct1)
#' stopifnot(is.double(EXP_ct1), !is.numeric(EXP_ct1))
#' EXP_ct2 = c(Sys.time() + 3600 * rnorm(20, mean = 5))
#' EXP_lt1 = as.POSIXlt.POSIXct(EXP_ct1)
#' EXP_lt2 = as.POSIXlt.POSIXct(EXP_ct2)
#' stopifnot(!is.double(EXP_lt2), !is.numeric(EXP_lt2))
#' typeof(EXP_lt1) # list
#' print.default(EXP_lt2) # wow!
#' ggHist(EXP_ct1)
#' ggHist(EXP_lt2)
#' #ggBoxJitter(list(EXP_ct1, EXP_ct2))
#' #ggBoxJitter(list(as.POSIXlt(EXP_ct1), EXP_lt2))
#' #ggBoxJitter(list(EXP_ct1, c(Sys.time() + 3600 * rnorm(20, mean = 5))))
#' 
#' # 'difftime' object
#' 
#' EXP_dft = EXP_Date2 - EXP_Date1
#' stopifnot(is.double(EXP_dft), !is.numeric(EXP_dft))
#' ggHist(EXP_dft)
#' @keywords internal
#' @importFrom scales label_percent
#' @importFrom rlang .data
#' @importFrom zoo scale_x_yearmon scale_x_yearqtr scale_y_yearmon scale_y_yearqtr
#' @export
ggHist <- function(object, rel = FALSE, by_integer = FALSE, ...) {
  
  if (!length(obj <- object[!is.na(object)])) return(invisible())
  
  supported_cls <- c('POSIXt', 'Date', 'difftime', 'yearmon', 'yearqtr')
  cls <- supported_cls[as.logical(inherits(object, what = supported_cls, which = TRUE))]
  if (!length(cls)) {
    if (!any(typeof(object) == c('double', 'integer'))) stop('input type must be double or integer')
    cls <- 'plain'
  }
  if (length(cls) != 1L) stop('should not happen')
  
  aes_x <- switch(cls, POSIXt = {
    quote(as.POSIXct(obj)) # ?base::as.POSIXct.POSIXlt ?base::as.POSIXct.default
  }, difftime = {
    quote(unclass(obj))
  }, quote(obj))
  
  mp <- if (rel) eval(call('aes', x = aes_x, y = quote(after_stat(density)))) else eval(call('aes', x = aes_x))
  #mp <- if (rel) aes(x = .data[[aes_x]], y = after_stat(density)) else aes(x = .data[[aes_x]])
  # um..
  
  ggplot() + 
    geom_histogram(mapping = mp, bins = if (by_integer) {
      ceiling(max(obj)) - floor(min(obj)) + 1L
    } else 30L, colour = 'white', alpha = .1) + 
    (if (rel) scale_y_continuous(labels = label_percent())) + 
    switch(cls, yearmon = scale_x_yearmon(), yearqtr = scale_x_yearqtr(format = '%Y Q%q')) +
    labs(x = paste0(deparse1(substitute(object)), switch(cls, difftime = {
      paste0(' in ', units(object))
    })), y = NULL)
}

