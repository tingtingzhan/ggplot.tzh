
#' @title ggQQ
#' 
#' @description 
#' ..
#' 
#' @param object ..
#' 
#' @param fun ..
#' 
#' @param ... ..
#' 
#' @details
#' Function [ggQQ] improves \link[stats]{qqnorm} and \link[stats]{qqplot}
#' 
#' @references  
#' \url{http://stackoverflow.com/questions/4357031/qqnorm-and-qqline-in-ggplot2}
#' 
#' @examples 
#' ggQQ(precip, mean = mean.default(precip), sd = sd(precip))
#' 
#' @importFrom stats complete.cases ppoints qnorm rstandard rstudent 
#' @export
ggQQ <- function(object, fun = qnorm, ...) {
  
  obj <- if (inherits(object, what = 'lm')) cbind(
      Resid = rstudent(object), # ?stats:::rstudent.lm
      'Std Resid' = rstandard(object) # ?stats:::rstandard.lm
  ) else if (is.vector(object, mode = 'numeric')) {
    as.matrix.default(object)
  } else object
  
  if (!is.numeric(obj) || !is.matrix(obj)) stop('must be convertible to numeric matrix')
  
  obj <- obj[complete.cases(obj), , drop = FALSE]
  obj <- obj[order(obj[,1L]), , drop = FALSE] # sort by 1st column
  
  dm <- dim(obj)
  if (dm[2L] > 1L) {
    cnm <- dimnames(obj)[[2L]]
    if (!length(cnm) || all(cnm == seq_along(cnm))) stop('matrix must have non-sequential colnames')
  }
  
  qq <- do.call(fun, args = list(ppoints(dm[1L]), ...))
  x <- rep(qq, times = dm[2L])

  ggplot() + 
    (if (dm[2L] > 1L) {
      geom_point(mapping = aes(x = x, y = c(obj), colour = rep(cnm, each = dm[1L])))
    } else geom_point(mapping = aes(x = x, y = c(obj)))) + 
    labs(x = paste('Quantiles:', deparse1(substitute(fun))),
         y = 'Quantiles: Sample(s)', colour = 'Index') + 
    geom_abline(intercept = 0, slope = 1, linetype = 2L, colour = 'grey')
  
}






