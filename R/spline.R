

#' @title Creates \link[ggplot2]{ggplot} of \link[stats]{spline} Object
#' 
#' @description ..
#' 
#' @param object a \link[stats]{spline} object
#' 
#' @param ... ..
#' 
#' @returns 
#' Function [autoplot.spline] returns a \link[ggplot2]{ggplot} figure.
#' 
#' @note 
#' Function [autoplot.spline] is not substantially different from `splines:::plot.spline`.
#' 
#' @examples 
#' library(splines)
#' class(x1 <- interpSpline(obj1 = weight ~ height, obj2 = women, bSpline = TRUE))
#' plot(x1)
#' autoplot(x1)
#' 
#' class(x2 <- polySpline(x1))
#' plot(x2)
#' autoplot(x2)
#' @export autoplot.spline
#' @export
autoplot.spline <- function(object, ...) {
  # see ?splines:::plot.spline
  fom <- attr(object, which = 'formula', exact = TRUE) # can we have more than one `x` ?
  ggplot() + 
    autolayer.spline(object, ...) +
    labs(x = deparse1(fom[[3L]]), y = deparse1(fom[[2L]]))
}

#' @importFrom stats predict
#' @export
autolayer.spline <- function(object, ...) {
  # see ?splines:::plot.spline
  xy <- predict(object, ...) # 'xyVector' from ?splines:::predict.*
  geom_path(mapping = aes(x = xy$x, y = xy$y))
}






