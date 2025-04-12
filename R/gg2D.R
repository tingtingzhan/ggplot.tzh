


#' @title ggContour
#' 
#' @description ..
#' 
#' @param data ..
#' 
#' @param x,y,z ..
#' 
#' @param ... ..
#' 
#' @details 
#' 
#' Function [ggContour] improves \link[graphics]{contour}
#' 
#' @examples 
#' ?volcano
#' graphics::filled.contour(volcano)
#' ggContour(volcano)
#' 
#' @importFrom metR geom_text_contour
#' @importFrom reshape2 melt
#' @importFrom rlang .data
#' @export
ggContour <- function(data, z, x, y, ...) {
  
  x <- substitute(x) # fine with missing argument :)
  y <- substitute(y)
  z <- substitute(z)
  
  if (is.matrix(data)) {
    data <- melt(data) # reshape2:::melt.matrix
    nm <- names(data)
    x <- as.symbol(nm[1L])
    y <- as.symbol(nm[2L])
    z <- as.symbol(nm[3L])
  }
  
  if (!is.data.frame(data)) stop('input must be convertible to data.frame')
  
  mp <- aes(x = .data[[x]], y = .data[[y]], z = .data[[z]], 
            colour = eval(after_stat(level))) # devtools::check *still* warns on unknown 'level'

  ggplot() + 
    geom_contour(data = data, mapping = mp, show.legend = FALSE) + 
    # difference between ?ggplot2::geom_contour vs. ?ggplot2::stat_contour ?
    geom_text_contour(data = data, mapping = mp, show.legend = FALSE) +
    coord_equal()
}






#' @title ggHeatmap
#' 
#' @description ..
#' 
#' @param data ..
#' 
#' @param x,y ..
#' 
#' @param fill ..
#' 
#' @param ... ..
#' 
#' @importFrom reshape2 melt
#' 
#' @examples 
#' ggHeatmap(VADeaths)
#' ggHeatmap(log1p(occupationalStatus))
#' 
#' @export
ggHeatmap <- function(data, fill, x, y, ...) {
  x <- substitute(x) # fine with missing argument :)
  y <- substitute(y)
  fill <- substitute(fill)
  
  if (is.matrix(data)) {
    data <- melt(data) # ?reshape2:::melt.matrix
    nm <- names(data)
    x <- as.symbol(nm[1L])
    y <- as.symbol(nm[2L])
    fill <- as.symbol(nm[3L])
  }
  
  if (!is.data.frame(data)) stop('input must be convertible to data.frame')
  
  ggplot() + 
    geom_tile(data = data, mapping = eval(call('aes', x = x, y = y, fill = fill)))
  
}

