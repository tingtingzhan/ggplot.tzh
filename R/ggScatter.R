
#' @title Scatter Plots using \CRANpkg{ggplot2}
#' 
#' @description ..
#' 
#' @param data \link[base]{data.frame} or \link[base]{list}
#' 
#' @param x,y,y1,y2 \link[base]{expression}s, currently only \link[base]{name}s are tested
#' 
#' @param colour,shape (optional) \link[base]{expression}s, currently only \link[base]{name}s are tested
#' 
#' @param jitter,... additional parameters of \link[ggplot2]{geom_jitter}
#' 
#' @param data.name ..
#' 
#' @details 
#' 
#' Function [ggScatter()] produces a scatter plot. 
#' 
#' @note
#' Potential name clash with `ggpubr::ggscatter`.
#' 
#' @examples 
#' ggScatter(iris, x = Sepal.Length, y = Petal.Length)
#' ggScatter(iris, x = Sepal.Length, y = Petal.Length, colour = Species)
#' library(plotly)
#' plot_ly(iris, x = ~Sepal.Length, y = ~Petal.Length, type = 'scatter', mode = 'markers')
#' plot_ly(iris, x = ~Sepal.Length, y = ~Petal.Length, color = ~Species, 
#'   type = 'scatter', mode = 'markers')
#' 
#' car = mtcars |> within(expr = {
#'  cyl = factor(cyl)
#'  vs = as.logical(vs)
#' })
#' ggScatter(car, x = mpg, y = drat, colour = cyl, shape = vs) + facet_grid(cols = vars(gear))
#' # not sure how to facet plotly
#' @name ggScatter
#' @importFrom dplyr vars
#' @importFrom stats cor.test
#' @export
ggScatter <- function(
    data, 
    y, x, colour = NULL, shape = NULL, 
    jitter = 0, 
    data.name = deparse1(substitute(data)),
    ...
) {
  
  y <- substitute(y)
  x <- substitute(x)
  colour <- substitute(colour)
  if (length(colour) && !is.symbol(colour)) stop('`colour` must be symbol')
  shape <- substitute(shape)
  if (length(shape) && !is.symbol(shape)) stop('`shape` must be symbol')
  
  if (is.list(data) && !is.data.frame(data)) {
    data <- as.data.frame.list(data) # let err
  } else if (is.matrix(data)) {
    data <- as.data.frame.matrix(data) # let err
  }
  
  ylab <- if (inherits(yval <- data[[y]], what = 'difftime')) {
    data[[y]] <- unclass(yval)
    paste0(y, ' in ', units(yval))
  } else y
  
  xlab <- if (inherits(xval <- data[[x]], what = 'difftime')) {
    data[[x]] <- unclass(xval)
    paste0(x, ' in ', units(xval))
  } else x
  
  # correlation coefficient
  grp <- unique(c(as.character(colour), as.character(shape)))
  if (length(grp)) {
    names(grp) <- grp
    corr <- lapply(grp, FUN = \(g) { # (g = grp[[1L]])
      ds <- split.data.frame(x = data, f = data[[g]])
      names(ds) <- paste(g, names(ds), sep = '=')
      cor_ <- lapply(ds, FUN = \(d) {
        cor_test_sum(cor.test(d[[y]], d[[x]]))
      })
      do.call(rbind, args = cor_)
    })
  } else {
    cor_ <- cor_test_sum(cor.test(data[[y]], data[[x]]))
    corr <- array(cor_, dim = c(1L, 1L), dimnames = list(data.name, names(cor_)))
  }
  
  p <- ggplot(data = data) + 
    geom_jitter(mapping = eval(call('aes', x = x, y = y, colour = colour, shape = shape)), 
                width = jitter, height = jitter, ...) + 
    labs(
      x = xlab, y = ylab, 
      colour = colour, shape = shape,
      title = data.name
    )
  attr(p, which = 'corr') <- corr
  return(p)
  
}



cor_test_sum <- function(x, ...) {
  # `x` is return of ?stats::cor.test
  #sprintf(fmt = '%s=%.2f, p=%.3f', x$method, x$estimate, x$p.value)
  ret <- sprintf(fmt = '%.2f, p=%.3f', x$estimate, x$p.value)
  names(ret) <- x$method
  return(ret)
}



#' @rdname ggScatter
#' 
#' @details 
#' 
#' Function [ggScatter2] produces a scatter plot with two vertical axes.
#' 
#' @examples
#' ggScatter2(swiss, y1 = Fertility, y2 = Agriculture, x = Infant.Mortality)
#' 
#' @importFrom scales pal_hue
#' @importFrom rlang .data
#' @export
ggScatter2 <- function(
    data, 
    y1, y2, x, 
    jitter = 0, 
    ...
) {
  
  y1 <- substitute(y1)
  y1_chr <- deparse1(y1)
  y2 <- substitute(y2)
  y2_chr <- deparse1(y2)
  x <- substitute(x)
  
  col <- pal_hue()(2L)
  
  if (is.list(data) && !is.data.frame(data)) {
    data <- as.data.frame.list(data) # let err
  } else if (is.matrix(data)) {
    data <- as.data.frame.matrix(data) # let err
  }
  
  ggplot(data = data) + 
    
    geom_jitter(
      data = data, 
      mapping = aes(x = .data[[x]], y = .data[[y1]]), 
      colour = col[1L], shape = 1L, 
      width = jitter, height = jitter, show.legend = FALSE, ...) + 
    
    geom_jitter(
      data = data,
      mapping = aes(x = .data[[x]], y = .data[[y2]]), 
      colour = col[2L], shape = 2L, 
      width = jitter, height = jitter, show.legend = FALSE, ...) + 
    
    scale_y_continuous(sec.axis = sec_axis(name = y2_chr, trans = ~.)) + 
    
    theme(
      axis.text.y = element_text(colour = col[1L]),
      axis.ticks.y = element_line(colour = col[1L]),
      axis.title.y = element_text(colour = col[1L]),
      axis.text.y.right = element_text(colour = col[2L]),
      axis.ticks.y.right = element_line(colour = col[2L]),
      axis.title.y.right = element_text(colour = col[2L])
    )
  
}


