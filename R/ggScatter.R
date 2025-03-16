
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
#' Function [ggScatter] produces a scatter plot. 
#' 
#' @note
#' Potential name clash with `ggpubr::ggscatter`.
#' 
#' @examples 
#' library(ggplot2)
#' theme_set(theme(
#'  legend.position = 'inside',
#'  legend.position.inside = c(.8, .3)
#' ))
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
    corr <- lapply(grp, FUN = function(g) { # (g = grp[[1L]])
      ds <- split.data.frame(x = data, f = data[[g]])
      names(ds) <- paste(g, names(ds), sep = '=')
      cor_ <- lapply(ds, FUN = function(d) {
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
#' library(ggplot2); theme_set(theme_bw())
#' (p = ggScatter2(swiss, y1 = Fertility, y2 = Agriculture, x = Infant.Mortality))
#' p + theme_minimal() # not ideal!
#' 
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
  
  if (is.list(data) && !is.data.frame(data)) {
    data <- as.data.frame.list(data) # let err
  } else if (is.matrix(data)) {
    data <- as.data.frame.matrix(data) # let err
  }
  
  p <- ggplot(data = data) + 
    geom_jitter(mapping = eval(call('aes', x = x, y = y1, colour = y1_chr, shape = y1_chr)), 
                width = jitter, height = jitter, ...) + 
    geom_jitter(mapping = eval(call('aes', x = x, y = y2, colour = y2_chr, shape = y2_chr)), 
                width = jitter, height = jitter, ...) + 
    scale_y_continuous(sec.axis = sec_axis(name = y2_chr, trans = ~.)) +
    labs(colour = '', shape = '')
  
  g <- ggplot_build(p) # ggplot2:::ggplot_build.ggplot
  cols <- vapply(g$data, FUN = function(i) unique.default(i[['colour']]), FUN.VALUE = '')
  p + theme(
    axis.text.y = element_text(colour = cols[1L], face = 'bold'),
    axis.ticks.y = element_line(colour = cols[1L], linewidth = 1),
    axis.title.y = element_text(colour = cols[1L], face = 'bold'),
    axis.text.y.right = element_text(colour = cols[2L]),
    axis.ticks.y.right = element_line(colour = cols[2L]),
    axis.title.y.right = element_text(colour = cols[2L]),
    legend.position = 'none'
  )
  
}


