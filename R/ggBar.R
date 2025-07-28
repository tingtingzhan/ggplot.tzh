

#' @title ggBar
#' 
#' @param x \link[base]{factor} \link[base]{matrix}
#' 
#' @param na.rm \link[base]{logical} scalar
#' 
#' @param ... ..
#' 
#' @examples
#' x = sample.int(4L, size = 100L, replace = TRUE) |> 
#'   array(dim = c(20L, 5L), dimnames = list(NULL, LETTERS[1:5]))
#' attr(x, 'levels') = letters[1:4]
#' class(x) = 'factor'
#' head(x)
#' # ggBar(x, position = 'dodge', colour = 'white') # move to vignette
#' 
#' @importFrom stats na.omit
#' @export
ggBar <- function(
    x, 
    na.rm = TRUE,
    ...
) {
  
  if (!is.matrix(x) || !is.factor(x)) stop('`x` must be factor matrix')
  cnm <- dimnames(x)[[2L]]
  if (!length(cnm) || !all(nzchar(cnm))) stop('must have sensible colnames')
  nr <- dim(x)[1L]
  
  d <- data.frame(
    group = structure(rep(seq_along(cnm), each = nr), levels = cnm, class = 'factor'),
    value = c(x)
  )
  if (na.rm) d <- na.omit(d)
  
  mp <- eval(call(
    name = 'aes', 
    x = quote(value), 
    # y = quote(after_stat(density)), # not accepted in [geom_bar]
    group = quote(group), fill = quote(group)
  ))
  ggplot() +
    geom_bar(mapping = mp, ..., data = d)
  
}


