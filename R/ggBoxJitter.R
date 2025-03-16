

#' @title ggBoxJitter
#' 
#' @description ..
#' 
#' @param data ..
#' 
#' @param x,y,colour ..
#' 
#' @param violin ..
#' 
#' @param dodge.width ..
#' 
#' @param data.name ..
#' 
#' @param ... ..
#' 
#' @details 
#' 
#' Stop doing \link[ggplot2]{geom_violin}, need to educate people what they are.
#' 
#' @examples 
#' library(ggplot2)
#' ggBoxJitter(data = CO2, y = uptake, x = Type)
#' ggBoxJitter(data = CO2, y = uptake, x = Treatment, colour = Type)
#' ggBoxJitter(data = CO2, y = uptake, x = Plant, colour = Type, violin = TRUE) + 
#'   facet_grid(rows = vars(Treatment))
#' 
#' #ggBoxJitter(list(stack.loss, precip))
#' #ggBoxJitter(list(stack.loss, btmean2 = boot_mean(precip, R = 999)))
#' #ggBoxJitter(lapply(list(stack.loss, precip), boot_mean, R = 999))
#' 
#' @export
ggBoxJitter <- function(
    data, y, x, 
    colour = NULL,
    violin = FALSE, 
    dodge.width = .5, 
    data.name = deparse1(substitute(data)),
    ...
) {
  
  y <- substitute(y)
  x <- substitute(x)
  colour <- substitute(colour)
  
  if (!is.data.frame(data) && is.list(data)) {
    
    .Defunct(msg = 'function arg2nm() moved to package tzhInternal')
    #cl <- match.call()
    #cls <- is_equal(data, FUN = class) # list element must be of same class
    #data_call <- cl[[2L]]
    #if (!is.symbol(data_call) && data_call[[1L]] == 'lapply') data_call <- data_call[[2L]]
    #data_call <- arg2nm(data_call)
    
    #if (all(cls == c('POSIXlt', 'POSIXt'))) data <- lapply(data, FUN = as.POSIXct.POSIXlt)
    #if (!all(vapply(data, FUN = typeof, FUN.VALUE = '') %in% c('numeric', 'double'))) stop('all elements must be numeric-like')
    #data <- data.frame(Values = do.call(c, args = data), Names = rep(names(data_call)[-1L], times = lengths(data, use.names = FALSE)))
    #y <- quote(Values)
    #x <- quote(Names)
    #colour <- NULL
    
  } else if (is.matrix(data)) {
    
    cnm <- dimnames(data)[[2L]]
    if (!length(cnm) || !all(nzchar(cnm))) stop('colnames must be complete')
    fnm <- structure(rep(seq_along(cnm), each = dim(data)[1L]), levels = cnm, class = 'factor')
    data <- data.frame(Values = c(data), Names = fnm)
    y <- quote(Values)
    x <- quote(Names)
    colour <- NULL
    
  }
  
  mp_line <- eval(call('aes', x = x, y = y, colour = colour))
  mp_point <- eval(call('aes', x = x, y = y, colour = colour, shape = colour))

  ggplot() + 
    (if (violin) geom_violin(data = data, mapping = mp_line, position = position_dodge(width = dodge.width))) + 
    geom_boxplot(data = data, mapping = mp_line, width = .3, fill = 'white', 
                 outlier.shape = NA, # jitter takes care of outliers
                 position = position_dodge(width = dodge.width)) +
    geom_jitter(data = data, mapping = mp_point, 
                #alpha = .3, # semi-transparency is not supported on `eps` device
                #color = 'red', 
                size = .2,
                position = if (length(colour)) {
                  position_jitterdodge(dodge.width = dodge.width, jitter.width = .1)
                } else position_jitter(width = .03, height = 0)) +
    labs(title = data.name)
  
}
  