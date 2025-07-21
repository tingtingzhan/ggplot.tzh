

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
#' ggBoxJitter(data = CO2, y = uptake, x = Type)
#' #ggBoxJitter(data = CO2, y = uptake, x = Type, caption = t.test)
#' #ggBoxJitter(data = CO2, y = uptake, x = Type, caption = wilcox.test)
#' ggBoxJitter(data = CO2, y = uptake, x = Treatment, colour = Type)
#' ggBoxJitter(data = CO2, y = uptake, x = Plant, colour = Type, violin = TRUE) + 
#'   facet_grid(rows = vars(Treatment))
#' 
#' #ggBoxJitter(list(stack.loss, precip))
#' #ggBoxJitter(list(stack.loss, btmean2 = boot_mean(precip, R = 999)))
#' #ggBoxJitter(lapply(list(stack.loss, precip), boot_mean, R = 999))
#' @keywords internal
#' @importFrom rlang .data
#' @importFrom stats as.formula t.test wilcox.test
#' @importFrom scales.tzh label_pvalue_sym
#' @export
ggBoxJitter <- function(
    data, y, x, 
    colour = NULL,
    violin = FALSE, 
    dodge.width = .5, 
    data.name = deparse1(substitute(data)),
    caption,
    ...
) {
  
  y <- substitute(y)
  x <- substitute(x)
  colour <- substitute(colour)
  
  if (!is.data.frame(data) && is.list(data)) {
    
    .Defunct(msg = 'use data.frame')
    
  } else if (is.matrix(data)) {
    
    cnm <- dimnames(data)[[2L]]
    if (!length(cnm) || !all(nzchar(cnm))) stop('colnames must be complete')
    fnm <- structure(rep(seq_along(cnm), each = dim(data)[1L]), levels = cnm, class = 'factor')
    data <- data.frame(Values = c(data), Names = fnm)
    y <- quote(Values)
    x <- quote(Names)
    colour <- NULL
    
  }
  
  if (is.null(colour)) {
    mp_line <- mp_point <- aes(x = .data[[x]], y = .data[[y]])
  } else {
    mp_line <- aes(x = .data[[x]], y = .data[[y]], colour = .data[[colour]])
    mp_point <- aes(x = .data[[x]], y = .data[[y]], colour = .data[[colour]], shape = .data[[colour]])
  }

  if (!missing(caption) && is.function(caption)) {
    d <- data[c(all.vars(x), all.vars(y))] |>
      na.omit() # ?stats:::na.omit.data.frame
    fom <- call(name = '~', y, x) |>
      as.formula()
    if (identical(caption, t.test)) {
      caption_text <- sprintf(
        fmt = 'Student\'s t-test %s: %s', 
        deparse1(fom),
        t.test(formula = fom, data = d)$p.value |> 
          label_pvalue_sym(add_p = TRUE)())
    } else if (identical(caption, wilcox.test)) {
      caption_text <- sprintf(
        fmt = 'Wilcoxon-test %s: %s', 
        deparse1(fom),
        wilcox.test(formula = fom, data = d)$p.value |> 
          label_pvalue_sym(add_p = TRUE)())
    } else stop('unsupported test for caption')
  } else caption_text <- NULL
  
  ggplot() + 
    (if (violin) geom_violin(data = data, mapping = mp_line, position = position_dodge(width = dodge.width))) + 
    geom_boxplot(
      data = data, mapping = mp_line, width = .3, fill = 'white', 
      outlier.shape = NA, # jitter takes care of outliers
      position = position_dodge(width = dodge.width)
    ) +
    geom_jitter(
      data = data, mapping = mp_point, 
      #alpha = .3, # semi-transparency is not supported on `eps` device
      #color = 'red', 
      size = .2,
      position = if (length(colour)) {
        position_jitterdodge(dodge.width = dodge.width, jitter.width = .1)
      } else position_jitter(width = .03, height = 0)
    ) +
    labs(
      title = data.name,
      caption = caption_text
    )
  
}
  