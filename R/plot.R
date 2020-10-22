#' Plot one-way ANOVA
#'
#' plot.oneway create groups comparison for a one-way ANOVA
#'
#' @param x is an object of class oneway.
#' @param ... additional argument passed to boxplot function.
#' @import ggplot2
#' @return NULL
#' @export
#' @examples
#' # mileage <- oneway(mpg~cyl, mtcars)
#' plot(mileage)
plot.oneway <- function(x,...){
  if(!inherits(x, "oneway")){
    stop("Must be class 'oneway")
  }
  g <- as.character(x$anova$terms[[3]])
  y <- as.character(x$anova$terms[[2]])
  ggplot(x$anova$model, aes(x = factor(.data[[g]]), y = .data[[y]],
                            fill = factor(.data[[g]])))+
    geom_boxplot(...)+
    labs(x=g)+
    theme(legend.position = "none")
}

