#' Plot one-way ANOVA
#'
#' plot.oneway create groups comparison for a one-way ANOVA
#'
#' @param x is an object of class oneway.
#' @param ... additional argument passed to boxplot function.
#'
#' @return NULL
#' @export
#' @examples
#' # mileage <- oneway(mpg~cyl, mtcars)
#' plot(mileage)
plot.oneway <- function(x,...){
  if(!inherits(x, "oneway")){
    stop("Must be class 'oneway")
  }
  boxplot(x$anova$terms, x$anova$model, ...)
}

