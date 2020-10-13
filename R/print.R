#' Print one-way ANOVA result
#'
#' print.oneway prints one-way ANOVA results
#'
#' @param x an object of class oneway
#' @param ... additional arguments passed to the function.
#'
#' @return the input object is returned silently
#' @export
#' @examples
#' # mileage <- oneway(mpe~cyl, mtcars)
#' print(mileage)
print.oneway <- function(x, ...){

  if(!inherits(x, "oneway")){
    stop("Must be class 'oneway")
  }

  cat("Summary statistics\n",
      "====================\n")
  print(x$summarystats, ...)
  cat("\nAnova\n",
      "====================\n")
  print(summary.lm(x$anova))
}
