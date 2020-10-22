#' @title One Way Analysis of Variance
#' @description
#' \code{oneway} computes a one-way analysis of variance
#' and inclues group level summary statistics
#' @details
#' This function computes a standard one-way ANOVA, means,
#' and standard deviations. Missing values are handled via
#' listwise deletion.
#' @param x forumula an object of class formula, relating the dependent
#' variable to the grouping variable
#' @param data a data frame contain the variables in the model.
#' @import dplyr
#' @export
#' @return a list with 2 elements:
#' \item{oneway}{a list with the lm result}
#' \item{summarystats}{a data frame with the summary statistics}
#' @author Cher Qin <qqin@@wesleyan.edu>
#' @examples
#' # mileage <- oneway(hwq~class, car)
#' summary(mileage)
#' mileage
#' plot(mileage)


oneway<-function(formula, data){
  #fit
  fit <- lm(formula, data)

  #summary statistics
  #stats <- aggregate(formula, data,
  #                   function(x) c(n=length(x),
  #                                 mean = mean(x),
  #                                 sd = sd(x)))
  group <- as.character(formula[[3]])
  y <- as.character(formula[[2]])
  stats <- data %>%
    group_by(.data[[group]]) %>%
    summarise(m = n(),
              mean = mean(.data[[y]]),
              sd = sd(.data[[y]]))

  # return results
  result<- list(anova = fit, summarystats = stats)
  class(result) <- "oneway"
  return(result)
}

#library(pkgdown)
#build_site()
