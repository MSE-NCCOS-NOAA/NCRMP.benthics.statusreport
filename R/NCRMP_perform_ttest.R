#' Two Tail t-test
#' @export
#' @description
#' performs a 2-tailed t-test between two calculated metrics
#' @param dataframe
#' A valid data.frame with, at a minimum, the following fields:
#' \itemize{
#'  \item{metric: }{specific metric calculated ("density", "old mortality", "coral cover", "macroalgae cover", "CCA cover")}
#'  \item{var: }{variance of calculated metric}
#'  \item{n: }{total number of PSUs sampled}
#'  \item{strat_num: }{number of strata sampled}
#' }
#' @param metric
#' specific metric to be tested.  Must be in dataframe parameter ("density", "old mortality", "coral cover", "macroalgae cover", "CCA cover")
#' @param alpha
#' alpha level to test metric (i.e 0.05, 0.01)
#' @param return_dataframe
#' Default is FALSE
#' \itemize{
#'  \item{if FALSE: }{returns t-test significance to console}
#'  \item{if TRUE: }{returns input dataframe with additional fields (degrees of freedom(df), t_value,
#'  lower confidence interval(LCI), upper confidence interval(UCI) )}
#' }
#'
#' @return Either the t-test output to the console (if return_dataframe = F), or a dataframe containing values needed to conduct t-test
#'
#'
#' @examples
#' Run a t-test on dataset for density at alpha = 0.05, and return test output string
#' two_tail_t_test(dataset, "density", .05, return_dataframe = F)
#'
#' Run a t-test on dataset for density at alpha = 0.01, and return the dataframe
#' two_tail_t_test(dataset, "density", .01, return_dataframe = T)
#'

two_tail_t_test <- function(dataframe, metric, alpha, return_dataframe = FALSE){

  in_range <- function(x, lower, upper){
    range = c(lower, upper)
    stopifnot(length(range) == 2L)
    range[1] < x & x < range[2]
  }


  # Function is expecting the following columns in the dataframe
  must_have <- c(metric, "Var", "n_sites", "n_strat")

  d <- dataframe

  # Check that dataframe includes must_have columns, stop with error if not
  for (i in must_have) {
    if (!i %in% colnames(d)) {
      stop(paste("Column", i, "not in dataframe"))
    }
  }

  # Add degrees of freedom (df), t-value, lower confidence interval (LCI) and upper confidence interval (UCI) to dataframe (d)
  d$df       <- d$n_sites - d$n_strat
  d$t_value  <- abs(qt(alpha/2, d$df))
  d$LCI      <- unlist(d[metric]) - (sqrt(d$Var) * d$t_value)
  d$UCI      <- unlist(d[metric]) + (sqrt(d$Var) * d$t_value)

  # Check if metric_1 is within the confidence interval of metric_2 (return True/False) and
  #       if metric_2 is within the confidence interval of metric_1 (return True/False)
  t <- c(in_range(x = d[1,][metric], lower = d[2,]$LCI, upper = d[2,]$UCI), in_range(x = d[2,][metric], lower = d[1,]$LCI, upper = d[1,]$UCI))

  return_test <- function(){
    if(any(t==T)) {
      print(paste(metric, "NOT significant at alpha = ", alpha))
    } else {
      print(paste(metric, "Significantly different p <", alpha))
    }
  }

  # Return dataframe (d) if return_datafram = T, otherwise return test output
  ifelse(return_dataframe == FALSE, return_test(), return(d))
}

