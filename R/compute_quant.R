#' Compute Means, SDs, and Sample Sizes
#'
#' \code{compute_stats} is a function for computing the mean, SD, and N
#' for a numeric variable. Designed to be passed as the \code{quant_func}
#' argument to \code{survey_table}.
#'
#' @param x a numeric vector.
#' @param labels a vector of character strings containing the labels for
#' the statistics computed on \code{x}.
#'
#' @export
#' @return a numeric vector containing the computed statistics.
compute_stats <- function(x, labels = NULL){

  output <- c( n = sum(!is.na(x)), mean = mean(x), sd = sd(x))

  if(!is.null(labels)) names(output) <- labels

  output


}


#' Basic Table, Mean (SD) format.
#'
#' \code{compute_stats_meansdcombined} computes the basic summary statistics
#' (N, mean, and SD) and combines mean and SD in the format 'Mean (SD)'.
#' @param x a numeric vector.
#' @param labels a vector of character strings.
#' @param digits an integer to pass to \code{round}.
#' @export
#' @return a character vector containing the computed statistics.
compute_stats_meansdcombined <- function(x, labels = NULL, digits = 2){


 mean_val <- round(mean(x, na.rm = T), digits = 2)
 sd_val <- round(sd(x, na.rm = T), digits = 2)

 meansd_combined <- paste0(mean_val, " (", sd_val, ")")

 c(n = sum(!is.na(x)), meansd = meansd_combined)

}



#' General Quantitative Function
#'
#' \code{compute_quant} provides the most general and flexible way of
#' specifying the \code{quant_func} argument to \code{survey_table}.
#'
#' @param x a numeric vector
#' @param func_names a vector of character strings giving the names of the
#' functions to be applied to \code{x}.
#' @param func_args a list with number of elements equal to \code{length(func_names)}.
#' Each element of \code{func_args} should itself be a named list of arguments to be
#' passed to the corresponding element of \code{func_names}.
#' @param labels a vector of character strings giving the labels for the elements
#' of the output. Default is to use \code{func_names}.
compute_quant <- function(x, func_names, func_args, labels = NULL){

 n_func <- length(func_names)
 output <- rep(NA, n_func)

 for(i in seq_along(func_names)){

  output[i] <- do.call(what = func_names[i], args = func_args[[i]])


 }

  names(output) <- func_names

  output

}

