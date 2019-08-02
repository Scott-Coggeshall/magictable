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
compute_quant <- function(x, digits = 3, labels = NULL, ...){


  stats <- c(n = sum(!is.na(x)), n_missing = sum(is.na(x)), mean = mean(x, ...), sd = sd(x, ...), min = min(x, ...), max = max(x, ...))

  if(!is.null(labels)) names(stats) <- labels

  if(length(digits) == 1){

    output <- round(stats, digits = digits)

  } else{


    output <- sapply(digits, function(x) round(stats, digits = x))

  }


  output

}


compute_cat <- function(x, prop = F, na.rm = T, digits = 3){

  if(prop){

    output <- mean(x, na.rm = na.rm)
  } else{

    output <- sum(x, na.rm = na.rm)
  }

  round(output, digits)

}

#' Basic Table, Mean (SD) format.
#'
#' \code{compute_meansd} computes N, Mean and SD.
#' @param x a numeric vector.
#' @param combine a logical value. If \code{TRUE}, then the mean and SD are
#' combined as 'Mean (SD)'. If \code{FALSE}, then the mean and SD are reported
#' separately.
#' @param digits an integer to pass to \code{round}.
#' @param ... additional arguments to pass to \code{mean} and \code{sd}.
#' @export
#' @return a character vector containing the computed statistics.
compute_meansd <- function(x, combine = T, digits = 2){


 mean_val <- round(mean(x, na.rm = T), digits = digits)
 sd_val <- round(sd(x, na.rm = T), digits = digits)
 if(combine == T){

   meansd_combined <- paste0(mean_val, " (", sd_val, ")")

  c(n = sum(!is.na(x)), meansd = meansd_combined)
 } else{


   c(n = sum(!is.na(x)), mean = mean_val, sd = sd_val)

 }
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
#' passed to the corresponding element of \code{func_names}. If the list consists of
#' non-list elements, then it is assumed that this single list contains arguments
#' that should be applied to all functions contained in \code{func_names}.
#' @param labels a vector of character strings giving the labels for the elements
#' of the output. Default is to use \code{func_names}.
compute_stats <- function(x, stat_func, stat_args, labels = NULL){

 n_func <- length(func_names)
 output <- rep(NA, n_func)
 if(!all(sapply(test_list, is.list))) func_args <- c(lapply(seq_along(func_names), function(x) func_args))
 for(i in seq_along(func_names)){

  output[i] <- do.call(what = func_names[i], args = func_args[[i]])


 }

  names(output) <- func_names

  output

}

