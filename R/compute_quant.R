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


    output <- sapply(seq_along(digits), function(x) as.character(round(stats[x], digits = digits[x])))

  }


  output

}

#' Expand Factor Variable into Indicators
#'
#' \code{construct_indicators} takes a factor variable with k levels contained in a data.frame and
#' expands it into a k-column data.frame of indicator variables for each of the levels.
#' Primarily a helper function for \code{magic_table_categorical}.
#'
#' @param var_name a character string or vector containing the name(s) of the factor variable(s).
#' @param dataset a data.frame object containing \code{var_name}.
#'
#' @export
#'
#' @return a data.frame containing the expanded factor variable.
construct_indicators <- function(var_name, dataset){

  if(length(var_name) == 1){
    df <- as.data.frame(sapply(levels(dataset[, var_name]), function(x) dataset[, var_name] == x ))

    names(df) <- paste(var_name, levels(dataset[, var_name]), sep = '_')
  } else{

   # var_name is a vector of variable names, use lapply and recursion
   # to construct individual expanded data.frames and then cbind result
   df_list <- lapply(var_name, construct_indicators, dataset = dataset)

   df <- Reduce(cbind, df_list)


  }
  df


}


#' Count or Proportion
#'
#' \code{compute_cat} takes an indicator variable and returns either the
#' proportion or count of TRUE values.
#'
#' @param x a vector containing entries for the indicator variable (either TRUE/FALSE or 1/0).
#' @param prop a logical. If TRUE, then a proportion is returned. If FALSE, then a count
#' is returned.
#' @param na.rm a logical. If TRUE, then NA values are removed.
#' @param digits an integer specifying the number of significant digits to report.
#'
#' @export
#'
#' @return a scalar containing the count or proportion.
compute_cat <- function(x, prop = F, na.rm = T, digits = 3){

  if(prop){

    stat <- mean(x, na.rm = na.rm)
    output <- c(n = as.character(sum(!is.na(x))), missing = as.character(sum(is.na(x))), prop = as.character(round(stat, digits = digits)))
  } else{

    stat <- sum(x, na.rm = na.rm)

    output <- c(n = as.character(sum(!is.na(x))), missing = as.character(sum(is.na(x))), count = as.character(stat))
  }

  output

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

