#' Magic Table
#'
#' \code{magic_table} is the top-level function for \code{magic_table_numeric} and
#' \code{magic_table_categorical}.
#'
#' @param dataset a data.frame object, or object that can be coerced to a data.frame.
#' @param vars a vector of character strings containing the names of the variables
#' for which summary statistics should be computed.
#' @param var_labels a vector of character strings containing the labels for the variables
#' in \code{vars}.
#' @param cat_labels a vector of character strings containing the labels for the categories
#' of the variables contained in \code{vars}. If NULL (default), then \code{cat_labels} is
#' constructed from the \code{levels} of \code{vars}.
#' @param stat_func a function for calculating the summary statistics for the variables specified in \code{vars}.
#' @param stat_labels a vector of character strings containing the labels for the values
#' in the output from \code{stat_func}.
#' @param condvar_wide a character string indicating a variable in \code{dataset} for wide conditioning,
#' i.e. within-group statistics are added as extra columns.
#' @param condvar_wide_labels a character vector containing the labels for the different values
#' of \code{condvar_wide}.
#' @param condvar_long a character string indicating a variable in \code{dataset} for long conditioning,
#' i.e. within-group statistics are added as extra rows.
#' @param condvar_long_labels a character vector containing the labels for the different values
#' of \code{condvar_long}.
#' @param categorical a logical value indicating whether \code{magic_table_categorical} or
#' \code{magic_table_numeric} should be called. If \code{NULL} (default), then \code{magic_table}
#' wil automatically choose one based on the classes of \code{vars}.
#' @param ... additional variables to be passed to \code{stat_func}.
#'
#' @details The main purpose of the \code{magic_table} function is to serve as a top-level function
#' for calling either \code{magic_table_numeric} or \code{magic_table_categorical}. For convenience,
#' \code{magic_table} will attempt to figure out which one should be called based on the classes
#' of the variables contained in \code{vars}.
#' @export
#'
#' @return a \code{magic_table} object, the result of a call to either \code{magic_table_numeric}
#' or \code{magic_table_categorical}.
magic_table <- function(dataset, vars, var_labels, cat_labels, stat_func, stat_labels, condvar_wide,
                        condvar_wide_labels, condvar_long, condvar_long_labels, categorical = NULL, ...){

  var_classes <- sapply(dataset[, vars], class)
  # convert character strings to factors
  dataset[, vars[var_classes == "character"]] <- lapply(dataset[, vars[var_classes == "character"]], factor)

  var_classes <- sapply(dataset[, vars], class)
  # check if there are a mix of factor and non-factor variables
  if(any(var_classes != "factor") & any(var_classes == "factor")) stop("Error: 'vars' contains a mix of categorical and non-categorical variables.")

  if(is.null(categorical)) categorical <- any(var_classes == "factor")

  if(categorical){

    magic_table_categorical(dataset, vars, var_labels, cat_labels, stat_func, condvar_wide,
                            condvar_wide_labels, condvar_long, condvar_long_labels, ...)

  } else{

   magic_table_numeric(dataset, vars, var_labels, stat_func, stat_labels, condvar_wide, condvar_wide_labels,
                       condvar_long, condvar_long_labels, ...)

  }




}
#' Magic Table
#'
#' \code{magic_table_numeric} creates a magic table object from a data.frame-type object when
#' the input variables are a set of numeric variables.
#'
#' @param dataset a data.frame object, or object that can be coerced to a data.frame.
#' @param vars a vector of character strings containing the names of the variables
#' for which summary statistics should be computed.
#' @param var_labels a vector of character strings containing the labels for the variables
#' contained in \code{vars}.
#' @param stat_func a function for calculating the summary statistics for the variables specified in \code{vars}.
#' @param condvar_wide a character string indicating a variable in \code{dataset} for wide conditioning,
#' i.e. within-group statistics are added as extra columns.
#' @param condvar_wide_labels a character vector containing the labels for the different values
#' of \code{condvar_wide}.
#' @param condvar_long a character string indicating a variable in \code{dataset} for long conditioning,
#' i.e. within-group statistics are added as extra rows.
#' @param condvar_long_labels a character vector containing the labels for the different values
#' of \code{condvar_long}.
#' @param categorical a logical value indicating whether \code{magic_table_numeric} or
#' \code{magic_table_categorical} should be called. If \code{NULL} (default), then
#' \code{magic_table} will select one based on the class of the variables contained in \code{vars}.
#' @param ... additional variables to be passed to \code{stat_func}.
#'
#'
#'
#' @export
#'
#' @return a list containing tables of summary statistics for the specified variables.
magic_table_numeric <- function(dataset, vars, var_labels = NULL, stat_func, stat_labels, condvar_wide = NULL, condvar_wide_labels = NULL,
                        condvar_long = NULL, condvar_long_labels = NULL, ...){
  output <- list()

  if(is.null(condvar_wide) & is.null(condvar_long)){

   output$table <- as.data.frame(t(sapply(dataset[, vars], stat_func, ...)))

  } else if(is.null(condvar_long) & !is.null(condvar_wide)){

    output$table <- lapply(unique(dataset[,condvar_wide]), function(x) as.data.frame(t(sapply(dataset[dataset[,condvar_wide] == x, vars], stat_func, ...))))

  } else if(!is.null(condvar_long) & is.null(condvar_wide)){

    temp_list <- lapply(unique(dataset[,condvar_long]), function(x){

      dat <- as.data.frame(t(sapply(dataset[dataset[,condvar_long] == x, vars], stat_func, ...)))

      dat$var_names <- vars
      dat <- dat[, c(ncol(dat), 1:(ncol(dat) - 1))]

      dat

      })

    long_df <- Reduce(rbind, temp_list)

    long_df <- long_df[order(long_df$var_names), ]

    output$table <- long_df


  } else {

    output$table <- lapply(unique(dataset[,condvar_wide]), function(x){

      temp_list <- lapply(unique(dataset[dataset[,condvar_wide] == x,condvar_long]), function(y){

        dat <- as.data.frame(t(sapply(dataset[dataset[,condvar_wide] == x & dataset[,condvar_long] == y, vars], stat_func,...)))

        dat$group <- y
        dat$var_name <- vars

        dat <- dat[, c("var_name", "group", names(dat)[-grep("var_name|group",names(dat))])]
        dat
      })

      temp_df <- Reduce(rbind, temp_list)

      temp_df <- temp_df[order(temp_df$var_name, temp_df$group), ]

      temp_df[, -(1:2)]


    })


  }

  class(output) <- c("magictable")

  if(is.null(var_labels)) var_labels <- vars
  if(is.null(condvar_long_labels) & !is.null(condvar_long)) condvar_long_labels <- unique(dataset[,condvar_long])
  if(is.null(condvar_wide_labels) & !is.null(condvar_wide)) condvar_wide_labels <- unique(dataset[,condvar_wide])
  if(is.null(stat_labels)){

    if(is.null(condvar_wide)){

      n_stats <- ncol(output$table[[1]])

    } else{

      n_stats <- ncol(output$table)

    }

    stat_labels <- paste0("x", 1:n_stats)

  }

  output$var_labels <- var_labels
  output$stat_labels <- stat_labels
  output$condvar_wide_labels <- condvar_wide_labels
  output$condvar_long_labels <- condvar_long_labels
  output$type <- "numeric"



  output
}


#' Magic Table
#'
#' \code{magic_table_categorical} creates a magic table object from a data.frame-type object when
#' the input variables are a set of categorical variables.
#'
#' @param dataset a data.frame object, or object that can be coerced to a data.frame.
#' @param vars a vector of character strings containing the names of the variables
#' for which summary statistics should be computed.
#' @param var_labels a vector of character strings containing the labels for the variables
#' in \code{vars}.
#' @param cat_labels a vector of character strings containing the labels for the categories
#' of the variables contained in \code{vars}. If NULL (default), then \code{cat_labels} is
#' constructed from the \code{levels} of \code{vars}.
#' @param stat_func a function for calculating the summary statistics for the variables specified in \code{vars}.
#' @param condvar_wide a character string indicating a variable in \code{dataset} for wide conditioning,
#' i.e. within-group statistics are added as extra columns.
#' @param condvar_wide_labels a character vector containing the labels for the different values
#' of \code{condvar_wide}.
#' @param condvar_long a character string indicating a variable in \code{dataset} for long conditioning,
#' i.e. within-group statistics are added as extra rows.
#' @param condvar_long_labels a character vector containing the labels for the different values
#' of \code{condvar_long}.
#'
#' @export
#'
#' @return a list containing tables of summary statistics for the specified variables.
magic_table_categorical <- function(dataset, vars, var_labels = NULL, cat_labels = NULL, stat_func, stat_labels, condvar_wide = NULL, condvar_wide_labels = NULL,
                                condvar_long = NULL, condvar_long_labels = NULL, ...){
  output <- list()

  # create dataset with factor variables expanded into indicator variables

  dataset_expanded <- construct_indicators(vars, dataset)


  if(is.null(cat_labels)) cat_labels <- unlist(lapply(dataset[, vars], levels))

  var_labels <- rep(var_labels, sapply(dataset[, vars], function(x) length(levels(x))))
  vars <- names(dataset_expanded)

  dataset <- cbind(dataset, dataset_expanded)

  if(is.null(condvar_wide) & is.null(condvar_long)){

    output$table <- as.data.frame(t(sapply(dataset[, vars], stat_func, ...)))

  } else if(is.null(condvar_long) & !is.null(condvar_wide)){

    output$table <- lapply(unique(dataset[,condvar_wide]), function(x) as.data.frame(t(sapply(dataset[dataset[,condvar_wide] == x, vars], stat_func, ...))))

  } else if(!is.null(condvar_long) & is.null(condvar_wide)){

    temp_list <- lapply(unique(dataset[,condvar_long]), function(x){

      dat <- as.data.frame(t(sapply(dataset[dataset[,condvar_long] == x, vars], stat_func, ...)))

      dat$var_names <- vars
      dat <- dat[, c(ncol(dat), 1:(ncol(dat) - 1))]

      dat

    })

    long_df <- Reduce(rbind, temp_list)

    long_df <- long_df[order(long_df$var_names), ]

    output$table <- long_df


  } else {

    output$table <- lapply(unique(dataset[,condvar_wide]), function(x){

      temp_list <- lapply(unique(dataset[dataset[,condvar_wide] == x,condvar_long]), function(y){

        dat <- as.data.frame(t(sapply(dataset[dataset[,condvar_wide] == x & dataset[,condvar_long] == y, vars], stat_func,...)))

        dat$group <- y
        dat$var_name <- vars

        dat <- dat[, c("var_name", "group", names(dat)[-grep("var_name|group",names(dat))])]
        dat
      })

      temp_df <- Reduce(rbind, temp_list)

      temp_df <- temp_df[order(temp_df$var_name, temp_df$group), ]

      temp_df[, -(1:2)]


    })


  }

  class(output) <- c("magictable")

  if(is.null(var_labels)) var_labels <- vars
  if(is.null(condvar_long_labels) & !is.null(condvar_long)) condvar_long_labels <- unique(dataset[,condvar_long])
  if(is.null(condvar_wide_labels) & !is.null(condvar_wide)) condvar_wide_labels <- unique(dataset[,condvar_wide])
  if(is.null(stat_labels)){

    if(is.null(condvar_wide)){

      n_stats <- ncol(output$table[[1]])

    } else{

      n_stats <- ncol(output$table)

    }

    stat_labels <- paste0("x", 1:n_stats)

  }

  output$var_labels <- var_labels
  output$cat_labels <- cat_labels
  output$stat_labels <- stat_labels
  output$condvar_wide_labels <- condvar_wide_labels
  output$condvar_long_labels <- condvar_long_labels
  output$type <- "categorical"



  output
}



#' \code{magic_table} Object?
#'
#' \code{is.magic_table} checks if the input is of class \code{magic_table}.
#'
#' @param x an \code{R} object.
#'
#' @export
#'
#' @return a logical indicting whether \code{x} is of class \code{magic_table}.
is.magic_table <- function(x){

 inherits(x, "magictable")

}









