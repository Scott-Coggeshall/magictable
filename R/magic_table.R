#' Magic Table
#'
#' \code{magic_table} creates a magic table object from a data.frame-type object.
#'
#' @param dataset a data.frame object, or object that can be coerced to a data.frame.
#' @param vars a vector of character strings containing the names of the variables
#' for which summary statistics should be computed.
#' @param stat_func a function for calculating the summary statistics for the variables specified in \code{vars}.
#' @param condvar_wide a character string indicating a variable in \code{dataset} for wide conditioning,
#' i.e. within-group statistics are added as extra columns.
#' @param condvar_long a character string indicating a variable in \code{dataset} for long conditioning,
#' i.e. within-group statistics are added as extra rows.
#' @param ... additional variables to be passed to \code{stat_func}.
#'
#' @export
#'
#' @return a list containing tables of summary statistics for the specified variables.
magic_table <- function(dataset, vars, var_labels = NULL, stat_func, stat_labels, condvar_wide = NULL, condvar_wide_labels = NULL,
                        condvar_long = NULL, condvar_long_labels = NULL, ...){
  output <- list()

  if(is.null(condvar_wide) & is.null(condvar_long)){

   output$table <- as.data.frame(t(sapply(dataset[, vars], stat_func, ...)))

  } else if(is.null(condvar_long) & !is.null(condvar_wide)){

    lapply(unique(dataset[,condvar_wide]), function(x) as.data.frame(t(sapply(dataset[dataset[,condvar_wide] == x, vars], stat_func, ...))))

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
  if(is.null(condvar_long_labels)) <- condvar_long
  if(is.null(condvar_wide_labels)) <- condvar_wide
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
  output$stat_labels <- stat_labels



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

 inherits(x, "magic_table")

}









