#' Survey Tables
#'
#' \code{survey_table} takes a dataset containing responses to WH
#' survey questions at different timepoints and constructs raw tables
#' of summary statistics.
#'
#' @param dataset a data.frame object.
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
survey_table <- function(dataset, vars, stat_func, condvar_wide = NULL, condvar_long = NULL,...){

  if(is.null(condvar_wide) & is.null(condvar_long)){

   as.data.frame(t(sapply(dataset[, vars], stat_func, ...)))

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

    long_df


  } else {

    lapply(unique(dataset[,condvar_wide]), function(x){

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
}












