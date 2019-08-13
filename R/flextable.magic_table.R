#' Magic Table Generic for Flextable
#'
#' @param data an object of class \code{magic_table}.
#' @param ... additional arguments to be passed to the function \code{flextable}.
#'
#' @export
#'
#' @return a \code{flextable} object constructed from a \code{magictable} object.
flextable.magic_table <- function(data){
  mt <- data
  var_labels <- mt$var_labels
  stat_labels <- mt$stat_labels
  condvar_wide_labels <- mt$condvar_wide_labels
  condvar_long_labels <- mt$condvar_long_labels
  tables <- mt$table

  if(mt$type == "numeric"){
  if(is.null(condvar_wide_labels) & is.null(condvar_long_labels)){
    names(tables) <- stat_labels
    tables$Variable <- var_labels
    tables <- tables[,c("Variable", names(tables)[-grep("Variable", names(tables))])]
    output_table <- flextable::flextable(tables)
    output_table <- flextable::set_header_labels(output_table, values = stat_labels)

    output_table
  } else if(!is.null(condvar_wide_labels) & is.null(condvar_long_labels)){
    # combining table list into one data.frame
    big_table <- data.frame(variable = var_labels)
    big_table <- cbind(big_table, as.data.frame(Reduce(cbind, tables)))

    names(big_table)  <- c("variable",paste(stat_labels, rep(condvar_wide_labels, each = length(stat_labels)), sep = "_"))


    stat_labels_list <- as.list(c("", rep(stat_labels, length(condvar_wide_labels))))

    names(stat_labels_list) <- names(big_table)

    output_table <- flextable::flextable(big_table)

    output_table <- flextable::set_header_labels(output_table, values = stat_labels_list )

    header_row_vals <- c("Variable",sapply(condvar_wide_labels, function(x) c(rep("", length(stat_labels) - 1), x)))
    output_table <- flextable::add_header_row(output_table,top = T,  values = header_row_vals, c(1,rep(1, length(stat_labels)*length(condvar_wide_labels))))

    output_table

  } else if(is.null(condvar_wide_labels) & !is.null(condvar_long_labels)){

    tables$group <- condvar_long_labels
    tables$var_names <- rep(var_labels, each = length(condvar_long_labels))
    tables <- tables[, c(1, ncol(tables), 2:(ncol(tables) - 1))]



    output_table <- flextable::flextable(tables)

    header_label_list <- as.list(c("Variable", "Group", stat_labels))
    names(header_label_list) <- names(tables)

    output_table <- flextable::set_header_labels(output_table, values = header_label_list )


    output_table

  } else{


    big_table <- Reduce(cbind, tables)
    big_table$group <- condvar_long_labels
    big_table$var_name <- rep(var_labels, each = length(condvar_long_labels))

    big_table <- big_table[, c(ncol(big_table), ncol(big_table) - 1, 1:(ncol(big_table) - 2))]

    names(big_table)  <- c("var_name", "group",paste(stat_labels, rep(condvar_wide_labels, each = length(stat_labels)), sep = "_"))

    stat_labels_list <- as.list(c("", "",rep(stat_labels, length(condvar_wide_labels))))

    names(stat_labels_list) <- names(big_table)

    output_table <- flextable::flextable(big_table)

    output_table <- flextable::set_header_labels(output_table, values = stat_labels_list )

    header_row_vals <- c("Variable", "Group", sapply(condvar_wide_labels, function(x) c(rep("", length(stat_labels) - 1), x)))
    output_table <- flextable::add_header_row(output_table,top = T,  values = header_row_vals, rep(1, 2 + length(stat_labels)*length(condvar_wide_labels)))


    output_table

  }

  } else {
    if(is.null(condvar_wide_labels) & is.null(condvar_long_labels)){
      names(tables) <- stat_labels
      tables$Variable <- var_labels
      tables$Category <- data$cat_labels
      tables <- tables[,c("Variable", "Category", names(tables)[!(names(tables) %in% c("Variable", "Category"))])]
      output_table <- flextable::flextable(tables)
      output_table <- flextable::set_header_labels(output_table, values = stat_labels)

      output_table
    } else if(!is.null(condvar_wide_labels) & is.null(condvar_long_labels)){
      # combining table list into one data.frame
      big_table <- data.frame(variable = var_labels, category = data$cat_labels)
      big_table <- cbind(big_table, as.data.frame(Reduce(cbind, tables)))

      names(big_table)  <- c("variable","category",paste(stat_labels, rep(condvar_wide_labels, each = length(stat_labels)), sep = "_"))


      stat_labels_list <- as.list(c("", "", rep(stat_labels, length(condvar_wide_labels))))

      names(stat_labels_list) <- names(big_table)

      output_table <- flextable::flextable(big_table)

      output_table <- flextable::set_header_labels(output_table, values = stat_labels_list )

      header_row_vals <- c("Variable", "Category", sapply(condvar_wide_labels, function(x) c(rep("", length(stat_labels) - 1), x)))
      output_table <- flextable::add_header_row(output_table,top = T,  values = header_row_vals, c(1,1, rep(1, length(stat_labels)*length(condvar_wide_labels))))

      output_table

    } else if(is.null(condvar_wide_labels) & !is.null(condvar_long_labels)){

      tables$group <- condvar_long_labels
      tables$var_names <- rep(var_labels, each = length(condvar_long_labels))
      tables <- tables[, c(1, ncol(tables), 2:(ncol(tables) - 1))]



      output_table <- flextable::flextable(tables)

      header_label_list <- as.list(c("Variable", "Group", stat_labels))
      names(header_label_list) <- names(tables)

      output_table <- flextable::set_header_labels(output_table, values = header_label_list )


      output_table

    } else{


      big_table <- Reduce(cbind, tables)
      big_table$group <- condvar_long_labels
      big_table$var_name <- rep(var_labels, each = length(condvar_long_labels))

      big_table <- big_table[, c(ncol(big_table), ncol(big_table) - 1, 1:(ncol(big_table) - 2))]

      names(big_table)  <- c("var_name", "group",paste(stat_labels, rep(condvar_wide_labels, each = length(stat_labels)), sep = "_"))

      stat_labels_list <- as.list(c("", "",rep(stat_labels, length(condvar_wide_labels))))

      names(stat_labels_list) <- names(big_table)

      output_table <- flextable::flextable(big_table)

      output_table <- flextable::set_header_labels(output_table, values = stat_labels_list )

      header_row_vals <- c("Variable", "Group", sapply(condvar_wide_labels, function(x) c(rep("", length(stat_labels) - 1), x)))
      output_table <- flextable::add_header_row(output_table,top = T,  values = header_row_vals, rep(1, 2 + length(stat_labels)*length(condvar_wide_labels)))


      output_table

    }




}

}

#' @importFrom flextable flextable
setGeneric(name = "flextable")
setOldClass("magictable")

#' @export flextable
flextable <- flextable

#' @export
setMethod("flextable", "magictable", flextable.magic_table)







