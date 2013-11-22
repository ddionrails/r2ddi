#' Variable description (Stata)
#'
#' Produce variable description for Stata-based documentation.
#' 
ddi_var_dscr_stata <- function(i,
                               var,
                               data,
                               val_labels,
                               import_options)
{

  main <- function()
  {
    var$val_labels  <- val_labels
    var$data_table  <- data.stata(data, var$miss, import_options$missing_codes)
    var$miss        <- NULL
    var$value_table <- .create_value_table(val_labels, import_options)
    var <- ddiExtractor(var,
                        import_options = import_options,
                        file_format    = "Stata")
    if(is.null(nrow(var$value_table)))
      var$value_table <- NULL
    else if(nrow(var$value_table) > 0 & var$intrvl == "labeled_numeric")
      var <- .label_stata_missings(var)
    else if(nrow(var$value_table) == 0)
      var$value_table <- NULL
    var
  }

  .create_value_table <- function(val_labels, import_options)
  {
    tmp_condition <- val_labels >= 2147483621 |
                     val_labels %in% import_options$missing_codes
    valid <- ifelse(tmp_condition, FALSE, TRUE)
    label <- names(val_labels)
    value <- val_labels
    if(is.null(value)) return(NULL)
    names(value) <- seq(length(value))
    names(valid) <- seq(length(valid))
    value_table <- data.frame(label = label ,
                              value = value ,
                              valid = valid ,
                              stringsAsFactors = FALSE )
    value_table
  }

  .label_stata_missings <- function(var)
  {
    # Generate ".a", ".b", ...
    tmp_select <- as.numeric(var$value_table$value) > 2147483621 &
                  !is.na(var$value_table$value)
    tmp_values <- var$value_table$value[tmp_select]
    tmp_values <- as.numeric(tmp_values) - 2147483621
    tmp_values <- paste(".", letters[tmp_values], sep="")
    var$value_table$value[tmp_select] <- tmp_values

    # Generate "."
    tmp_select <- suppressWarnings(as.numeric(var$value_table$value)) == 2147483621 &
                  !is.na(var$value_table$value)
    var$value_table$value[tmp_select] <- "."

    var
  }

  var <- main()
  var
}
