#' Printing function for rules list
#'
#' @param x A \code{semid} object
#' @param names Character vector. The names of the columns of the main table
#' @param print_msgs Logical. If \code{TRUE} messages are printed
#' @param msgs_name Character. The name of the messages index column
#' @param msgs_sec Character. The name of the messages section
#' @param window Integer. The width of the output window
#' @param pos_lab Character. The label for positive cells, e.g., "Yes".
#' @param neg_lab Character. The label for negative cells, e.g., "No".
#' @param na_lab Character. The label for \code{NA}/blank cells.
#' @param print_version Logical. If \code{TRUE}, the version of the package is
#'        printed in a header before the rules output.
#' @param print_meta Logical. If \code{TRUE}, the model type and lavaan function
#'        are printed in a table before the rules output.
#' @param meta_sep Character. The separator between the meta labels and values.
#' @param applicable_rules_policy Character. How to handle rules that are not
#'        applicable to the model. Options are "hide" (default), "footnote", or
#'        "show". Option "hide" does not print them at all; option "footnote",
#'        prints their names only in a footnote after the main table; option
#'        "show" prints them in the main table with a message indicating they
#'        are not applicable.
#' @param ... Arguments passed to print.semscale if applicable.
#'
#' @export
print.semid <- function(
  x,
  names = c("", "Pass", "Necessary", "Sufficient"),
  print_msgs = NULL,
  msgs_name = "Message",
  msgs_sec = "Messages",
  window = 56L,
  pos_lab = "Yes",
  neg_lab = "No",
  na_lab = "-",
  print_version = TRUE,
  print_meta = TRUE,
  meta_sep = ":",
  applicable_rules_policy = c("footnote", "hide", "show"), ...
) {
  if (is.null(print_msgs)) {
    print_msgs <- x$print_options$print_msgs %||% TRUE
  } else if (!is.null(x$print_options$print_msgs) &&
              !identical(print_msgs, x$print_options$print_msgs)) {
    id_warn(
      "`print_msgs` supplied to print() overrides the value stored in the object."
    )
  }

  applicable_rules_policy <- match.arg(applicable_rules_policy)
  id_model_type <- x$id_model_type
  footnote_rules <- character(0) # global footnote vector

  if (print_version) {
    version <- utils::packageVersion("semrulesid")
    cat(sprintf("semrulesid %s Rule Check\n\n", version))
  }

  if (print_meta) {
    meta_labels <- c("lavaan function", "Model type")
    meta_labels <- format(
      meta_labels,
      width = max(nchar(meta_labels)),
      justify = "left"
    )
    meta_rows <- c(
      paste0(meta_labels[1], " ", meta_sep, " ", format_lavaan_fun(x$lav_fun)),
      paste0(meta_labels[2], " ", meta_sep, " ", get_model_type_name(x$id_model_type, long = FALSE))
    )
    cat(paste(meta_rows, collapse = "\n"), "\n")
  }

  if (print_version || print_meta) {
    cat("\n")
  }

  if (print_msgs) {
    names[5] <- msgs_name
    msgs <- character(0) # global message vector
  }
  cols_width <- sum(nchar(names[-1])) + length(names[-1])
  names[1] <- format(names[1], width = window - cols_width)
  midx <- 0L # global message index

  cat(names, strrep("\n", 1L))

  for (i in seq_along(x$Rules)) {
    this_rule <- x$Rules[[i]]
    applicable <- id_model_type %in% this_rule$applies_to
    if (applicable_rules_policy == "hide" && !applicable) {
      next
    }
    if (applicable_rules_policy == "footnote" && !applicable) {
      footnote_rules <- c(footnote_rules, this_rule$rule)
      next
    }
    row <- c(
      # rule title
      format(
        this_rule$rule,
        width = window - cols_width
      ),
      # did the rule pass?
      format(
        switch(
          as.character(this_rule$pass),
          "NA" = na_lab,
          "TRUE" = pos_lab,
          "FALSE" = neg_lab
        ),
        width = nchar(names[2]), justify = "right"
      ),
      # necessary and/or sufficient?
      switch(
        as.character(this_rule$cond),
        "N" = c(pos_lab, neg_lab),
        "S" = c(neg_lab, pos_lab),
        "NS" = c(pos_lab, pos_lab),
        "NA" = rep(na_lab, 2)
      )
    )
    row[3:4] <- c(
      format(row[3], width = nchar(names[3]), justify = "right"),
      format(row[4], width = nchar(names[4]), justify = "right")
    )
    if (print_msgs && all(!is.na(this_rule$msgs))) {
      idx.m0 <- c()
      for (msg in this_rule$msgs) {
        if (msg %in% msgs) {
          # prevent duplicate messages
          idx.m0 <- c(idx.m0, which(msgs == msg))
        } else {
          midx <- midx + 1
          idx.m0 <- c(idx.m0, midx)
          msgs <- c(msgs, msg)
        }
      }
      cat(
        c(row, format(
          paste(idx.m0, collapse = ","),
          width = nchar(names[5]), justify = "right")),
        "\n"
      )
    } else {
      cat(row, "\n")
    }
  }
  if (length(footnote_rules) > 0) {
    cat("\n")
    footnote <- paste0(
      "Rules not applicable to ",
      get_model_type_name(
        x$id_model_type, capitalize = FALSE, plural = TRUE, long = FALSE
      ), ": ",
      paste(footnote_rules, collapse = ", ")
    )
    footnote <- strwrap(footnote, width = window, exdent = 2)
    cat(footnote, sep = "\n")
  }
  if (print_msgs && midx > 0) {
    cat("---", msgs_sec, sep = "\n")
    for (i in 1:midx) {
      # make space for index, e.g., "1 - ", "2 - ", etc.
      m0 <- strwrap(msgs[i], width = window, initial = sprintf("%s - ", i),
                    exdent = 4)
      cat(m0, sep = "\n")
    }
  }

  cat("\n")

  if (!is.null(x$scaling)) {
    print(x$scaling, ...)
  }

  invisible(x)

}


#' Printing function for two-step rules lists
#'
#' @param x A \code{semid2} object
#' @param ... Arguments to be passed to print.semid.
#' @param step_titles Character. The labels to be given to each step.
#' @param step_names Character. The names of each step/block.
#' @param print_version Logical. If \code{TRUE}, the version of the package is
#'        printed in a header before the rules output.
#' @param print_lav_fun Logical. If \code{TRUE}, the lavaan function is printed
#'        in a header before the rules output.
#' @export
print.semid2 <- function(
  x, ...,
  step_names = c("Measurement Model", "Latent Variable/Structural Model"),
  step_titles = c("Step 1", "Step 2"),
  print_version = TRUE,
  print_lav_fun = TRUE
) {

  applicable_rules_policy <- x$print_options$applicable_rules_policy %||% "hide"

  # preliminary printing
  if (print_version) {
    version <- utils::packageVersion("semrulesid")
    cat(sprintf("semrulesid %s Two-Step Rule Check\n", version))
  }
  if (print_lav_fun) {
    lav_fun <- x$lav_fun
    cat(sprintf("lavaan function: %s\n", format_lavaan_fun(lav_fun)))
  }
  if (print_version || print_lav_fun) {
    cat("\n")
  }

  cat(paste0(step_titles[1], ": ", step_names[1], "\n\n"))
  print(x$id_cfa, print_version = FALSE, ..., print_lav_fun = FALSE, applicable_rules_policy = applicable_rules_policy)

  cat(paste0(step_titles[2], ": ", step_names[2], "\n\n"))
  print(x$id_reg, print_version = FALSE, ..., print_lav_fun = FALSE, applicable_rules_policy = applicable_rules_policy)

  invisible(x)

}


#' Printing function for scaling table
#'
#' @param x A \code{semscale} object, i.e., a list of scaling information
#'        for each latent variable in the model.
#' @param print_msgs Logical. If \code{TRUE} messages are printed.
#' @param window Integer. The width of the output window.
#' @param sep_spaces Integer. The number of spaces to separate the row names
#' from the row values.
#' @param indent_lengths Integer vector of length 3. The number of spaces to
#'        indent for each level of information (currently there are three
#'        supported).
#' @param na_lab Character. The label for NA/blank cells.
#' @param pos_lab Character. The label for positive cells, e.g., "Yes".
#' @param neg_lab Character. The label for negative cells, e.g., "No".
#' @param empty_lab Character. The label for empty cells, e.g., "None".
#' @param bullet Character. The bullet symbol for messages, e.g., "-".
#' @param print_version Logical. If \code{TRUE}, the version of the package is
#'        printed in a header before the rules output.
#' @param print_lav_fun Logical. If \code{TRUE}, the lavaan function is printed
#'        in a header before the rules output.
#' @param ... Not currently used.
#'
#' @export
print.semscale <- function(
  x, ...,
  print_msgs = TRUE,
  window = 56L,
  sep_spaces = 3L,
  indent_lengths = c(0L, 2L, 2L),
  na_lab = "na",
  pos_lab = "Yes",
  neg_lab = "No",
  empty_lab = "None",
  bullet = "-",
  print_version = TRUE,
  print_lav_fun = TRUE
) {
                            
  stopifnot("`indent_lengths` must be three equal or ascending integers" =
    length(indent_lengths) == 3 && indent_lengths[2] >= indent_lengths[1] && indent_lengths[3] >= indent_lengths[2])

  if (length(x) ==1 && is.na(x)) {
    cat("No latent variables in the model\n")
    return(invisible(x))
  }
  if (!is.null(x$print_options$print_msgs)) {
    if (x$print_options$print_msgs != print_msgs) {
      id_warn("The `print_msgs` argument in the print method is overriding the `print_msgs` argument in the semscale object.")
    } else {
      print_msgs <- x$print_options$print_msgs
    }
  }

  scaling <- x$Scaling
  indents <- strrep(" ", indent_lengths)

  if (print_version) {
    version <- utils::packageVersion("semrulesid")
    cat(sprintf("semrulesid %s Latent Variable Scaling\n", version))
  }

  if (print_lav_fun) {
    lav_fun <- x$lav_fun
    if (!is.na(lav_fun)) {
      cat(sprintf("lavaan function: %s\n", format_lavaan_fun(lav_fun)))
    }
  }

  if (print_version || print_lav_fun) {
    cat("\n")
  }

  for (i in seq_along(scaling)) {
    var <- scaling[[i]]$lv
    cat(sprintf("%s%s\n", indents[1], var))

    is_scaled <- switch(
      as.character(scaling[[i]]$scaled),
      "NA" = na_lab,
      "TRUE" = pos_lab,
      "FALSE" = neg_lab
    )

    n_indicators <- as.integer(scaling[[i]]$n_indicators)
    scaling_indicator <- scaling[[i]]$scaling_indicator
    # in case of multiple scaling indicators
    scaling_indicator <- if (all(is.na(scaling_indicator))) {
      empty_lab
    } else {
      paste(scaling_indicator, collapse = ", ")
    }
    mean_structure <- switch(
      as.character(scaling[[i]]$mean_structure),
      "NA" = na_lab,
      "TRUE" = pos_lab,
      "FALSE" = neg_lab
    )

    row_names <- c(
      "LV is scaled?",
      "No. of indicators:",
      "Scaling indicator(s):",
      "Mean structure?"
    )
    row_names_nchar <- nchar(row_names)
    row_values_nspaces <- max(row_names_nchar) - row_names_nchar + sep_spaces
    row_values_spaces <- strrep(" ", row_values_nspaces)
    row_values <- c(is_scaled, n_indicators, scaling_indicator, mean_structure)
    row_values <- paste0(row_values_spaces, row_values)

    cat(sprintf("%s%s %s\n", indents[2], row_names[1], row_values[1]))
    cat(sprintf("%s%s %s\n", indents[2], row_names[2], row_values[2]))
    cat(sprintf("%s%s %s\n", indents[2], row_names[3], row_values[3]))
    cat(sprintf("%s%s %s\n\n", indents[2], row_names[4], row_values[4]))

    if (print_msgs) {
      prefix <- paste0(bullet, " ")
      if (isTRUE(scaling[[i]]$scaled)) {
        # make space for index, e.g., "1 - ", "2 - ", etc.
        cat(sprintf("%sScaling method(s):\n", indents[2]))
        msgs <- strwrap(scaling[[i]]$scaling_method, width = window,
                        initial = paste0(indents[3], prefix),
                        prefix = indents[3], exdent = nchar(prefix))
        cat(msgs, sep = "\n")
      } else {
        cat(sprintf("%sScaling error:\n", indents[2]))
        cat(paste0(indents[3], prefix, scaling[[i]]$fail_reason))
      }
      cat("\n\n")
    }

  }

  invisible(x)

}