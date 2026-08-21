#' Printing function for rules list
#'
#' @param x A \code{semid} object
#' @param col_names Character vector. The names of the columns of the main table
#' @param print_msgs Logical. If \code{TRUE} messages are printed
#' @param msgs_name Character. The name of the messages index column
#' @param window Integer. The width of the output window
#' @param pos_lab Character. The label for positive cells, e.g., "Yes".
#' @param neg_lab Character. The label for negative cells, e.g., "No".
#' @param na_lab Character. The label for \code{NA}/blank cells.
#' @param print_version Logical. If \code{TRUE}, the version of the package is
#'        printed in a header before the rules output.
#' @param print_lav_fun Logical. If \code{TRUE}, the lavaan function is printed
#'        in a header before the rules output.
#' @param print_model_type Logical. If \code{TRUE}, the model type is printed
#'        in a header before the rules output.
#' @param name_value_sep Character. The separator between the meta labels and values.
#' @param na_rule_policy Character. How to handle rules that are not
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
  col_names = c("", "Pass", "Necessary", "Sufficient"),
  print_msgs = NULL,
  msgs_name = "Message",
  window = 56L,
  pos_lab = "Yes",
  neg_lab = "No",
  na_lab = "-",
  print_version = TRUE,
  print_lav_fun = TRUE,
  print_model_type = TRUE,
  name_value_sep = ":",
  na_rule_policy = c("footnote", "hide", "show"), ...
) {

  # check for conflicting print options
  if (is.null(print_msgs)) {
    print_msgs <- x$print_options$print_msgs %||% TRUE
  } else if (!is.null(x$print_options$print_msgs) &&
              !identical(print_msgs, x$print_options$print_msgs)) {
    id_warn(
      "`print_msgs` supplied to print() overrides the value stored in the object."
    )
  }

  # info for checking for applicable rules
  na_rule_policy <- match.arg(na_rule_policy)
  id_model_type <- x$id_model_type
  footnote_rules <- character(0) # global footnote vector

  # semid object "meta" data printing
  print_model_meta(
    print_version = print_version,
    print_lav_fun = print_lav_fun,
    print_model_type = print_model_type,
    obj_type = "semid",
    lav_fun = x$lav_fun,
    id_model_type = x$id_model_type,
    window = window,
    name_value_sep = name_value_sep
  )

  # setup for printing messages (if requested)
  if (print_msgs) {
    col_names[length(col_names) + 1] <- msgs_name
    msgs <- character(0) # global message vector
  }
  cols_width <- sum(nchar(col_names[-1])) + length(col_names[-1])
  col_names[1] <- format(col_names[1], width = window - cols_width)
  global_msg_idx <- 0L # global message index

  cat(col_names, strrep("\n", 1L))

  for (i in seq_along(x$Rules)) {
    this_rule <- x$Rules[[i]]
    applicable <- id_model_type %in% this_rule$applies_to
    # skip rules that are not applicable if na_rule_policy is "hide"
    if (na_rule_policy == "hide" && !applicable) {
      next
    }
    # save rules that are not applicable for footnote printing if na_rule_policy is "footnote"
    if (na_rule_policy == "footnote" && !applicable) {
      footnote_rules <- c(footnote_rules, this_rule$rule)
      next
    }
    row <- c(
      # rule title
      "rule_title" = format(
        this_rule$rule,
        width = window - cols_width
      ),
      # did the rule pass?
      "pass" = format(
        switch(
          as.character(this_rule$pass),
          "NA" = na_lab,
          "TRUE" = pos_lab,
          "FALSE" = neg_lab
        ),
        width = nchar(col_names[2]), justify = "right"
      ),
      # necessary and/or sufficient?
      "necessary" = switch(
        as.character(this_rule$cond),
        "N" = pos_lab,
        "S" = neg_lab,
        "NS" = pos_lab,
        "NA" = na_lab
      ),
      "sufficient" = switch(
        as.character(this_rule$cond),
        "N" = neg_lab,
        "S" = pos_lab,
        "NS" = pos_lab,
        "NA" = na_lab
      )
    )
    row[c("necessary", "sufficient")] <- c(
      format(row["necessary"], width = nchar(col_names[3]), justify = "right"),
      format(row["sufficient"], width = nchar(col_names[4]), justify = "right")
    )
    # if printing messages, add message index column
    if (print_msgs && all(!is.na(this_rule$msgs))) {
      msg_nums <- c()
      for (i in seq_along(this_rule$msgs)) {
        msg <- this_rule$msgs[[i]]
        names(msg) <- names(this_rule$msgs)[i]
        if (msg %in% msgs) {
          # prevent duplicate messages
          msg_nums <- c(msg_nums, which(msgs == msg))
        } else {
          global_msg_idx <- global_msg_idx + 1
          msg_nums <- c(msg_nums, global_msg_idx)
          msgs <- c(msgs, msg)
        }
      }
      cat(
        c(row, format(
          paste(msg_nums, collapse = ","),
          width = nchar(col_names[5]), justify = "right")),
        "\n"
      )
    } else {
      cat(row, "\n")
    }
  }
  # print footnote if requested
  if (identical(na_rule_policy, "footnote") && length(footnote_rules) > 0) {
    cat("\n")
    footnote_header <- paste0(
      "Rules not applicable to ",
      get_model_type_name(
        x$id_model_type, capitalize = FALSE, plural = TRUE, long = FALSE
      ), ": ")
    footnote_header <- strwrap(footnote_header, width = window)
    footnote_body <- strwrap(
      paste(footnote_rules, collapse = ", "),
      width = window - 2, indent = 2, exdent = 2
    )
    cat(footnote_header, footnote_body, sep = "\n")
  }

  # print messages ordered by severity level if requested
  if (print_msgs && global_msg_idx > 0L) {
    level_sections <- get_rule_level_labels(type = "labels")
    level_order <- get_rule_level_labels(type = "order")
    # sort messages by level of severity
    present_levels <- intersect(
      names(level_sections),
      unique(names(msgs))
    )
    present_levels <- present_levels[order(level_order[present_levels])]

    cat("\n---\n\n")

    for (level in present_levels) {
      msg_idx <- which(names(msgs) == level)
      cat(level_sections[[level]], ":\n\n", sep = "")

      for (i in msg_idx) {
        prefix <- sprintf("(%d) ", i)
        wrapped_msg <- strwrap(
          msgs[[i]],
          width = window,
          initial = prefix,
          exdent = nchar(prefix)
        )
        cat(wrapped_msg, sep = "\n")
      }

      cat("\n")

    }
  }

  if (!is.null(x$scaling)) {
    cat(strrep("-", window), "\n\n")
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
#' @param print_model_type Logical. If \code{TRUE}, the model type is printed
#'        in a header before the rules output.
#' @param window Integer. The width of the output window.
#' @param name_value_sep Character. The separator between the meta labels and
#'        values.
#' @export
print.semid2 <- function(
  x, ...,
  step_names = c("Measurement Model", "Latent Variable/Structural Model"),
  step_titles = c("Step 1", "Step 2"),
  print_version = TRUE,
  print_lav_fun = TRUE,
  print_model_type = TRUE,
  window = 56L,
  name_value_sep = ":"
) {

  na_rule_policy <- x$print_options$na_rule_policy %||% "hide"

  # print semid2 meta information
  meta_printed <- print_model_meta(
    print_version = print_version,
    print_lav_fun = print_lav_fun,
    print_model_type = print_model_type,
    obj_type = "semid2",
    lav_fun = x$lav_fun,
    id_model_type = x$id_model_type,
    window = window
  )
  if (meta_printed) {
    cat(strrep("-", window), "\n\n")
  }

  # print step 1 semid object
  cat(paste0(step_titles[1], ": ", step_names[1], "\n\n"))
  print(
    x$id_cfa,
    print_version = FALSE,
    ...,
    print_lav_fun = FALSE,
    print_model_type = FALSE,
    na_rule_policy = na_rule_policy,
    window = window
  )

  cat(strrep("-", window), "\n\n")

  # print step 2 semid object
  cat(paste0(step_titles[2], ": ", step_names[2], "\n\n"))
  print(
    x$id_reg,
    print_version = FALSE,
    ...,
    print_lav_fun = FALSE,
    print_model_type = FALSE,
    na_rule_policy = na_rule_policy,
    window = window
  )

  cat("\n")

  invisible(x)

}


#' Printing function for scaling table
#'
#' @param x A \code{semscale} object, i.e., a list of scaling information
#'        for each latent variable in the model.
#' @param print_msgs Logical. If \code{TRUE} messages are printed.
#' @param window Integer. The width of the output window.
#' @param sep_spaces Integer. The number of spaces to separate the row names
#'        from the row values.
#' @param indent_lengths Integer vector of length 3. The number of spaces to
#'        indent for each level of information (currently there are three
#'        supported).
#' @param name_value_sep Character. The separator between the row names and row
#'        values.
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
  name_value_sep = ":",
  print_version = TRUE,
  print_lav_fun = TRUE
) {

  # check for conflicting print options                          
  stopifnot("`indent_lengths` must be three equal or ascending integers" =
    length(indent_lengths) == 3 && indent_lengths[2] >= indent_lengths[1] && indent_lengths[3] >= indent_lengths[2])

  if (length(x) == 1 && is.na(x)) {
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

  # print semscale meta information
  print_model_meta(
    print_version = print_version,
    print_lav_fun = print_lav_fun,
    print_model_type = FALSE,
    obj_type = "semscale",
    lav_fun = x$lav_fun,
    id_model_type = x$id_model_type,
    window = window
  )

  for (i in seq_along(scaling)) {
    var <- scaling[[i]]$lv
    cat(sprintf("%s%s\n", indents[1], var))

    # gather info about the latent variable
    is_scaled <- switch(
      as.character(scaling[[i]]$scaled),
      "NA" = na_lab,
      "TRUE" = pos_lab,
      "FALSE" = neg_lab
    )
    n_indicators <- as.integer(scaling[[i]]$n_indicators)
    scaling_indicator <- scaling[[i]]$scaling_indicator
    # in case of multiple scaling indicators:
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

    # print scaling information like a table
    row_names <- c(
      "LV is scaled",
      "No. of indicators",
      "Scaling indicator(s)",
      "Mean structure"
    )
    row_values <- c(is_scaled, n_indicators, scaling_indicator, mean_structure)
    col_width <- max(nchar(row_names)) + 1
    cat(
      paste0(
        indents[2],
        format(row_names, width = max(nchar(row_names)) + 1, justify = "left"),
        ": ",
        row_values,
        collapse = "\n"
      ),
      "\n\n"
    )

    # print scaling method or scaling error messages if requested
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

# wrapper function to print semid, semscale, or semid2 meta information
#' @noRd
print_model_meta <- function(
  print_version = TRUE,
  print_lav_fun = TRUE,
  print_model_type = TRUE,
  obj_type = c("semid", "semscale", "semid2"),
  lav_fun = NA,
  id_model_type = NA,
  window = 56L,
  name_value_sep = ":"
) {
  obj_type <- match.arg(obj_type)
  if (print_version) {
    obj_type_label <- switch(
      obj_type,
      "semid" = "Rule Check",
      "semscale" = "Latent Variable Scaling",
      "semid2" = "Two-Step Rule Check"
    )
    version <- utils::packageVersion("semrulesid")
    cat(sprintf("semrulesid %s %s\n\n", version, obj_type_label))
  }
  if (print_lav_fun || print_model_type) {
    meta_labels <- c("Fitting function", "Model type")
    to_keep <- c(print_lav_fun, print_model_type)
    meta_width <- max(nchar(meta_labels)) + 1
    meta_values <- c(
      format_lavaan_fun(lav_fun),
      get_model_type_name(id_model_type, long = FALSE)
    )
    meta_lines <- paste0(
      format(meta_labels[to_keep], width = meta_width, justify = "left"),
      name_value_sep, " ", meta_values[to_keep]
    )
    cat(paste0(meta_lines, collapse = "\n"), "\n\n")
  }
  if (print_version || print_lav_fun || print_model_type) {
    invisible(TRUE)
  } else {
    invisible(FALSE)
  }
}