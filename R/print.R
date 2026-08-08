#' Printing function for rules list
#'
#' @param x A \code{semid} object
#' @param names Character vector. The names of the columns of the main table
#' @param include.msgs Logical. If \code{TRUE} messages are printed
#' @param msgs.name Character. The name of the messages index column
#' @param msgs.sec Character. The name of the messages section
#' @param msgs.levels Named character vector. The names of the message levels,
#'        e.g., "1" = "Info", "2" = "Reason", "3" = "WARNING".
#' @param window Integer. The width of the output window
#' @param pos.lab Character. The label for positive cells, e.g., "Yes".
#' @param neg.lab Character. The label for negative cells, e.g., "No".
#' @param na.lab Character. The label for NA/blank cells.
#' @param print.version Logical. If \code{TRUE}, the version of the package is
#'        printed in a header before the rules output.
#' @param print.lav_fun Logical. If \code{TRUE}, the lavaan function is printed in a
#'        header before the rules output.
#' @param ... Not currently used.
#'
#' @export
print.semid <- function(x, ..., names = c("", "Pass", "Necessary", "Sufficient"),
                        include.msgs = TRUE, msgs.name = "Message", msgs.sec = "Messages",
                        msgs.levels = c("1" = "Info", "2" = "Reason", "3" = "WARNING"),
                        window = 56L, pos.lab = "Yes", neg.lab = "No", na.lab = "-",
                        print.version = TRUE, print.lav_fun = TRUE) {
  if (!is.null(x$print.options$include.msgs)) {
    if (x$print.options$include.msgs != include.msgs) {
      warning("The `include.msgs` argument in the print method is overriding the `include.msgs` argument in the semid object.")
    } else {
      include.msgs <- x$print.options$include.msgs
    }
  }
  if (include.msgs) {
    names[5] <- msgs.name
    msgs <- character(0) # global message vector
  }
  cols_width <- sum(nchar(names[-1])) + length(names[-1])
  names[1] <- format(names[1], width = window - cols_width)
  midx <- 0L # global message index

  if (print.version) {
    version <- utils::packageVersion("semidentify")
    cat(sprintf("semidentify %s Rule Check\n", version))
  }

  if (print.lav_fun) {
    lav_fun <- x$lav_fun
    cat(sprintf("lavaan function: %s\n", format_lavaan_fun(lav_fun)))
  }

  if (print.version || print.lav_fun) {
    cat("\n")
  }

  cat(names, strrep("\n", 1L))

  for (i in seq_along(x$Rules)) {
    this_rule <- x$Rules[[i]]
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
          "NA" = na.lab,
          "TRUE" = pos.lab,
          "FALSE" = neg.lab),
        width = nchar(names[2]), justify = "right"
      ),
      # necessary and/or sufficient?
      switch(
        as.character(this_rule$cond),
        "N" = c(pos.lab, neg.lab),
        "S" = c(neg.lab, pos.lab),
        "NS" = c(pos.lab, pos.lab),
        "NA" = rep(na.lab, 2)
      )
    )
    row[3:4] <- c(
      format(row[3], width = nchar(names[3]), justify = "right"),
      format(row[4], width = nchar(names[4]), justify = "right")
    )
    if (include.msgs && all(!is.na(this_rule$msgs))) {
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

  if (include.msgs && midx > 0) {
    cat("---", msgs.sec, sep = "\n")
    for (i in 1:midx) {
      # make space for index, e.g., "1 - ", "2 - ", etc.
      m0 <- strwrap(msgs[i], width = window, initial = sprintf("%s - ", i),
                    exdent = 4)
      cat(m0, sep = "\n")
    }
  }

  cat("\n")

  return(invisible(x))

}


#' Printing function for two-step rules lists
#' 
#' @param x A \code{semid2} object
#' @param ... Arguments to be passed to print.semid.
#' @param step.titles Character. The labels to be given to each step.
#' @param step.names Character. The names of each step/block.
#' @param print.version Logical. If \code{TRUE}, the version of the package is
#'        printed in a header before the rules output.
#' @param print.lav_fun Logical. If \code{TRUE}, the lavaan function is printed in a
#'        header before the rules output.
#' @export
print.semid2 <- function(x, ..., step.names = c("Measurement Model", "Latent Variable/Structural Model"),
                         step.titles = c("Step 1", "Step 2"), print.version = TRUE, print.lav_fun = TRUE) {

  # preliminary printing
  if (print.version) {
    version <- utils::packageVersion("semidentify")
  cat(sprintf("semidentify %s Two-Step Rule Check\n", version))
  }
  if (print.lav_fun) {
    lav_fun <- x$lav_fun
    cat(sprintf("lavaan function: %s\n", format_lavaan_fun(lav_fun)))
  }
  if (print.version || print.lav_fun) {
    cat("\n")
  }
  
  cat(paste0(step.titles[1], ": ", step.names[1], "\n\n"))
  print(x$id.cfa, print.version = FALSE, ..., print.lav_fun = FALSE)

  cat(paste0(step.titles[2], ": ", step.names[2], "\n\n"))
  print(x$id.reg, print.version = FALSE, ..., print.lav_fun = FALSE)

  return(invisible(x))

}


#' Printing function for scaling table
#' 
#' @param x A \code{semscale} object, i.e., a list of scaling information
#'        for each latent variable in the model.
#' @param include.msgs Logical. If \code{TRUE} messages are printed.
#' @param window Integer. The width of the output window.
#' @param sep.spaces Integer. The number of spaces to separate the row names from the row values.
#' @param indent.lens Integer vector of length 3. The number of spaces to indent
#'        for each level of information (currently there are three supported).
#' @param na.lab Character. The label for NA/blank cells.
#' @param pos.lab Character. The label for positive cells, e.g., "Yes".
#' @param neg.lab Character. The label for negative cells, e.g., "No".
#' @param empty.lab Character. The label for empty cells, e.g., "None".
#' @param bullet Character. The bullet symbol for messages, e.g., "-".
#' @param print.version Logical. If \code{TRUE}, the version of the package is printed in a
#'        header before the rules output.
#' @param print.lav_fun Logical. If \code{TRUE}, the lavaan function is printed in a
#'        header before the rules output.
#' @param ... Not currently used.
#'
#' @export
print.semscale <- function(x, ..., include.msgs = TRUE, window = 56L, sep.spaces = 3L,
                           indent.lens = c(0L, 2L, 2L), na.lab = "na",
                           pos.lab = "Yes", neg.lab = "No", empty.lab = "None",
                           bullet = "-", print.version = TRUE, print.lav_fun = TRUE) {
                            
  stopifnot("`indent.lens` must be three equal or ascending integers" =
    length(indent.lens) == 3 && indent.lens[2] >= indent.lens[1] && indent.lens[3] >= indent.lens[2])

  if (length(x) ==1 && is.na(x)) {
    cat("No latent variables in the model\n")
    return(invisible(x))
  }
  if (!is.null(x$print.options$include.msgs)) {
    if (x$print.options$include.msgs != include.msgs) {
      warning("The `include.msgs` argument in the print method is overriding the `include.msgs` argument in the semid object.")
    } else {
      include.msgs <- x$print.options$include.msgs
    }
  }

  scaling <- x$Scaling
  indents <- strrep(" ", indent.lens)

  if (print.version) {
    version <- utils::packageVersion("semidentify")
    cat(sprintf("semidentify %s Latent Variable Scaling\n", version))
  }

  if (print.lav_fun) {
    lav_fun <- x$lav_fun
    cat(sprintf("lavaan function: %s\n", format_lavaan_fun(lav_fun)))
  }

  if (print.version || print.lav_fun) {
    cat("\n")
  }

  for (i in seq_along(scaling)) {
    var <- scaling[[i]]$lv
    cat(sprintf("%s%s\n", indents[1], var))

    is.scaled <- switch(
      as.character(scaling[[i]]$scaled),
      "NA" = na.lab,
      "TRUE" = pos.lab,
      "FALSE" = neg.lab
    )
    
    n.ind <- as.integer(scaling[[i]]$n.indicators)
    scale.ind <- scaling[[i]]$scaling.indicator
    # in case of multiple scaling indicators
    scale.ind <- if (all(is.na(scale.ind))) {
      empty.lab
    } else {
      paste(scale.ind, collapse = ", ")
    }
    mean.structure <- switch(
      as.character(scaling[[i]]$mean.structure),
      "NA" = na.lab,
      "TRUE" = pos.lab,
      "FALSE" = neg.lab
    )

    row.names <- c(
      "LV is scaled?",
      "No. of indicators:",
      "Scaling indicator(s):",
      "Mean structure?"
    )
    row.names.nchar <- nchar(row.names)
    row.values.nspaces <- max(row.names.nchar) - row.names.nchar + sep.spaces
    row.values.spaces <- strrep(" ", row.values.nspaces)
    row.values <- c(is.scaled, n.ind, scale.ind, mean.structure)
    row.values <- paste0(row.values.spaces, row.values)

    cat(sprintf("%s%s %s\n", indents[2], row.names[1], row.values[1]))
    cat(sprintf("%s%s %s\n", indents[2], row.names[2], row.values[2]))
    cat(sprintf("%s%s %s\n", indents[2], row.names[3], row.values[3]))
    cat(sprintf("%s%s %s\n\n", indents[2], row.names[4], row.values[4]))

    if (include.msgs) {
      prefix <- paste0(bullet, " ")
      if (isTRUE(scaling[[i]]$scaled)) {
        # make space for index, e.g., "1 - ", "2 - ", etc.
        cat(sprintf("%sScaling method(s):\n", indents[2]))
        msgs <- strwrap(scaling[[i]]$scaling.method, width = window,
                        initial = paste0(indents[3], prefix),
                        prefix = indents[3], exdent = nchar(prefix))
        cat(msgs, sep = "\n")
      } else {
        cat(sprintf("%sScaling error:\n", indents[2]))
        cat(paste0(indents[3], prefix, scaling[[i]]$fail.reason))
      }
      cat("\n\n")
    }

  }

  return(invisible(x))

}