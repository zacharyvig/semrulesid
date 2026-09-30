source(testthat::test_path("..", "test_models.R"))

make_partable <- function(model) {
  lavaan::lavaanify(
    model$model,
    warn = FALSE,
    auto = TRUE,
    model_type = model$type
  )
}

test_that("rule functions produce the correct output", {
  rules <- get_rules(rule = "*", model_type = "*")
  partable <- lavaan::lavaanify("y ~ x", warn = FALSE)
  for (fn in names(rules)) {
    metadata <- attr(rules[[fn]], "metadata")
    expect_true(is.list(metadata), label = fn)
    expect_true(all(c("fn", "rule", "applies_to") %in% names(metadata)), label = fn)

    out <- do.call(rules[[fn]], list(partable))
    expect_named(
      out,
      c("metadata", "pass", "msgs", "cond"),
      label = fn, ignore.order = FALSE
    )
    expect_identical(out$metadata$fn, fn, label = fn)
    expect_identical(out$metadata$rule, metadata$rule, label = fn)
    expect_identical(out$metadata$applies_to, metadata$applies_to, label = fn)
  }
})

test_that("print() emits applicable rows for id()", {
  partables <- list(
    reg = lavaan::lavaanify("y ~ x", warn = FALSE),
    cfa = lavaan::lavaanify("f =~ y1 + y2 + y3", warn = FALSE),
    sem = lavaan::lavaanify("f =~ y1 + y2 + y3; f ~ x", warn = FALSE)
  )
  expected_rules <- c(
    reg = "Null B_YY Rule",
    cfa = "Two Indicator Rule",
    sem = "N_theta Rule (t-Rule)"
  )

  for (model in names(partables)) {
    printed <- capture.output(
      print(id(partables[[model]], print_msgs = FALSE, lav_fun = NA), na_rule_policy = "hide")
    )
    expect_gt(length(printed), 3, label = model)
    expect_true(any(grepl(expected_rules[[model]], printed, fixed = TRUE)), label = model)
  }
})

test_that("print() emits applicable rows for id2()", {
  printed <- capture.output(
    print(
      id2(
        make_partable(list(
          type = "sem",
          model = test_models$sem_complex$model
        )),
        lav_fun = NA
      )
    )
  )

  expect_true(any(grepl("Two-Step Rule Check", printed, fixed = TRUE)))
  expect_true(any(grepl("Step 1: Measurement Model", printed, fixed = TRUE)))
  expect_true(any(grepl("Step 2: Latent Variable/Structural Model", printed, fixed = TRUE)))
  expect_true(sum(grepl("N_theta Rule (t-Rule)", printed, fixed = TRUE)) >= 2)
})

test_that("print() emits applicable rows for scaling()", {
  printed <- capture.output(
    print(
      scaling(
        make_partable(test_models$sem_scaling_pass),
        lv = "L1",
        lav_fun = NA
      ),
      print_msgs = TRUE
    )
  )

  expect_true(any(grepl("Latent Variable Scaling", printed, fixed = TRUE)))
  expect_true(any(grepl("LV is scaled", printed, fixed = TRUE)))
  expect_true(any(grepl("Scaling indicator(s)", printed, fixed = TRUE)))
})

test_that("printed rule titles should be correct length", {
  rules <- get_rule_names()
  partable <- lavaan::lavaanify("y ~ x", warn = FALSE)
  test <- capture.output(id(partable, print_msgs = TRUE, lav_fun = NA))
  header <- grep("Pass", test)
  blank <- sub("^(\\s+)([A-Za-z\\s]+)$", "\\1", test[header], perl = TRUE)
  for (rule in rules) {
    title <- do.call(rule, list(partable))$metadata$rule
    expect_lt(nchar(!!title), nchar(blank))
  }
})

test_that("get_rules() returns the correct rule functions", {
  rules <- get_rules(rule = "*", model_type = "all")
  expect_true(all(sapply(rules, is.function)))
  expect_true(all(names(rules) %in% get_rule_names()))
  # test partial matching of rule names
  rules <- get_rules(rule = "latent_scaling", model_type = "sem")
  expect_length(rules, 1)
  expect_true(is.function(rules[[1]]))
  expect_equal(names(rules), "rule_sem_latent_scaling")
})

test_that("add_rule_msgs() adds messages correctly", {
  expect_identical(
    unname(add_rule_msgs(msgs = NA_character_, new_msgs = "One")),
    "One"
  )

  expect_identical(
    unname(add_rule_msgs(msgs = c("One", "Two"), new_msgs = "Three")),
    c("One", "Two", "Three")
  )

  expect_identical(
    names(add_rule_msgs(
      msgs = c("One", "Two"),
      new_msgs = c("Three", "Four"),
      levels = c("not_applicable", "suff_cond_not_satisfied")
    )),
    c("", "", "not_applicable", "suff_cond_not_satisfied")
  )

  expect_identical(
    unname(add_rule_msgs(
      msgs = c("One", "Two"),
      new_msgs = c("Three", "Four"),
      levels = c(NA, NA)
    )),
    c("One", "Two", "Three", "Four")
  )
})


test_that("na_rule_policy works correctly", {
  partables <- list(
    "reg" = lavaan::lavaanify("y ~ x", warn = FALSE),
    "cfa" = lavaan::lavaanify("f =~ y1 + y2 + y3", warn = FALSE),
    "sem" = lavaan::lavaanify("f =~ y1 + y2 + y3; f ~ x", warn = FALSE)
  )

  example_na_rules <- c(
    "reg" = "Latent(\\s+)Scaling(\\s+)Rule",
    "cfa" = "Null(\\s+)B_YY(\\s+)Rule",
    "sem" = "Null(\\s+)B_YY(\\s+)Rule"
  )

  for (model in c("reg", "cfa", "sem")) {
    partable <- partables[[model]]
    na_rule <- example_na_rules[model]
    hide <- capture.output(print(id(partable, print_msgs = TRUE, lav_fun = NA), na_rule_policy = "hide"))
    show <- capture.output(print(id(partable, print_msgs = TRUE, lav_fun = NA), na_rule_policy = "show"))
    footnote <- capture.output(print(id(partable, print_msgs = TRUE, lav_fun = NA), na_rule_policy = "footnote"))

    expect_true(any(grepl(na_rule, paste(show, collapse = ""))), label = paste("show policy for", model))
    expect_false(any(grepl(na_rule, paste(hide, collapse = ""))), label = paste("hide policy for", model))
    expect_true(any(grepl(na_rule, paste0(footnote, collapse = ""))), label = paste("footnote policy for", model))
  }
  
})