source(testthat::test_path("..", "test_models.R"))

test_that("rule functions produce the correct output", {
  rules <- get_rules(rule = "*", model_type = "*")
  partable <- lavaan::lavaanify("y ~ x", warn = FALSE)
  for (fn in names(rules)) {
    expect_named(
      do.call(rules[[fn]], list(partable)),
      c("rule", "pass", "msgs", "cond", "applies_to"),
      label = fn, ignore.order = FALSE
    )
    expect_in(
      do.call(rules[[fn]], list(partable))$applies_to,
      c("reg", "cfa", "sem")
    )
  }
})

test_that("printed rule titles should be correct length", {
  rules <- get_rule_names(model_type = "all")
  partable <- lavaan::lavaanify("y ~ x", warn = FALSE)
  test <- capture.output(id(partable, print_msgs = TRUE, lav_fun = NA))
  header <- grep("Pass", test)
  blank <- sub("^(\\s+)([A-Za-z\\s]+)$", "\\1", test[header], perl = TRUE)
  for (rule in rules) {
    title <- do.call(rule, list(partable))$rule
    expect_lt(nchar(!!title), nchar(blank))
  }
})

test_that("get_rules() returns the correct rule functions", {
  rules <- get_rules(rule = "*", model_type = "all")
  expect_true(all(sapply(rules, is.function)))
  expect_true(all(names(rules) %in% get_rule_names(model_type = "all")))
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