source(testthat::test_path("..", "test_models.R"))

test_that("rule functions produce the correct output", {
  rules <- get_rules(rule = "*", model_type = "*")
  partable <- lavaan::lavaanify("y ~ x", warn = FALSE)
  for (fn in names(rules)) {
    expect_named(
      do.call(rules[[fn]], list(partable)),
      c("rule", "pass", "msgs", "cond"),
      label = fn, ignore.order = FALSE
    )
  }
})

test_that("printed rule titles should be correct length", {
  rules <- get_rule_names(model_type = "all")
  partable <- lavaan::lavaanify("y ~ x", warn = FALSE)
  test <- capture.output(id(partable))
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