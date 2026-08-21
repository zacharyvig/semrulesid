source(testthat::test_path("..", "test_models.R"))

make_partable <- function(model) {
  lavaan::lavaanify(
    model$model,
    warn = FALSE,
    auto = TRUE,
    model_type = model$type
  )
}

test_that("classify_model identifies reg, cfa, sem, and mlm parts", {
  expect_identical(
    classify_model(make_partable(test_models$reg_pass)),
    "reg"
  )
  expect_identical(
    classify_model(make_partable(test_models$cfa_three_pass)),
    "cfa"
  )
  expect_identical(
    classify_model(make_partable(test_models$sem_scaling_pass)),
    "sem"
  )
  expect_error(
    classify_model(make_partable(test_models$mlm)),
    "This model type is not currently supported"
  )
  expect_error(
    classify_model(make_partable(test_models$categorical)),
    "This model type is not currently supported"
  )
})

test_that("get_lavaan_cmd returns the correct cmd", {
  fit <- lavaan::lavaan(
    test_models$sem_scaling_pass$model,
    model.type = test_models$sem_scaling_pass$type
  )
  expect_identical(get_lavaan_cmd(fit), "lavaan")
  fit_sem <- lavaan::sem(
    test_models$sem_scaling_pass$model,
    model.type = test_models$sem_scaling_pass$type
  )
  expect_identical(get_lavaan_cmd(fit_sem), "sem")
  fit_cfa <- lavaan::cfa(
    test_models$cfa_three_pass$model,
    model.type = test_models$cfa_three_pass$type
  )
  expect_identical(get_lavaan_cmd(fit_cfa), "cfa")
})

test_that("get_model_type_name returns the correct model type name", {
  expect_identical(get_model_type_name("reg"), "Simultaneous Equations Model")
  expect_identical(get_model_type_name("cfa"), "Confirmatory Factor Analysis Model")
  expect_identical(get_model_type_name("sem"), "General Structural Equation Model")
  expect_identical(get_model_type_name("reg", long = FALSE, capitalize = FALSE), "simultaneous equations model")
  expect_identical(get_model_type_name("cfa", long = FALSE), "CFA Model")
  expect_identical(get_model_type_name("sem", long = FALSE, capitalize = FALSE), "general SEM")
  expect_identical(get_model_type_name("reg", long = TRUE, plural = TRUE), "Simultaneous Equations Models")
})