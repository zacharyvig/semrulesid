source(testthat::test_path("..", "test_models.R"))

make_partable <- function(model) {
  lavaan::lavaanify(
    model$model,
    warn = FALSE,
    auto = TRUE,
    model.type = model$type
  )
}

test_that("scaling returns logical and object outputs", {
  logical_out <- scaling(
    make_partable(test_models$sem_scaling_pass),
    lv = "L1",
    return.type = "logical"
  )
  object_out <- scaling(
    make_partable(test_models$sem_scaling_pass),
    lv = "L1",
    return.type = "object"
  )

  expect_identical(logical_out, c(L1 = TRUE))
  expect_s3_class(object_out, "semscale")
  expect_true(object_out$Scaling[[1]]$scaled)
  expect_identical(object_out$Scaling[[1]]$lv, "L1")
  expect_false(object_out$Scaling[[1]]$mean.structure)
})

test_that("scaling printing shows mean structure and the revised messages", {
  pass_out <- capture.output(
    print(
      scaling(make_partable(test_models$sem_scaling_pass), lv = "L1"),
      include.msgs = TRUE,
      window = 120
    )
  )
  mean_out <- capture.output(
    print(
      scaling(make_partable(test_models$sem_scaling_mean_pass), lv = "f1"),
      include.msgs = TRUE,
      window = 120
    )
  )
  fail_out <- capture.output(
    print(
      scaling(make_partable(test_models$sem_scaling_fail), lv = "L1"),
      include.msgs = TRUE,
      window = 120
    )
  )

  expect_true(any(grepl("Latent Variable Scaling", pass_out)))
  expect_true(any(grepl("LV is scaled\\?\\s*Yes", pass_out)))
  expect_true(any(grepl("Mean structure\\?\\s*No", pass_out)))
  expect_true(any(grepl("Scaling method(s):", pass_out, fixed = TRUE)))
  expect_true(any(grepl("L1", pass_out)))


  expect_true(any(grepl("LV is scaled\\?\\s*Yes", mean_out)))
  expect_true(any(grepl("Mean structure\\?\\s*Yes", mean_out)))
  expect_true(any(grepl("Scaling method(s):", mean_out, fixed = TRUE)))

  expect_true(any(grepl("Latent Variable Scaling", fail_out)))
  expect_true(any(grepl("Scaling error", fail_out)))
  expect_true(any(grepl("Neither scaling indicator nor fixed latent variance", fail_out, fixed = TRUE)))
})

test_that("scaling printing handles edge cases", {
  two_scaling_out <- capture.output(
    print(
      scaling(make_partable(test_models$sem_two_scaling_ind), lv = "L1"),
      include.msgs = TRUE,
      window = 120
    )
  )
  zero_loading_out <- capture.output(
    print(
      scaling(make_partable(test_models$sem_zero_loading), lv = "L1"),
      include.msgs = TRUE,
      window = 120
    )
  )
  negative_loading_out <- capture.output(
    print(
      scaling(make_partable(test_models$sem_negative_loading), lv = "L1"),
      include.msgs = TRUE,
      window = 120
    )
  )

  expect_true(any(grepl("Latent Variable Scaling", two_scaling_out)))
  expect_true(any(grepl("Scaling indicator\\(s\\)\\:\\s*Y1, Y2", two_scaling_out)))

  expect_true(any(grepl("Latent Variable Scaling", zero_loading_out)))
  expect_true(any(grepl("Scaling indicator\\(s\\)\\:\\s*None", zero_loading_out)))

  expect_true(any(grepl("Latent Variable Scaling", negative_loading_out)))
  expect_true(any(grepl("Scaling indicator\\(s\\)\\:\\s*Y1", negative_loading_out)))
})