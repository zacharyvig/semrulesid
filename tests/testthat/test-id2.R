source(testthat::test_path("..", "test_models.R"))

make_partable <- function(model) {
  lavaan::lavaanify(
    model$model,
    warn = FALSE,
    auto = TRUE,
    model.type = model$type
  )
}

test_that("id2 returns the two-step object structure", {
  out <- id(
    make_partable(list(
      type = "sem",
      model = "L1 =~ Y1 + Y2 + Y3\nL2 =~ Y4 + Y5 + Y6\nL2 ~ L1"
    )),
    twostep = TRUE,
    lav_fun = NA
  )

  expect_s3_class(out, "semid2")
  expect_named(out, c("id.model.type", "id.cfa", "id.reg", "partable", "lav_fun", "print.options"))
  expect_s3_class(out$id.cfa, "semid")
  expect_s3_class(out$id.reg, "semid")
})

test_that("id2 printing shows both steps", {
  out <- capture.output(
    print(
      id2(
        make_partable(list(
          type = "sem",
          model = "L1 =~ Y1 + Y2 + Y3\nL2 =~ Y4 + Y5 + Y6\nL2 ~ L1"
        )),
        lav_fun = NA
      )
    )
  )

  expect_true(any(grepl("Two-Step Rule Check", out)))
  expect_true(any(grepl("Step 1: Measurement Model", out)))
  expect_true(any(grepl("Step 2: Latent Variable/Structural Model", out)))
})