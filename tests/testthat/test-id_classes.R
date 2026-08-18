test_that("id() runs with lavaan syntax", {
  model <- paste(
    "f1 =~ x1 + x2 + x3",
    "x1 ~~ x1",
    "x2 ~~ x2",
    "x3 ~~ x3",
    sep = "\n"
  )

  expect_no_error(
    out <- id(model)
  )

  expect_s3_class(out, "semid")
})

test_that("id() runs with a lavaan parameter table", {
  model <- paste(
    "f1 =~ x1 + x2 + x3",
    "x1 ~~ x1",
    "x2 ~~ x2",
    "x3 ~~ x3",
    sep = "\n"
  )

  partable <- lavaan::lavaanify(model, auto = TRUE)

  expect_no_error(
    out <- id(partable, lav_fun = NA)
  )

  expect_no_warning(
    out <- id(partable, lav_fun = NA)
  )

  expect_s3_class(out, "semid")
})

test_that("id() runs with a fitted lavaan object", {
  model <- '
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem65 =~ y5 + y6 + y7 + y8

    dem60 ~ ind60
    dem65 ~ ind60 + dem60

    y1 ~~ y5
    y2 ~~ y4 + y6
    y3 ~~ y7
    y4 ~~ y8
    y6 ~~ y8
  '

  fit <- lavaan::sem(
    model,
    data = lavaan::PoliticalDemocracy
  )

  expect_no_error(
    out <- id(fit, lav_fun = "sem")
  )

  expect_s3_class(out, "semid")
})

test_that("id() and scaling() piping produces enriched semid objects", {
  model <- paste(
    "f1 =~ x1 + x2 + x3",
    "x1 ~~ x1",
    "x2 ~~ x2",
    "x3 ~~ x3",
    sep = "\n"
  )

  out1 <- id(model) |> scaling()
  out2 <- scaling(model) |> id()

  expect_s3_class(out1, "semid")
  expect_s3_class(out2, "semid")

  expect_s3_class(out1$scaling, "semscale")
  expect_s3_class(out2$scaling, "semscale")

  expect_equal(out1$Rules, out2$Rules)
  expect_equal(out1$partable, out2$partable)

  expect_equal(
    out1$scaling$Scaling,
    out2$scaling$Scaling
  )
})