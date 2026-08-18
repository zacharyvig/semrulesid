test_that("id() runs with lavaan syntax", {
  model <- paste(
    "f1 =~ x1 + x2 + x3",
    "x1 ~~ x1",
    "x2 ~~ x2",
    "x3 ~~ x3",
    sep = "\n"
  )
  expect_no_error(id(model))
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
  expect_no_error(id(partable, lav_fun = NA))
})

test_that("id() runs with a fitted lavaan object", {
  model <- '
   # latent variable definitions
     ind60 =~ x1 + x2 + x3
     dem60 =~ y1 + y2 + y3 + y4
     dem65 =~ y5 + y6 + y7 + y8
   # regressions
     dem60 ~ ind60
     dem65 ~ ind60 + dem60
   # residual covariances
     y1 ~~ y5
     y2 ~~ y4 + y6
     y3 ~~ y7
     y4 ~~ y8
     y6 ~~ y8
    '
  fit <- lavaan::sem(model, data = lavaan::PoliticalDemocracy)
  expect_no_error(id(fit))
})