test_that("id_mplus converts Mplus syntax strings", {
  model <- paste(
    "f1 BY x1*1 x2 x3;",
    "x1 WITH x2;",
    "x3 (1);",
    "x2 (1);",
    sep = "\n"
  )

  out <- id_mplus(model)
  expected <- id(lavaan::lav_mplus_syntax_model(model))

  expect_s3_class(out, "semid")
  expect_identical(out$model_type, expected$model_type)
  expect_identical(out$partable, expected$partable)
})

test_that("id accepts lavaan syntax strings containing inp in the name", {
  model <- paste(
    "latentinp =~ x1 + x2",
    "x1 ~~ x1",
    "x2 ~~ x2",
    sep = "\n"
  )

  expect_s3_class(id(model), "semid")
})

test_that("id_mplus reads Mplus input files", {
  model <- paste(
    "f1 BY x1*1 x2 x3;",
    sep = "\n"
  )
  tmpdir <- tempfile("id-mplus-")
  dir.create(tmpdir)
  data_path <- file.path(tmpdir, "data.dat")
  inp <- file.path(tmpdir, "model.inp")
  set.seed(1)
  factor_score <- rnorm(100)
  write.table(
    data.frame(
      x1 = factor_score + rnorm(100, sd = 0.2),
      x2 = factor_score + rnorm(100, sd = 0.2),
      x3 = factor_score + rnorm(100, sd = 0.2)
    ),
    file = data_path,
    row.names = FALSE,
    col.names = FALSE
  )
  writeLines(
    c(
      "TITLE: id_mplus test",
      "DATA:",
      "  FILE IS data.dat;",
      "VARIABLE:",
      "  NAMES ARE x1 x2 x3;",
      "MODEL:",
      "  f1 BY x1*1 x2 x3;"
    ),
    inp
  )

  out <- id_mplus(inp)
  expected <- id_mplus(model)

  expect_s3_class(out, "semid")
  expect_identical(out$model_type, expected$model_type)
  expect_true(nrow(out$partable) > nrow(expected$partable))
  expect_true(any(out$partable$lhs == "f1" & out$partable$op == "=~"))
})