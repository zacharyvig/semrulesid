source(testthat::test_path("..", "test_models.R"))

make_partable <- function(model) {
  lavaan::lavaanify(
    model$model,
    warn = FALSE,
    auto = TRUE,
    model_type = model$type
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
  expect_named(out, c("id_model_type", "id_cfa", "id_reg", "partable", "lav_fun", "print_options"))
  expect_s3_class(out$id_cfa, "semid")
  expect_s3_class(out$id_reg, "semid")
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

test_that("id2() creates the expected CFA and simultaneous-equations partables", {
  model <- '
    L1 =~ Y1 + Y2 + Y3
    L2 =~ Y4 + Y5 + Y6

    L2 ~ L1
  '

  expect_no_error(
    out <- id2(model, lav_fun = "sem")
  )

  expect_s3_class(out, "semid2")
  expect_s3_class(out$id_cfa, "semid")
  expect_s3_class(out$id_reg, "semid")

  cfa_pt <- out$id_cfa$partable
  reg_pt <- out$id_reg$partable

  # Step 1: measurement relations remain unchanged.
  expect_true(any(
    cfa_pt$lhs == "L1" &
      cfa_pt$op == "=~" &
      cfa_pt$rhs == "Y1"
  ))

  expect_true(any(
    cfa_pt$lhs == "L2" &
      cfa_pt$op == "=~" &
      cfa_pt$rhs == "Y6"
  ))

  # Step 1: the latent structural path becomes a covariance.
  expect_false(any(
    cfa_pt$lhs == "L2" &
      cfa_pt$op == "~" &
      cfa_pt$rhs == "L1"
  ))

  expect_true(any(
    cfa_pt$op == "~~" &
      (
        (cfa_pt$lhs == "L2" & cfa_pt$rhs == "L1") |
          (cfa_pt$lhs == "L1" & cfa_pt$rhs == "L2")
      )
  ))

  # Step 2: the latent structural path remains a regression.
  expect_true(any(
    reg_pt$lhs == "L2" &
      reg_pt$op == "~" &
      reg_pt$rhs == "L1"
  ))

  # Step 2: observed measurement-indicator loadings are removed.
  expect_false(any(
    reg_pt$op == "=~" &
      reg_pt$rhs %in% c("Y1", "Y2", "Y3", "Y4", "Y5", "Y6")
  ))

  # Confirm the transformed components carry their intended classifications.
  expect_identical(out$id_cfa$id_model_type, "cfa")
  expect_identical(out$id_reg$id_model_type, "reg")
})

test_that("id2() rejects non-SEM models", {
  cfa_model <- '
    L1 =~ Y1 + Y2 + Y3
    L2 =~ Y4 + Y5 + Y6
    L1 ~~ L2
  '

  reg_model <- '
    Y1 ~ X1 + X2
    Y2 ~ Y1 + X3
  '

  expect_error(
    id2(cfa_model, lav_fun = "cfa"),
    "only applicable to full SEMs"
  )

  expect_error(
    id2(reg_model, lav_fun = "sem"),
    "only applicable to full SEMs"
  )
})