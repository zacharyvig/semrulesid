source(testthat::test_path("..", "test_models.R"))

make_partable <- function(model) {
  lavaan::lavaanify(
    model$model,
    warn = FALSE,
    auto = TRUE,
    model_type = model$type
  )
}

test_that("regression rules identify recursive and non-recursive models", {
  rules <- get_rules(model_type = "reg")
  for (model_name in c("reg_pass", "reg_corr_err_pass", "reg_feedback_fail")) {
    partable <- make_partable(test_models[[model_name]])
    expected <- switch(
      model_name,
      reg_pass = list(
        rule_reg_null_byy = list(pass = TRUE, cond = "S"),
        rule_reg_fully_recursive = list(pass = TRUE, cond = "S"),
        rule_reg_recursive_corr_err = list(pass = TRUE, cond = "S")
      ),
      reg_corr_err_pass = list(
        rule_reg_null_byy = list(pass = FALSE, cond = "S"),
        rule_reg_fully_recursive = list(pass = FALSE, cond = "S"),
        rule_reg_recursive_corr_err = list(pass = TRUE, cond = "S")
      ),
      reg_feedback_fail = list(
        rule_reg_null_byy = list(pass = FALSE, cond = "S"),
        rule_reg_fully_recursive = list(pass = FALSE, cond = "S"),
        rule_reg_recursive_corr_err = list(pass = FALSE, cond = "S")
      )
    )
    for (rule_name in names(expected)) {
      out <- rules[[rule_name]](partable)
      expect_identical(out$pass, expected[[rule_name]]$pass, label = paste(model_name, rule_name))
      expect_identical(out$cond, expected[[rule_name]]$cond, label = paste(model_name, rule_name))
    }
  }
})

test_that("cfa rules distinguish two- and three-indicator models", {
  rules <- get_rules(model_type = "cfa")
  expectations <- list(
    cfa_two_fail = list(
      rule_cfa_two_indicator = list(pass = FALSE, cond = "S"),
      rule_cfa_three_indicator = list(pass = NA, cond = NA_character_)
    ),
    cfa_two_pass_three_fail = list(
      rule_cfa_two_indicator = list(pass = TRUE, cond = "S"),
      rule_cfa_three_indicator = list(pass = NA, cond = NA_character_)
    ),
    cfa_three_pass = list(
      rule_cfa_two_indicator = list(pass = TRUE, cond = "S"),
      rule_cfa_three_indicator = list(pass = TRUE, cond = "S")
    ),
    cfa_three_fail = list(
      rule_cfa_two_indicator = list(pass = FALSE, cond = "S"),
      rule_cfa_three_indicator = list(pass = FALSE, cond = "S")
    ),
    cfa_higher_order_pass = list(
      rule_cfa_two_indicator = list(pass = FALSE, cond = NA_character_),
      rule_cfa_three_indicator = list(pass = TRUE, cond = NA_character_)
    )
  )
  for (model_name in names(expectations)) {
    partable <- make_partable(test_models[[model_name]])
    for (rule_name in names(expectations[[model_name]])) {
      out <- rules[[rule_name]](partable)
      expect_identical(out$pass, expectations[[model_name]][[rule_name]]$pass, label = paste(model_name, rule_name))
      expect_identical(out$cond, expectations[[model_name]][[rule_name]]$cond, label = paste(model_name, rule_name))
    }
  }
})

test_that("sem rules handle scaling, emitted paths, and causal indicators", {
  rules <- get_rules(model_type = "sem")
  expectations <- list(
    sem_two_emitted_paths_fail = list(
      rule_sem_ntheta = list(pass = FALSE, cond = "N"),
      rule_sem_latent_scaling = list(pass = FALSE, cond = "N"),
      rule_sem_two_emitted_paths = list(pass = FALSE, cond = "N"),
      rule_sem_exogenous_x = list(pass = FALSE, cond = "S")
    ),
    sem_two_emitted_paths_pass = list(
      rule_sem_ntheta = list(pass = TRUE, cond = "N"),
      rule_sem_latent_scaling = list(pass = FALSE, cond = "N"),
      rule_sem_two_emitted_paths = list(pass = TRUE, cond = "N"),
      rule_sem_exogenous_x = list(pass = TRUE, cond = "S")
    ),
    sem_scaling_pass = list(
      rule_sem_ntheta = list(pass = TRUE, cond = "N"),
      rule_sem_latent_scaling = list(pass = TRUE, cond = "N"),
      rule_sem_two_emitted_paths = list(pass = NA, cond = NA_character_),
      rule_sem_exogenous_x = list(pass = TRUE, cond = "S")
    ),
    sem_scaling_fail = list(
      rule_sem_ntheta = list(pass = FALSE, cond = "N"),
      rule_sem_latent_scaling = list(pass = FALSE, cond = "N"),
      rule_sem_two_emitted_paths = list(pass = TRUE, cond = "N"),
      rule_sem_exogenous_x = list(pass = NA, cond = NA_character_)
    )
  )
  for (model_name in names(expectations)) {
    partable <- make_partable(test_models[[model_name]])
    for (rule_name in names(expectations[[model_name]])) {
      out <- rules[[rule_name]](partable)
      expect_identical(out$pass, expectations[[model_name]][[rule_name]]$pass, label = paste(model_name, rule_name))
      expect_identical(out$cond, expectations[[model_name]][[rule_name]]$cond, label = paste(model_name, rule_name))
    }
  }
})
