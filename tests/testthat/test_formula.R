context("formula mappings")

library(glmnet)

expect_interfaces_agree <- function(f, form, data, seed) {

  wrapped <- formulize(f)

  # manually create the data or interest
  X <- model.matrix(form, data)
  y <- model.response(model.frame(form, data))

  # check that formulized
  set.seed(seed)
  wrap_mod <- wrapped(form, data)

  set.seed(seed)
  orig_mod <- f(X, y)

  # test that wrapped models get wrapped class
  expect_true(inherits(wrap_mod, "wrapped"))
  expect_false(inherits(orig_mod, "wrapped"))

  # wrapped models have store info to map dataframes to matrices
  expect_false(is.null(wrap_mod$design))

  # otherwise equivalent
  wrap_same <- wrap_mod
  orig_same <- orig_mod

  # TODO: nested call statements
  wrap_same$call <- NULL
  orig_same$call <- NULL

  wrap_same$design <- NULL
  expect_equivalent(wrap_same, orig_same)

  # predictions are the same on the training data
  wrap_preds <- predict(wrap_mod, data)
  orig_preds <- predict(orig_mod, X)

  expect_equal(wrap_preds, orig_preds)

  # predictions on subset are the same on training data
  # this is important for splines, etc
  wrap_preds_head <- predict(wrap_mod, head(data))
  orig_preds_head <- predict(orig_mod, head(X))

  # interface equivalence
  expect_equal(wrap_preds_head, orig_preds_head)

  # spline sanity
  expect_equal(head(wrap_preds), wrap_preds_head)
  expect_equal(head(orig_preds), orig_preds_head)
}

test_that("as.factor, as.character, intercept only error out", {
  wrapped <- formulize(glmnet)
  expect_error(wrapped(mpg ~ as.factor(hp), mtcars),
               "Formulize does not permit type conversion in formulas.")

  expect_error(wrapped(mpg ~ as.character(hp), mtcars),
               "Formulize does not permit type conversion in formulas.")

  expect_error(wrapped(mpg ~ 1, mtcars),
               "Formulize does not permit intercept only models.")
})

test_that("without intercept", {
  form <- mpg ~ disp + hp + drat - 1
  expect_interfaces_agree(glmnet, form, mtcars, seed = 27)
})

test_that("with intercept", {
  form <- mpg ~ disp + hp + drat
  expect_interfaces_agree(glmnet, form, mtcars, seed = 27)

  form <- mpg ~ disp + hp + drat +  1
  expect_interfaces_agree(glmnet, form, mtcars, seed = 27)
})

test_that("polynomials", {
  form <- mpg ~ disp + hp + poly(drat, 2)
  expect_interfaces_agree(glmnet, form, mtcars, seed = 27)
})

test_that("natural splines", {
  library(splines)
  form <- mpg ~ disp + hp + ns(drat, 2)
  expect_interfaces_agree(glmnet, form, mtcars, seed = 27)

  # but this doesn't work??
  # form <- mpg ~ disp + hp + splines::ns(drat, 2)
  # expect_interfaces_agree(glmnet, form, mtcars, seed = 27)
})

test_that("interactions", {
  form <- mpg ~ drat + hp + drat:hp
  expect_interfaces_agree(glmnet, form, mtcars, seed = 27)
})

test_that(" works with 'y ~ .' notation", {
  form <- mpg ~ .
  expect_interfaces_agree(glmnet, form, mtcars, seed = 27)
})

test_that("star notation", {
  form <- mpg ~ hp * drat
  expect_interfaces_agree(glmnet, form, mtcars, seed = 27)
})

test_that("I()", {
  form <- mpg ~ hp + I(drat^2)
  expect_interfaces_agree(glmnet, form, mtcars, seed = 27)
})

test_that("transformed response", {
  form <- log(mpg) ~ hp + drat
  expect_interfaces_agree(glmnet, form, mtcars, seed = 27)
})
