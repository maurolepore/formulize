context("test basic functionality")

library(glmnet)

form <- as.formula(mpg ~ drat * hp - 1)
X <- model.matrix(form, mtcars)
y <- mtcars$mpg

test_that("get_data_design works", {

  # warns on intercept only model
  expect_warning(get_data_design(mpg ~ 1, mtcars))

  design <- get_data_design(form, mtcars)

  expect_equal(design$X, X)
  expect_equivalent(design$y, y)
  expect_false(design$intercept)
  expect_true(get_data_design(mpg ~ drat, mtcars)$intercept)
})

test_that("formulize and predict.wrapped work", {

  glmnet_form <- formulize(cv.glmnet)

  # interface agreement test
  set.seed(27)
  frm_mod <- glmnet_form(form, mtcars)

  set.seed(27)
  mat_mod <- cv.glmnet(X, y)

  expect_equal(coef(frm_mod), coef(mat_mod))

  frm_preds <- predict(frm_mod, head(mtcars))
  mat_preds <- predict(mat_mod, head(X))

  expect_equal(frm_preds, mat_preds)
})

test_that("genericize works", {

  expect_error(genericize(mean))
  expect_error(genericize(glmnet::cv.glmnet))

  expect_false(utils::isS3stdGeneric(cv.glmnet))

  genericize(cv.glmnet)  # call for side effects

  # expect_true(utils::isS3stdGeneric(cv.glmnet))

  expect_equal(cv.glmnet.formula, formulize(cv.glmnet))
  expect_true(is.function(cv.glmnet.default))

  #  repeat interface agreement test
  set.seed(27)
  frm_mod <- cv.glmnet(form, mtcars)

  set.seed(27)
  mat_mod <- cv.glmnet(X, y)

  expect_equal(coef(frm_mod), coef(mat_mod))

  frm_preds <- predict(frm_mod, head(mtcars))
  mat_preds <- predict(mat_mod, head(X))

  expect_equal(frm_preds, mat_preds)

  rm(list = ls())

  # check that fname can be set
  genericize(glmnet, "gnet")  # call for side effects

  expect_true(utils::isS3stdGeneric(gnet))
  expect_equal(gnet.formula, formulize(glmnet))
  expect_true(is.function(gnet.default))

  #  repeat interface agreement test
  set.seed(27)
  frm_mod <- gnet(form, mtcars)

  set.seed(27)
  mat_mod <- gnet(X, y)

  frm_preds <- predict(frm_mod, head(mtcars))
  mat_preds <- predict(mat_mod, head(X))

  expect_equal(frm_preds, mat_preds)
})
