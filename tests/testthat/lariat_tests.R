# library(lariat)
#
# iris2 <- iris
# iris2$Species <- as.character(iris$Species)
#
# expect_cv_interfaces_agree <- function(form, data, intercept, seed) {
#
#   X <- model.matrix(form, data)
#   y <- model.response(model.frame(form, data))
#
#   X <- if (intercept) X[, !(colnames(X) %in% "(Intercept)"), drop = FALSE] else X
#
#   set.seed(seed)
#   lcv_frm <- lariat_cv(form, data)
#
#   set.seed(seed)  # make sure both interfaces get the same cv folds
#   lcv_mat <- lariat_cv(X, y, intercept = intercept)
#
#   frm_preds <- predict(lcv_frm, inclusion = 0.25)
#   mat_preds <- predict(lcv_mat, inclusion = 0.25)
#
#   frm_preds_new <- predict(lcv_frm, inclusion = 0.25, newdata = data[1:5, ])
#   mat_preds_new <- predict(lcv_mat, inclusion = 0.25, newdata = X[1:5, ])
#
#   frm_coefs <- coef(lcv_frm, inclusion = 0.25)
#   mat_coefs <- coef(lcv_mat, inclusion = 0.25)
#
#   lfit_frm <- extract_fit(lcv_frm)
#   lfit_mat <- extract_fit(lcv_mat)
#
#   expect_equivalent(has_intercept(lfit_frm), intercept)
#   expect_equal(has_intercept(lfit_frm), has_intercept(lfit_mat))
#   expect_equal(frm_preds, mat_preds)
#   expect_equal(frm_coefs, mat_coefs)
#   expect_equal(frm_preds[1:5], as.vector(frm_preds_new))
#   expect_equal(mat_preds[1:5], as.vector(mat_preds_new))
#   expect_equal(frm_preds_new, mat_preds_new)
# }
#
# expect_interfaces_agree <- function(form, data, intercept) {
#
#   X <- model.matrix(form, data)
#   y <- model.response(model.frame(form, data))
#
#   X <- if (intercept) X[, !(colnames(X) %in% "(Intercept)"), drop = FALSE] else X
#
#   lfit_frm <- lariat(form, data)
#   lfit_mat <- lariat(X, y, intercept = intercept)
#
#   frm_preds <- predict(lfit_frm, inclusion = 0.25)
#   mat_preds <- predict(lfit_mat, inclusion = 0.25)
#
#   frm_preds_new <- predict(lfit_frm, inclusion = 0.25, newdata = data[1:5, ])
#   mat_preds_new <- predict(lfit_mat, inclusion = 0.25, newdata = X[1:5, ])
#
#   frm_coefs <- coef(lfit_frm, inclusion = 0.25)
#   mat_coefs <- coef(lfit_mat, inclusion = 0.25)
#
#   expect_equivalent(has_intercept(lfit_frm), intercept)
#   expect_equal(has_intercept(lfit_frm), has_intercept(lfit_mat))
#   expect_equal(frm_preds, mat_preds)
#   expect_equal(frm_coefs, mat_coefs)
#   expect_equal(frm_preds[1:5], as.vector(frm_preds_new))
#   expect_equal(mat_preds[1:5], as.vector(mat_preds_new))
#   expect_equal(frm_preds_new, mat_preds_new)
# }
#
# context("lariat.formula and lariat_cv.formula work as expected")
#
# test_that("lariat_cv.formula errors out on factor predictor", {
#   expect_error(lariat_cv(Sepal.Length ~ Species, data = iris))
# })
#
# test_that("lariat_cv.formula errors out on character predictor", {
#   expect_error(lariat_cv(Sepal.Length ~ Species, data = iris2))
# })
#
# test_that("lariat_cv.formula errors out when 'intercept' passed as an argument", {
#   # also be sure to catch partial matches!
#   expect_error(lfit <- lariat_cv(mpg ~ disp + hp + drat, mtcars, int = TRUE))
#   expect_error(lfit <- lariat_cv(mpg ~ disp + hp + drat, mtcars, intercept = TRUE))
# })
#
# test_that("deals with type conversion in formula gracefully", {
#
#   form <- mpg ~ as.factor(hp)
#   form2 <- mpg ~ as.character(hp)
#
#   expect_error(lfit <- lariat_cv(form, mtcars))
#   expect_error(lfit <- lariat_cv(form2, mtcars))
#
#   # TODO: uncomment following once categorical predictors implemented
#   # if type conversation in the formula is a pain to make work, should
#   # give an informative error message at least
#
#   # expect_interfaces_agree(form, mtcars, intercept = TRUE)
#   # expect_cv_interfaces_agree(form, mtcars, intercept = TRUE)
#   #
#   # expect_interfaces_agree(form2, mtcars, intercept = TRUE)
#   # expect_cv_interfaces_agree(form2, mtcars, intercept = TRUE)
# })
#
# test_that("lariat.default catches issues with penalty_weights", {
#   # need to specify a penalty weight for each column except the intercept
#   expect_silent(lariat(mpg ~ hp * drat, mtcars, penalty_weights = 1:3))
#   expect_error(lariat(mpg ~ hp * drat, mtcars, penalty_weights = 1:2))
#   expect_error(lariat(mpg ~ hp * drat, mtcars, penalty_weights = 1:4))
# })
#
# test_that("lariat_cv.formula works without intercept", {
#   form <- mpg ~ disp + hp + drat - 1
#   expect_interfaces_agree(form, mtcars, intercept = FALSE)
#   expect_cv_interfaces_agree(form, mtcars, intercept = FALSE, seed = 24)
#
#   form <- mpg ~ disp + hp + drat + 0
#   expect_interfaces_agree(form, mtcars, intercept = FALSE)
#   expect_cv_interfaces_agree(form, mtcars, intercept = FALSE, seed = 25)
# })
#
# test_that("lariat_cv.formula works with intercept", {
#   form <- mpg ~ disp + hp + drat
#   expect_interfaces_agree(form, mtcars, intercept = TRUE)
#   expect_cv_interfaces_agree(form, mtcars, intercept = TRUE, seed = 26)
#
#   form <- mpg ~ disp + hp + drat +  1
#   expect_interfaces_agree(form, mtcars, intercept = TRUE)
#   expect_cv_interfaces_agree(form, mtcars, intercept = TRUE, seed = 267)
#
#   # error out on intercept only model
#   expect_error(lariat(mpg ~ 1, mtcars))
# })
#
# test_that("lariat_cv.formula works with polynomials", {
#   form <- mpg ~ disp + hp + poly(drat, 2)
#   expect_interfaces_agree(form, mtcars, intercept = TRUE)
#   expect_cv_interfaces_agree(form, mtcars, intercept = TRUE, seed = 28)
# })
#
# test_that("lariat_cv.formula works with natural splines", {
#   form <- mpg ~ disp + hp + splines::ns(drat, 2)
#   expect_interfaces_agree(form, mtcars, intercept = TRUE)
#   expect_cv_interfaces_agree(form, mtcars, intercept = TRUE, seed = 28)
# })
#
# test_that("lariat_cv.formula works with interactions", {
#   form <- mpg ~ drat + hp + drat:hp
#   expect_interfaces_agree(form, mtcars, intercept = TRUE)
#   expect_cv_interfaces_agree(form, mtcars, intercept = TRUE, seed = 29)
#
#   des <- lariat(form, mtcars)$X
#   expected <- scale(with(mtcars, cbind(drat, hp, drat * hp)))
#   expect_equivalent(des, expected)
# })
#
# test_that("lariat_cv.formula works with 'y ~ .' notation", {
#   form <- mpg ~ .
#   expect_interfaces_agree(form, mtcars, intercept = TRUE)
#   expect_cv_interfaces_agree(form, mtcars, intercept = TRUE, seed = 30)
#
#   des <- lariat(form, mtcars)$X
#   expected <- mtcars %>% select(-mpg) %>% as.matrix() %>% scale()
#   expect_equivalent(des, expected)
# })
#
# test_that("lariat_cv.formula works with star notation", {
#   form <- mpg ~ hp * drat
#   expect_interfaces_agree(form, mtcars, intercept = TRUE)
#   expect_cv_interfaces_agree(form, mtcars, intercept = TRUE, seed = 31)
#
#   des <- lariat(form, mtcars)$X
#   expected <- scale(with(mtcars, cbind(hp, drat, hp * drat)))
#   expect_equivalent(des, expected)
# })
#
# test_that("lariat_cv.formula works with I()", {
#   form <- mpg ~ hp + I(drat^2)
#   expect_interfaces_agree(form, mtcars, intercept = TRUE)
#   expect_cv_interfaces_agree(form, mtcars, intercept = TRUE, seed = 32)
#
#   des <- lariat(form, mtcars)$X
#   expected <- scale(with(mtcars, cbind(hp, drat^2)))
#   expect_equivalent(des, expected)
# })
#
# test_that("lariat_cv.formula works with transformed response", {
#   form <- log(mpg) ~ hp + drat
#   expect_interfaces_agree(form, mtcars, intercept = TRUE)
#   expect_cv_interfaces_agree(form, mtcars, intercept = TRUE, seed = 33)
#
#   resp <- lariat(form, mtcars)$y
#   expect_equivalent(resp, log(mtcars$mpg))
# })
#
# test_that("Prediction works correctly with calculated bases", {
#   # Example from ?lariat_formula
#
#   library(splines)
#   # lm() + splines predicts correctly
#   lm_fit <- lm(mpg ~ ns(hp, 2), data=mtcars)
#
#   expect_equal(predict(lm_fit, newdata=head(mtcars)),
#                head(predict(lm_fit, newdata=mtcars)))
#
#   # So does lariat() + splines
#   lariat_fit <- lariat(mpg ~ ns(hp, 2), data=mtcars)
#
#   expect_equal(predict(lariat_fit, newdata=head(mtcars), iter=1),
#                head(predict(lariat_fit, newdata=mtcars, iter=1)))
# })
#
# rm(iris2)
