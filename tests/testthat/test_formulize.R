library(formulize)

context("dummy tests until I write better ones")

expect_silent("works on glmnet", {

  library(glmnet)
  genericize(cv.glmnet)

  form <- mpg ~ hp + drat - 1
  dat <- mtcars

  X <- model.matrix(form, dat)
  y <- model.response(model.frame(form, dat))

  frm_mod <- cv.glmnet(form, dat)
  mat_mod <- cv.glmnet(X, y)

  coef(frm_mod)
  coef(mat_mod)
})

#
#
# library(glmnet)
# genericize(cv.glmnet)
# glmnet_mod <- cv.glmnet(form, dat)
# predict(glmnet_mod, dat)
#
#
# #
# # mcp.matrix <- wrap(glmnet::cv.glmnet)
# # mcp.formula <- formulize(glmnet::cv.glmnet)  # but mcp.formula depends on
# #
# # mcp <- genericize(glmnet::cv.glmnet)
# #
# # fnet <- formulize(glmnet::cv.glmnet)
# #
# # ridge <- fnet(form, dat, alpha = 1)
# # predict(ridge, dat)
# #
# # # option 2:
# #
# # mcp_model <- mcp(form, dat)
# # predict(mcp_model, dat)
