get_design <- function(design, data, ...) {
  UseMethod("get_design")
}

get_design.formula <- function(formula, data, ...) {

  y <- stats::model.response(stats::model.frame(formula, data))
  X <- stats::model.matrix(formula, data)

  assign <- attr(X, "assign")
  intercept <- 0 %in% assign        # check if formula included intercept term

  if (intercept && length(assign) == 1) {
    warning("Intercept only models may break things.")
  }

  list(X = X, y = y, intercept = intercept)
}

#' @importFrom recipes all_predictors all_outcomes
get_design.recipe <- function(recipe, data, ...) {

  prepped <- recipes::prep(recipe)
  X <- recipes::bake(prepped, data, all_predictors())
  y <- recipes::bake(prepped, data, all_outcomes())

  X <- as.matrix(X)
  y <- as.matrix(y)
  y <- if (ncol(y) == 1) as.vector(y) else y

  # TODO: update to detect_step after PR gets merged
  intercept <- "step_intercept" %in% unlist(lapply(prepped$steps, class))

  list(X = X, y = y, intercept = intercept)
}

#' Predict on new data form a \code{wrapped} object
#'
#' @param object Object of class \code{wrapped} with a subclass with a
#'   valid \code{predict} method.
#' @param newdata Data to predict on.
#' @param ... Additional arguments to pass to the subclass predict method.
#'
#' @return Predictions from subclass predict method.
#' @export
#' @importFrom recipes all_predictors
predict.wrapped <- function(object, newdata = NULL, ...) {

  des <- object$design

  if (inherits(des, "formula")) {
    newdata <- stats::model.matrix(des, newdata)
  } else {
    newdata <- as.matrix(recipes::bake(des, newdata, all_predictors()))
  }
  NextMethod()
}

#' Create a formula/recipe interface to a modelling method
#'
#' @param f A function with a matrix/vector interface. Assumes data is passed
#'   to this function via some combination of arguments \code{x}, \code{X},
#'   \code{y}, \code{Y}
#'
#' @return The same function, wrapped to have formula/dataframe and
#'   recipe/dataframe interfaces.
#' @export
#'
#' @examples
#'
#' library(glmnet)
#'
#' glmnet_form <- formulize(cv.glmnet)
#'
#' model <- glmnet_form(mpg ~ drat * hp - 1, mtcars)
#' predict(model, head(mtcars))
#'
formulize <- function(f) {
  function(design, data, ...) {

    des <- get_design(design, data)

    # the following can break if functions have both `x` and `X` as arguments
    x_arg <- match.arg(c("x", "X"), names(formals(f)), several.ok = TRUE)
    y_arg <- match.arg(c("y", "Y"), names(formals(f)), several.ok = TRUE)

    args <- list(des$X, des$y)
    names(args) <- c(x_arg, y_arg)
    object <- do.call("f", args)
    object$design <- design
    class(object) <- c("wrapped", class(object))
    object
  }
}

# stolen from utils::isS3stdGeneric to backport R 3.1, 3.2, 3.3
already_generic <- function(f) {
  bdexpr <- body(f)
  while (as.character(bdexpr[[1L]]) == "{") bdexpr <- bdexpr[[2L]]
  is.call(bdexpr) && identical(bdexpr[[1L]], as.name("UseMethod"))
}


#' Turn a modelling function into an S3 modelling generic
#'
#' Call this function for its side effects: (1) creating an S3 generic of the
#' passed modelling function, (2) creating a default method that's exactly the
#' X/y interface to formulize, (3) creating a formula method with formulize.
#'
#' @param f Modelling function to turn into a generic.
#' @param fname Optional name of new generic. Useful because otherwise the
#'   created generic masks the original modelling function.
#'
#' @return Nothing.
#' @export
genericize <- function(f, fname = NULL) {

  fname <- if (is.null(fname)) as.character(substitute(f)) else fname

  if ("::" %in% fname || ":::" %in% fname)
    stop("Do not use genericize with double or triple colon operators.")

  if (already_generic(f))
    stop(paste("S3 generic already exists for this function."))

  env <- globalenv()
  gen <- rlang::expr_interp(function(obj, ...) UseMethod(!!fname))
  environment(gen) <- env
  wrapper <- formulize(f)

  assign(fname, gen, envir = env)
  assign(paste0(fname, ".default"), f, envir = env)
  assign(paste0(fname, ".formula"), wrapper, envir = env)
  assign(paste0(fname, ".recipe"), wrapper, envir = env)
}

