get_data_design <- function(formula, data, ...) {

  y <- stats::model.response(stats::model.frame(formula, data))
  X <- stats::model.matrix(formula, data)

  assign <- attr(X, "assign")
  intercept <- 0 %in% assign        # check if formula included intercept term

  if (intercept && length(assign) == 1) {
    warning("Intercept only models may break things.")
  }

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
predict.wrapped <- function(object, newdata = NULL, ...) {
  if (!is.null(object$formula)) {
    newdata <- get_data_design(object$formula, newdata)$X
  }
  NextMethod()
}

#' Create a formula interface to a modelling method
#'
#' @param f A function with a matrix/vector interface.
#'
#' @return The same function, wrapped to have a formula/dataframe interface.
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
  wrapper <- f
  function(formula, data, ...) {

    # TODO: match on x/X/y/Y rather than positionally

    design <- get_data_design(formula, data)
    object <- wrapper(design$X, design$y, ...)
    object$formula <- formula
    class(object) <- c("wrapped", class(object))
    object
  }
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

  if (utils::isS3stdGeneric(f))
    stop(paste("S3 generic already exists for this function."))

  env <- globalenv()
  gen <- rlang::expr_interp(function(obj, ...) UseMethod(!!fname))
  environment(gen) <- env

  assign(fname, gen, envir = env)
  assign(paste0(fname, ".default"), f, envir = env)
  assign(paste0(fname, ".formula"), formulize(f), envir = env)
}

