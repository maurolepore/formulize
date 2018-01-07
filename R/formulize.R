#' Convert a formula and data frame to a design matrix and response vector
#'
#' This function is not exported.
#'
#' @param formula Formula specifying design matrix.
#' @param data Data frame in which to evaluate formula.
#' @param remove_intercept
#' @param ... Included so we can check if an \code{intercept} was specified.
#'
#' @return A list with three named entries: a data matrix \code{X},
#'   a response vector (potentially a matrix later on) \code{y}, and
#'   a logical \code{intercept} indicating if the formula specified that
#'   an intercept should be added to the design matrix.
#'
#' @importFrom stats model.response model.frame model.matrix
formula_helper <- function(formula,
                           data,
                           remove_intercept,
                           ...) {

  y <- model.response(model.frame(formula, data))
  X <- model.matrix(formula, data)

  assign <- attr(X, "assign")
  intercept <- 0 %in% assign        # check if formula included intercept term

  if (intercept && length(assign) == 1) {
    warning("Intercept only models may break things.")
  }

  if (remove_intercept) {
    X <- X[, !(colnames(X) %in% "(Intercept)"), drop = FALSE]
  }

  list(X = X, y = y, intercept = intercept)
}


#' TODOCUMENTDO
#'
#' @param object doc doc
#' @param newdata more doc
#' @param ... woops
#'
#' @return
#' @export
#'
#' @examples
predict.wrapped <- function(object, newdata = NULL, ...) {
  if (!is.null(object$formula)) {
    newdata <- formula_helper(formula = object$formula,
                              data = newdata,
                              remove_intercept = object$has_int_arg)$X
  }
  NextMethod()
}

#' Document me pleeeease
#'
#' @param f thing
#'
#' @return
#' @importFrom rlang exprs lang "!!" "!!!"
#' @export
wrap <- function(f) {
  new_exprs <- exprs(obj <- !!body(f), class(obj) <- c("wrapped", class(obj)), obj)
  new_body <- lang("{", !!!new_exprs)
  body(f) <- new_body
  f
}

#' Also meeeee
#'
#' @param f nother thing
#'
#' @return
#' @export
#'
#' @examples
formulize <- function(f) {
  wrapped_f <- wrap(f)
  function(formula, data, ...) {

    # TODO: add intercept_control argument? always strip, look for arg name

    # if there's an intercept argument:
    #   1. don't create an intercept column
    #   2. passing something to it during the formulized train call
    has_int_arg <- "intercept" %in% tolower(names(formals(f)))

    design <- formula_helper(formula = formula,
                             data = data,
                             remove_intercept = has_int_arg)

    # if there's an intercept argument, the user shouldn't specify it since we will
    # warn them if they do
    if ("intercept" %in% tolower(names(match.call(f)))) {
      stop("Do not specify `intercept` argument when using formula interface.")
    }

    # TODO: match on x/X/y/Y rather than positional

    if (has_int_arg) {
      object <- wrapped_f(design$X, design$y, intercept = design$intercept, ...)
    } else {
      object <- wrapped_f(design$X, design$y, ...)
    }

    object$formula <- formula
    object$has_int_arg <- has_int_arg
    object
  }
}

#' A good day to title hard
#'
#' TODO: calls like glmnet::cv.glmnet break function name discovery, maybe other things
#' TODO: don't call genericize twice, call it once for side-effects
#' TODO: don't call genericize on things that already have a nice formula interface
#'
#' Title
#'
#' @param f more stuff
#' @param fname so much good documentation
#'
#' @return
#' @export
#'
#' @examples
genericize <- function(f, fname = NULL) {

  fname <- if (is.null(fname)) as.character(substitute(f)) else fname

  if (utils::isS3stdGeneric(f))
    stop(paste(fname, "already a S3 generic. Use `formulize` to add formula method."))

  gen <- rlang::expr_interp(function(obj, ...) {
    UseMethod(!!fname)
  })

  env <- globalenv()
  environment(gen) <- env

  assign(fname, gen, envir = env)
  assign(paste0(fname, ".default"), f, envir = env)
  assign(paste0(fname, ".formula"), formulize(f), envir = env)
}

