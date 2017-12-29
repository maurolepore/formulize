#' Convert a formula and data frame to a design matrix and response vector
#'
#' Note that this function is not exported.
#'
#' @param formula Formula specifying design matrix.
#' @param data Data frame in which to evaluate formula.
#' @param ... Included so we can check if an \code{intercept} was specified.
#'
#' @return A list with three named entries: a data matrix \code{X},
#'   a response vector (potentially a matrix later on) \code{y}, and
#'   a logical \code{intercept} indicating if the formula specified that
#'   an intercept should be added to the design matrix.
#'
#' @importFrom stats model.response model.frame model.matrix
formula_helper <- function(formula, data, ...) {

  if ("intercept" %in% names(match.call())) {
    stop("Do not specify ", sQuote("intercept"), " when using formula interface.")
  }

  y <- model.response(model.frame(formula, data))
  X <- model.matrix(formula, data)

  assign <- attr(X, "assign")

  # infer the user's desire to include an intercept or not
  intercept <- 0 %in% assign

  if (intercept && length(assign) == 1) {
    warning("Intercept only models may break things.")
  }

  if (intercept) {
    X <- X[, !(colnames(X) %in% "(Intercept)")]
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
    newdata <- formula_helper(object$formula, newdata)$X
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
#' TODO: better search for X, y arguments other than positional matching
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
    design <- formula_helper(formula, data)
    obj <- wrapped_f(design$X, design$y, ...)
    obj$formula <- formula
    obj
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
    stop(paste(fname, "already a S3 generic. Use `formulize` to add a formula method."))

  gen <- rlang::expr_interp(function(obj, ...) {
    UseMethod(!!fname)
  })

  # environment(gen) <- env

  env <- globalenv()

  assign(fname, gen, envir = env)
  assign(paste0(fname, ".default"), f, envir = env)
  assign(paste0(fname, ".formula"), formulize(f), envir = env)
}

