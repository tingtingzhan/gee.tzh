

#' @title Variance-Covariance for \link[gee]{gee} Object
#' 
#' @description
#' ..
#' 
#' @param object a \link[gee]{gee} object
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @note
#' Function [vcov.gee()] is super important for `lmtest:::coeftest.glm`.
#' 
#' @seealso
#' `geepack:::vcov.geeglm`.
#' 
#' 
#' @importFrom utils getS3method
#' @importFrom stats vcov
#' @export vcov.gee
#' @export
vcov.gee <- function(object, ...) object$robust.variance





#' @title Additional S3 methods for \link[gee]{gee} and \link[geepack]{geeglm} Objects
#' 
#' @param x a \link[gee]{gee} or \link[geepack]{geeglm} object
#' 
#' @param level ..
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @examples
#' library(gee) # ?gee::gee
#' m1 = gee(breaks ~ tension, id=wool, data = warpbreaks, corstr="exchangeable")
#' # gee::gee object is badly defined; as well as package \pkg{gee}
#' 
#' library(geepack) # ?geepack::geeglm
#' m2 = geeglm(breaks ~ tension, id=wool, data = warpbreaks, corstr="exchangeable")
#' # I prefer using geepack::geeglm
#' 
#' @name S3_gee
#' @importFrom utils packageDate
#' @export .pval.summary.gee
#' @export
.pval.summary.gee <- function(x) {
  cf <- x$coefficients
  dt <- packageDate(pkg = 'gee')
  if (dt == as.Date('2024-12-10')) stop('gee:::summary.gee (2024-12-10) does not carry p-value(s)')
  stop('check if new version of gee:::summary.gee carry p-value(s)')
}

#' @rdname S3_gee
#' @export .pval.summary.geeglm
#' @export
.pval.summary.geeglm <- function(x) {
  cf <- x$coefficients
  ret <- cf[, 'Pr(>|W|)']
  names(ret) <- rownames(cf)
  return(ret)
}


# no gee:::confint.gee function as of packageDate('gee') 2024-12-10
# will dispatch to ?stats:::confint.glm
#' @rdname S3_gee
#' @importFrom stats confint.default
#' @export confint_.gee
#' @export
confint_.gee <- function(x, level = .95, ...) {
  ci <- confint.default(object = x, level = level, ...)
  attr(ci, which = 'conf.level') <- level
  return(ci)
}
# also takes care of geepack::geeglm objects
# no geepack:::confint.* as of packageDate('geepack') 2024-09-23

#' @rdname S3_gee
#' @export nobsText.gee
#' @export
nobsText.gee <- function(x) .Defunct(new = 'geeglm()', msg = 'cannot retrieve clustering from gee')


#' @rdname S3_gee
#' @export nobsText.geeglm
#' @export
nobsText.geeglm <- function(x) x[['geese']] |> nobsText.geese()


#' @rdname S3_gee
#' @export nobsText.geese
#' @export
nobsText.geese <- function(x) {
  id <- x[['id']] # very often vector
  id_call <- x$call$id
  sprintf(fmt = '%d records from %d `%s`', length(id), length(unique(id)), deparse1(id_call))
}

#' @rdname S3_gee
#' @export desc_.gee
#' @export
desc_.gee <- function(x, ...) {
  paste('generalized estimating equations (GEE) with', 
        NextMethod(generic = 'desc_', object = x))
}
# also dispatches to geepack::geeglm objects



# needed, but I don't want to define [model.frame.gee()]
# ?gee::gee return has a `$model`; and ?stats:::model.frame.glm returns it (eh...)
# I want to avoid the use of ?gee::gee all-together
# @method model.frame gee
# @importFrom stats model.frame model.frame.default
# @export
#model.frame.gee <- function(formula, ...) {
#  if (!is.data.frame(data <- eval(formula$call$data))) stop('`data` must be evaluable')
#  # otherwise dispatch to ?stats:::model.frame.glm
#  model.frame.default(formula(formula), data = data, ...)
#} 







# use ?geepack:::vcov.geeglm





