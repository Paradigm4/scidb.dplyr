# A SciDB backend for dplyr. This is loosely based on the dplyr native
# tbl-cube.r example code (see the dplyr source code).

#' Create a dplyr tbl from a \code{scidb} object
#' @param x an object of class \code{scidb}
#' @return an object of class \code{tbl_scidb}
#' @seealso \code{\link{scidb}}
#' @export
tbl_scidb <- function(x) {
  structure(list(db=x), class="tbl_scidb")
}

#' Create a dplyr tbl from a \code{scidb} object
#' @param data an object of class \code{scidb}
#' @return an object of class \code{tbl_scidb}
#' @seealso \code{\link{scidb}} \code{\link{tbl_scidb}}
#' @export
tbl.scidb <- function(data, ...)
{
  tbl_scidb(data)
}

#' @export
tbl_vars.tbl_scidb <- function(x) names(x$db)

#' @export
dim.tbl_scidb <- function(x) {
  dim(x$db)
}

#' @export
print.tbl_scidb <- function(x, ...) {
  print(x$db, ...)
}

# Coercion methods ------------------------------------------------------------

#' @export
as.data.frame.tbl_scidb <- function(x, ...) {
  as.R(x$db)
}

#' @export
as_data.frame.tbl_scidb <- function(x, ...) {
  as.R(x$db)
}

#' Coerce an existing data structure into a `tbl_scidb`
#'
#' @param x an object to convert. Built in methods will convert vectors, arrays,
#'   some sparse matrices and data frames (see \code{\link{as.scidb}}).
#' @param db a scidb database connetion object.
#' @param ... additional parameters passed to \code{\link{as.scidb}}.
#' @export
as.tbl_scidb <- function(x, db, ...) as.scidb(db, x, ...)

# Verbs -----------------------------------------------------------------------

#' @export
select.tbl_scidb <- function(.data, ...) {
  vars <- select_vars(names(.data$db), ...)
  scidb(.data$db@meta$db, sprintf("project(%s, %s)", .data$db@name, paste(vars, collapse=",")))
}

#' @importFrom lazyeval all_dots
#' @export
select_.tbl_scidb <- function(.data, ..., .dots = list()) {
  dots <- all_dots(.dots, ...)
## XXX not good enough, see filter below
  env <- if(length(dots) > 0) dots[[1]]$env else globalenv()
  exprs <- lapply(dots, "[[", "expr")
  vars <- select_vars_(names(.data$db), lapply(exprs, lazyeval::as.lazy, env))
# XXX why not the following? Scoping problems with this...
#  .data$db@meta$db$project(R(.data$db@name), paste(vars, collapse=","))
  tbl(scidb(.data$db@meta$db, sprintf("project(%s, %s)", .data$db@name, paste(vars, collapse=","))))
}


#' @export
filter_.tbl_scidb <- function(.data, ..., .dots = list()) {
  dots <- all_dots(.dots, ...)
  .args = paste(
             lapply(dots,
               function(.x) tryCatch({
                   if (class(eval(.x$expr, envir=.x$env))[1] %in% "scidb")
                   {
                     eval(.x$expr, envir=.x$env)@name
                   }
                   else capture.output(scidb:::rsub(.x$expr, .x$env)) # XXX fragile, improve this, also remove ::: call
               }, error=function(e) capture.output(.x$expr))),
         collapse=" and ")
  expr = sprintf("filter(%s, %s)", .data$db@name, .args)
# handle aliasing
  expr = gsub("%as%", " as ", expr)
  tbl(scidb(.data$db@meta$db, expr))
}

