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
  if("groups" %in% names(attributes(x))) message("Groups: ", paste(attr(x, "groups"), collapse=", "))
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

# Utilities -------------------------------------------------------------------

#' @export
collect.tbl_scidb <- function(x, ...) {
  tbl_df(as.R(x$db))
}

#' @export
compute.tbl_scidb <- function(x, name, ...) {
  if(missing(name)) y <- store(db=x$db@meta$db, expr=x$db, ...)
  else y <- store(db=x$db@meta$db, expr=x$db, name=name, ...)
  tbl(y)
}


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
                   } else call2str(.x)
               }, error=function(e) call2str(.x))),
         collapse=" and ")
  expr = aflify(sprintf("filter(%s, %s)", .data$db@name, .args))
  tbl(scidb(.data$db@meta$db, expr))
}

#' @export
mutate_.tbl_scidb <- function(.data, ..., .dots = list()) {
  dots <- all_dots(.dots, ...)
  .args = paste(names(dots),
             lapply(dots,
               function(.x) tryCatch({
                   if (class(eval(.x$expr, envir=.x$env))[1] %in% "scidb")
                   {
                     eval(.x$expr, envir=.x$env)@name
                   } else call2str(.x)
               }, error=function(e) call2str(.x))),
         sep=", ", collapse=", ")
  expr = aflify(sprintf("apply(%s, %s)", .data$db@name, .args))
  tbl(scidb(.data$db@meta$db, expr))
}

#' @export
transmute_.tbl_scidb <- function(.data, ..., .dots = list()) {
  dots <- all_dots(.dots, ...)
  .args = paste(names(dots),
             lapply(dots,
               function(.x) tryCatch({
                   if (class(eval(.x$expr, envir=.x$env))[1] %in% "scidb")
                   {
                     eval(.x$expr, envir=.x$env)@name
                   } else call2str(.x)
               }, error=function(e) call2str(.x))),
         sep=", ", collapse=", ")
  expr = aflify(sprintf("project(apply(%s, %s), %s)", .data$db@name, .args, paste(names(dots), collapse=",")))
  tbl(scidb(.data$db@meta$db, expr))
}

#' @export
group_by_.tbl_scidb <- function(.data, ..., .dots = list()) {
  dots <- all_dots(.dots, ...)
  groups <- NULL
  if(dots$add$expr) groups <- attr(.data, "groups")
  .args = lapply(dots[-1],
               function(.x) tryCatch({
                   if (class(eval(.x$expr, envir=.x$env))[1] %in% "scidb")
                   {
                     stop("invalid context for SciDB array")
                   } else call2str(.x)
               }, error=function(e) call2str(.x)))
  attr(.data, "groups") <- c(groups, gsub(" *$", "", gsub("^ *", "", unlist(.args))))
  .data
}

#' @export
summarise_.tbl_scidb <- function(.data, ..., .dots = list()) {
  dots <- all_dots(.dots, ...)
  N <- names(dots)
  N[N == ""] <- "V"
  N <- paste(" as ", gsub("\\.", "_", make.names(N, unique=TRUE)))
  .args = paste(agsub(lapply(dots,
               function(.x) tryCatch({
                   if (class(eval(.x$expr, envir=.x$env))[1] %in% "scidb")
                   {
                     eval(.x$expr, envir=.x$env)@name
                   } else call2str(.x)
               }, error=function(e) call2str(.x), warning=function(e) call2str(.x)))), N,
         sep=" ", collapse=", ")
  if(is.null(attr(.data, "groups")))
    expr = aflify(sprintf("aggregate(%s, %s)", .data$db@name, paste(.args, collapse=",")))
  else
    expr = aflify(sprintf("grouped_aggregate(%s, %s)", .data$db@name, paste(c(attr(.data, "groups"), .args), collapse=",")))
  tbl(scidb(.data$db@meta$db, expr))
}


#' @export
inner_join.tbl_scidb <- function(x, y, by=NULL, copy=FALSE, suffix=c(".x", ".y"), ...) {
  equi_join(x, y, by, suffix)
}

#' @export
left_join.tbl_scidb <- function(x, y, by=NULL, copy=FALSE, suffix=c(".x", ".y"), ...) {
  equi_join(x, y, by, suffix, "'left_outer=1'")
}

#' @export
right_join.tbl_scidb <- function(x, y, by=NULL, copy=FALSE, suffix=c(".x", ".y"), ...) {
  equi_join(x, y, by, suffix, "'right_outer=1'")
}

#' @export
full_join.tbl_scidb <- function(x, y, by=NULL, copy=FALSE, suffix=c(".x", ".y"), ...) {
  equi_join(x, y, by, suffix, "'left_outer=1'", "'right_outer=1'")
}
