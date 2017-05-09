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

# internal
call2str <- function(.x)
{
# XXX fragile, improve this, also remove ::: call
  if("call" %in% class(.x$expr)) return(rsub(capture.output(.x$expr), .x$env))
  rsub(as.character(.x$expr), .x$env)
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

# AFL expression syntax differences XXX not finished -- these substitutions must be supressed in quoted strings!
# internal function
aflify <- function(expr)
{
  expr = gsub("%as%", " as ", expr)
  expr = gsub("==", " = ", expr)
  expr = gsub("!=", " <> ", expr)
  expr = gsub("\\|", " or ", expr)
  expr = gsub("\\|\\|", " or ", expr)
  expr = gsub('"', "'", expr)
  expr
}

# internal, handle R() escaped values
rsub <- function (x, env)
{   
    x = sprintf(" %s", x)
    if (!grepl("[^[:alnum:]_]R\\(",x))
        return(x)
    imbalance_paren = function(x) {
        which(cumsum((as.numeric(charToRaw(x) == charToRaw("("))) -
            (as.numeric(charToRaw(x) == charToRaw(")")))) < 0)[1]
    }
    y = gsub("([^[:alnum:]_])R\\(", "\\1@R(", x)
    y = strsplit(y, "@R\\(")[[1]]
    expr = Map(function(x) {
        i = imbalance_paren(x)
        rexp = eval(parse(text = substring(x, 1, i - 1)), envir = env)
        rmdr = substring(x, i + 1)
        paste(rexp, rmdr, sep = "")
    }, y[-1])
    sprintf("%s%s", y[1], paste(expr, collapse = ""))
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
