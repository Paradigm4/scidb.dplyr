# A SciDB backend for dplyr. This is loosely based on the dplyr native
# tbl-cube.r example code (see the dplyr source code).

#' Create a dplyr tbl from a \code{scidb} object
#' @param x an object of class \code{scidb}
#' @return an object of class \code{tbl_scidb}
#' @seealso \code{\link{scidb}}
#' @export
tbl_scidb <- function(x) {
  if(!("scidb" %in% class(x))) stop("x must be a 'scidb' object")
  structure(list(db=x), class="tbl_scidb")
}

#' Create a dplyr tbl from a \code{scidb} object
#' @param src an object of class \code{scidb}
#' @param ... ignored
#' @return an object of class \code{tbl_scidb}
#' @seealso \code{\link{scidb}} \code{\link{tbl_scidb}}
#' @importFrom dplyr tbl
#' @export
tbl.scidb <- function(src, ...)
{
  tbl_scidb(src)
}

#' @importFrom dplyr tbl_vars
#' @export
tbl_vars.tbl_scidb <- function(x) names(x$db)

#' @export
dim.tbl_scidb <- function(x) {
  dim(x$db)
}

#' Print a summary of a SciDB tbl object
#' @param x an object of class \code{tbl_scidb}
#' @param ... additional arguments passed to the print method
#' @return NULL
#' @export
print.tbl_scidb <- function(x, ...) {
  if("groups" %in% names(attributes(x))) message("Groups: ", paste(attr(x, "groups"), collapse=", "))
  print(x$db, ...)
}

# Coercion methods ------------------------------------------------------------

#' @export
as.data.frame.tbl_scidb <- function(x, ...) {
  as.R(x$db, ...)
}

#' Download a remote SciDB array into an R data frame
#' @param x an object of class \code{tbl_scidb}
#' @param ... additional arguments passed to \code{\link{as.R}}
#' @importFrom scidb as.R
#' @export
as_data.frame.tbl_scidb <- function(x, ...) {
  as.R(x$db, ...)
}

#' Coerce an existing data structure into a `tbl_scidb`
#'
#' @param x an object to convert. Built in methods will convert vectors, arrays,
#'   some sparse matrices and data frames (see \code{\link{as.scidb}}).
#' @param db a scidb database connetion object.
#' @param ... additional parameters passed to \code{\link{as.scidb}}.
#' @importFrom scidb as.scidb
#' @export
as.tbl_scidb <- function(x, db, ...) as.scidb(db, x, ...)

# Utilities -------------------------------------------------------------------

#' @importFrom dplyr collect tbl_df
#' @export
collect.tbl_scidb <- function(x, ...) {
  tbl_df(as.R(x$db, ...))
}

#' @importFrom dplyr compute
#' @export
compute.tbl_scidb <- function(x, name, ...) {
  if(missing(name)) y <- store(db=x$db@meta$db, expr=x$db, ...)
  else y <- store(db=x$db@meta$db, expr=x$db, name=name, ...)
  tbl(y)
}


# Verbs -----------------------------------------------------------------------

#' @importFrom lazyeval all_dots
#' @importFrom scidb schema scidb
#' @importFrom dplyr select_
#' @export
select_.tbl_scidb <- function(.data, ..., .dots = list()) {
  dots <- all_dots(.dots, ...)
  atts <- schema(.data$db, "attributes")$name
  dplyr:::set_current_vars(atts)  # hmmm... XXX how to resolve this requirement?
  .args <-lapply(dots,
               function(.x) tryCatch({
                   if (class(eval(.x$expr, envir=.x$env))[1] %in% "scidb")
                   {
                     eval(.x$expr, envir=.x$env)@name
                   } else eval(.x$expr, envir=.x$env)
               }, error=function(e) call2str(.x)))
  if(length(args) < 1) stop("no matches")
  .args <- unlist(.args)
  if(is.null(names(.args)) || prod(nchar(names(.args))) == 0)
  {
    expr <- aflify(sprintf("project(%s, %s)", .data$db@name, paste(atts[.args], collapse=",")))
  } else { # arrgh...rename attributes; this needs work XXX
    aply <- paste(names(.args), atts[.args], sep=", ", collapse=", ")
    expr <- sprintf("project(apply(%s, %s), %s)", .data$db@name, aply, paste(names(.args), collapse=","))
  }
  tbl(scidb(.data$db@meta$db, expr))
}

#' @importFrom dplyr filter_
#' @export
filter_.tbl_scidb <- function(.data, ..., .dots = list()) {
  dots <- all_dots(.dots, ...)
  .args <-paste(
             lapply(dots,
               function(.x) tryCatch({
                   if (class(eval(.x$expr, envir=.x$env))[1] %in% "scidb")
                   {
                     eval(.x$expr, envir=.x$env)@name
                   } else call2str(.x)
               }, error=function(e) call2str(.x))),
         collapse=" and ")
  expr <- aflify(sprintf("filter(%s, %s)", .data$db@name, .args))
  tbl(scidb(.data$db@meta$db, expr))
}

#' @importFrom dplyr mutate_
#' @export
mutate_.tbl_scidb <- function(.data, ..., .dots = list()) {
  dots <- all_dots(.dots, ...)
  .args <-paste(names(dots),
             lapply(dots,
               function(.x) tryCatch({
                   if (class(eval(.x$expr, envir=.x$env))[1] %in% "scidb")
                   {
                     eval(.x$expr, envir=.x$env)@name
                   } else call2str(.x)
               }, error=function(e) call2str(.x))),
         sep=", ", collapse=", ")
  expr <- aflify(sprintf("apply(%s, %s)", .data$db@name, .args))
  tbl(scidb(.data$db@meta$db, expr))
}

#' @importFrom dplyr transmute_
#' @export
transmute_.tbl_scidb <- function(.data, ..., .dots = list()) {
  dots <- all_dots(.dots, ...)
  .args <-paste(names(dots),
             lapply(dots,
               function(.x) tryCatch({
                   if (class(eval(.x$expr, envir=.x$env))[1] %in% "scidb")
                   {
                     eval(.x$expr, envir=.x$env)@name
                   } else call2str(.x)
               }, error=function(e) call2str(.x))),
         sep=", ", collapse=", ")
  expr <- aflify(sprintf("project(apply(%s, %s), %s)", .data$db@name, .args, paste(names(dots), collapse=",")))
  tbl(scidb(.data$db@meta$db, expr))
}

#' @importFrom dplyr group_by_
#' @export
group_by_.tbl_scidb <- function(.data, ..., .dots = list()) {
  dots <- all_dots(.dots, ...)
  groups <- NULL
  if(dots$add$expr) groups <- attr(.data, "groups")
  .args <-lapply(dots[-1],
               function(.x) tryCatch({
                   if (class(eval(.x$expr, envir=.x$env))[1] %in% "scidb")
                   {
                     stop("invalid context for SciDB array")
                   } else call2str(.x)
               }, error=function(e) call2str(.x)))
  attr(.data, "groups") <- c(groups, gsub(" *$", "", gsub("^ *", "", unlist(.args))))
  .data
}

#' @importFrom dplyr summarise_
#' @export
summarise_.tbl_scidb <- function(.data, ..., .dots = list()) {
  dots <- all_dots(.dots, ...)
  N <- names(dots)
  N[N == ""] <- "V"
  N <- paste(" as ", gsub("\\.", "_", make.names(N, unique=TRUE)))
  .args <-paste(agsub(lapply(dots,
               function(.x) tryCatch({
                   if (class(eval(.x$expr, envir=.x$env))[1] %in% "scidb")
                   {
                     eval(.x$expr, envir=.x$env)@name
                   } else call2str(.x)
               }, error=function(e) call2str(.x), warning=function(e) call2str(.x)))), N,
         sep=" ", collapse=", ")
  if(is.null(attr(.data, "groups")))
    expr <- aflify(sprintf("aggregate(%s, %s)", .data$db@name, paste(.args, collapse=",")))
  else
    expr <- aflify(sprintf("grouped_aggregate(%s, %s)", .data$db@name, paste(c(attr(.data, "groups"), .args), collapse=",")))
  tbl(scidb(.data$db@meta$db, expr))
}


#' @importFrom dplyr inner_join
#' @export
inner_join.tbl_scidb <- function(x, y, by=NULL, copy=FALSE, suffix=c(".x", ".y"), ...) {
  equi_join(x, y, by, suffix)
}

#' @importFrom dplyr left_join
#' @export
left_join.tbl_scidb <- function(x, y, by=NULL, copy=FALSE, suffix=c(".x", ".y"), ...) {
  equi_join(x, y, by, suffix, "'left_outer=1'")
}

#' @importFrom dplyr right_join
#' @export
right_join.tbl_scidb <- function(x, y, by=NULL, copy=FALSE, suffix=c(".x", ".y"), ...) {
  equi_join(x, y, by, suffix, "'right_outer=1'")
}

#' @importFrom dplyr full_join
#' @export
full_join.tbl_scidb <- function(x, y, by=NULL, copy=FALSE, suffix=c(".x", ".y"), ...) {
  equi_join(x, y, by, suffix, "'left_outer=1'", "'right_outer=1'")
}
