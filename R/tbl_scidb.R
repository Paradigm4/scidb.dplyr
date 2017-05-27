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

#' @export
src_scidb <- function(ip_address, ...) {
  db = scidbconnect(ip_address, ...)
}

#' @importFrom dplyr src_tbls
#' @export
src_tbls <- function(db) 
{
  line0 = db$list("'arrays'")
  line1 = db$project(line0, "name")
  as.R(line1, only_attributes = TRUE)$name
}

#' @importFrom dplyr copy_to tbl
#' @export
copy_to <- function(db, data, temporary, ...) {
  argsvals = list(...)
  if("name" %in% argsvals){
    tbl(as.scidb(db, data, name=name, gc = temporary, ...))
  }else{
    tbl(as.scidb(db, data, gc = temporary, ...))
  }
}

#' @importFrom dplyr collect tbl_df
#' @export
collect.tbl_scidb <- function(x, ...) {
  tbl_df(as.R(x$db, ...))
}

#' @importFrom dplyr compute
#' @importFrom scidb store
#' @export
compute.tbl_scidb <- function(x, name, ...) {
  if(missing(name)) y <- store(db=x$db@meta$db, expr=x$db, ...)
  else y <- store(db=x$db@meta$db, expr=x$db, name=name, ...)
  tbl(y)
}


# Verbs -----------------------------------------------------------------------

#' Select SciDB subarrays by coordinates
#'
#' This method is mapped to the SciDB \code{between} operator. Each argument
#' specifies a range of SciDB array coordinates in the order of the listed
#' SciDB array dimensions. Use \code{NA} or missing ranges to include everything.
#'
#' @examples
#' \dontrun{
#' library(scidb.dplyr)
#' db <- scidbconnect()
#' x <- tbl(as.scidb(db, matrix(1:20, nrow=5))) # (a 5 x 4 matrix)

#' # Slice rows zero to three in the first dimension, all of the 2nd dimension:
#' slice(x, 0:3)
#'
#' # Slice rows zero to three in the 1st array dimension, 1 to 2 in the 2nd:
#' slice(x, 0:3, 1:2)
#' }
#' @importFrom lazyeval all_dots
#' @importFrom scidb schema scidb
#' @importFrom dplyr slice_
#' @export
slice_.tbl_scidb <- function(.data, ..., .dots = list()) {
  dims <- schema(.data$db, "dimensions")$name
  dots <- all_dots(.dots, ...)
  .args <-lapply(dots,
               function(.x) tryCatch({
                   if (class(eval(.x$expr, envir=.x$env))[1] %in% "scidb")
                   {
                     eval(.x$expr, envir=.x$env)@name
                   } else eval(.x$expr, envir=.x$env)
               }, error=function(e) call2str(.x)))
  if(length(.args) < 1) stop("no matches")
  w <- options(warn=-1)
  on.exit(options(w))
  N <- length(dims) - length(.args)
  if(N > 0) .args <- c(.args, rep(NA, N))
  N <- length(dims)
  .args <- unlist(Map(range, .args))
  .args <- paste(c(.args[seq(1, by=2, length.out=N)], .args[seq(2, by=2, length.out=N)]), sep=", ", collapse=", ")
  .args <- gsub("NA", "NULL", gsub("Inf", "NULL", gsub("-Inf", "NULL", .args)))
  expr <- aflify(sprintf("between(%s, %s)", .data$db@name, .args))
  tbl(scidb(.data$db@meta$db, expr))
}

#' @importFrom dplyr select_
#' @export
select_.tbl_scidb <- function(.data, ..., .dots = list()) {
  atts <- schema(.data$db, "attributes")$name
  dplyr:::set_current_vars(atts)  # hmmm... XXX how to resolve this requirement?
  dots <- all_dots(.dots, ...)
  .args <-lapply(dots,
               function(.x) tryCatch({
                   if (class(eval(.x$expr, envir=.x$env))[1] %in% "scidb")
                   {
                     eval(.x$expr, envir=.x$env)@name
                   } else eval(.x$expr, envir=.x$env)
               }, error=function(e) call2str(.x)))
  if(length(.args) < 1) stop("no matches")
  .args_char <- vapply(.args, is.character, TRUE)
  .args_name <- names(.args)
  if(any(.args_char))
  {
    .args[.args_char] <- lapply(.args[.args_char], function(x) match(gsub("^ *", "", x), atts))
  }
  names(.args) <- .args_name
  .args <- unlist(.args)
  if(is.character(.args)) stop("selection error, this may be a bug in scidb.dplyr, please report")
  if(!is.null(names(.args)) && prod(nchar(names(.args))) == 0)
  { # only some attributes renamed, not all of them.
    .args_name <- which(nchar(names(.args)) == 0)
    names(.args)[.args_name] <- atts[.args[.args_name]]
  }
  if(is.null(names(.args)))
  {
    expr <- aflify(sprintf("project(%s, %s)", .data$db@name, paste(atts[.args], collapse=",")))
  } else { # arrgh...rename attributes; this needs work XXX
    .args_rename <- .args[which(names(.args) != atts[.args])]
    if(length(.args_rename) < 1)
      expr <- aflify(sprintf("project(%s, %s)", .data$db@name, paste(atts[.args], collapse=",")))
    else
    {
      names(.args_rename) <- gsub("\\.", "_", make.names(names(.args_rename), unique=TRUE))
      aply <- paste(names(.args_rename), atts[.args_rename], sep=", ", collapse=", ")
      expr <- sprintf("project(apply(%s, %s), %s)", .data$db@name, aply, paste(names(.args), collapse=","))
    }
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
