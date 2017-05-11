#' @importFrom utils capture.output
call2str <- function(.x)
{
# XXX fragile, improve this, also remove ::: call
  if("call" %in% class(.x$expr)) return(rsub(capture.output(.x$expr), .x$env))
  rsub(as.character(.x$expr), .x$env)
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
  expr = gsub("&", " and ", expr)
  expr = gsub("&&", " and ", expr)
  expr = gsub('"', "'", expr)
  expr
}

# handle R() escaped values
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

# XXX suffix argument not yet supported, maybe use out_names in equi_join?
# internal
equi_join <- function(x, y, by=NULL, suffix=NULL, ...) {
  if(!(class(y[[1]]) %in% "scidb"))
  {
    y <- tbl_scidb(as.scidb(as.data.frame(y)))
  }
  if(is.null(by)) by <- intersect(schema(x[[1]], "attributes")$name, schema(y[[1]], "attributes")$name)
  N <- names(by)
  right_names <- sprintf("'right_names=%s'", paste(by, collapse=","))
  if(is.null(N)) left_names <- sprintf("'left_names=%s'", paste(by, collapse=","))
  else left_names <- sprintf("'left_names=%s'", paste(N, collapse=","))
  expr <- sprintf("equi_join(%s, %s, %s, %s)", x$db@name, y$db@name, left_names, paste(c(right_names, list(...)), collapse=", "))
  tbl(scidb(x$db@meta$db, expr))
}


# re-write common aggregation functions
# XXX must not apply to quoted expressions -- fix me
agsub <- function(x) {
  x <- gsub("n *\\( *\\)", "count(\\*)", x)
  x <- gsub("mean *\\(", "avg(", x)
  x <- gsub("sd *\\(", "stdev(", x)
  x
}
