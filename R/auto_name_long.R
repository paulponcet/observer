
#' @importFrom lazyeval as.lazy_dots
#' 
auto_name_long <- 
function(x)
{
  x <- lazyeval::as.lazy_dots(x)
  nms <- if (is.null(names(x))) rep("", length(x)) else names(x)
  missing <- nms == ""
  expr <- lapply(x[missing], `[[`, "expr")
  nms[missing] <- vapply(expr, deparse_long, FUN.VALUE = character(1), USE.NAMES = FALSE)
  names(x) <- nms
  x
}

deparse_long <- 
function(x)
{
  if (is.symbol(x)) {
    return(as.character(x))
  }
  paste0(deparse(x), collapse = "")
}
