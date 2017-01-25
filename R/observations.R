#' @title 
#' Get or set the 'observations' attribute
#' 
#' @description 
#' The function \code{observations} (or \code{obs} for short) 
#' extracts the \code{observations} attribute from \code{.data} 
#' created by \code{\link[observer]{observe_if}} or 
#' \code{\link[observer]{observe_if_}}. 
#'  
#' @param .data,x
#' A tbl or data frame
#' 
#' @param compressed
#' logical. By default, the column \code{"Rows"} 
#' of \code{observations} is compressed with the \code{\link[bit]{as.bit}} 
#' function from package \pkg{bit}. 
#' If \code{compressed=FALSE}, this column is uncompressed 
#' (and becomes understandable / readable by the user). 
#' 
#' @param value
#' A tibble, see 
#' \code{\link[observer]{observations}}. 
#' 
#' @return 
#' A tbl. 
#'  
#' @seealso 
#' \code{\link[observer]{observe_if}} in this package. 
#' 
#' @export
#' 
observations <-
function(.data, 
         compressed = TRUE)
{
  obs <- attr(.data, "observations")
  if (!compressed) {
    obs[["Rows"]] <- lapply(obs[["Rows"]], FUN = function(x) which(as.logical(x)))
  }
  obs
}


#' @importFrom tibble is_tibble
#' @export
#' @rdname observations
#' 
"observations<-" <- 
function(x, 
         value)
{
  stopifnot(tibble::is_tibble(value))
  attr(x, "observations") <- value
  x
}


#' @export
#' @rdname observations
#' 
obs <- observations


#' @export
#' @rdname observations
#' 
"obs<-" <- 
function(x, 
         value)
{
  attr(x, "observations") <- value
  x
}
