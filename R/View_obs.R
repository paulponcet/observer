#' @title 
#' View observations 
#' 
#' @description 
#' Invoke data viewer on \code{observations(x)}. 
#' 
#' @param x
#' A data frame, possibly with an \code{observations} attribute. 
#' 
#' @param title
#' Title for viewer window. 
#' 
#' @seealso 
#' \code{\link[utils]{View}} from package \pkg{utils}. 
#' 
# #' @importFrom utils View
#' @export
#' 
View_obs <- 
function(x, 
         title)
{
  if (missing(title))
    title <- paste0("obs(", deparse(substitute(x))[1L], ")")
  View(observations(x, compressed = FALSE), title)
}
