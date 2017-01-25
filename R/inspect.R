#' @title 
#' Inspect observations
#' 
#' @description 
#' Once a table has been observed with \code{observe_if}, one may wish to dig 
#' into the observations made. The function \code{inspect} returns a sub-table 
#' of \code{.data} whose rows correspond to the rows identified by the 
#' observation \code{ob}. 
#' 
#' @param .data
#' A tbl or data frame. 
#' 
#' @param ob
#' integer. A row of the \code{observations} attribute to be inspected. 
#' 
#' @param cols
#' character. One of \code{"all"}, \code{"most"}, \code{"some"}. 
#' If \code{cols = "all"}, all \code{.data} columns are kept in the result. 
#' If \code{cols = "some"}, only columns that are concerned by the 
#' observation \code{ob} are kept. 
#' \code{cols = "most"} is like \code{cols = "some"}, except that 
#' columns which are of type \code{"character"} or \code{"factor"} are also 
#' kept. 
#' 
#' @seealso 
#' \code{\link[observer]{observe_if}} in this package. 
#' 
#' @importFrom bazar as.empty
#' @importFrom bazar is.empty
#' @importFrom bazar is.wholenumber
#' @export
#' 
#' @examples 
#' library(magrittr)
#' 
#' df <- data.frame(id = c("a", "b", "c"), x = 1:3, y = 2:4, z = c(1.1, 10, 5))
#' df <- df %>% 
#'   observe_if(nrow(.)==4, 
#'              z==floor(z), 
#'              y-x==1, 
#'              x < 3) %>% 
#'   observe_if(y < 4, 
#'              x > 1)
#'   
#' print(obs(df))
#' 
#' df1 <- df %>%  
#'   inspect(ob = 6, cols="most")
#' print(obs(df1))
#' 
inspect <- 
function(.data, 
         ob,
         cols = "all")
{
  if (length(ob) > 1L) stop("'ob' must be of length one, inspect one observation at a time")
  stopifnot(bazar::is.wholenumber(ob))
  
  obs <- observations(.data)
  if (bazar::is.empty(obs)) {
    message("no observations detected in '.data', so '.data[0,]' is returned")
    return(bazar::as.empty(.data))
  }
  
  cols_selection <- cols
  ob <- obs[ob,]
  if (ob[["Status"]]=="passed") {
    message("The observation considered has passed successfully, nothing to investigate, so '.data[0,]' is returned")
    rows <- 0L
    cols_selection <- "all"
  } else {
    rows <- which(as.logical(ob[["Rows"]][[1L]]))
    cols <- parse(text = ob[["Predicate"]])
    cols <- intersect(all.vars(cols), names(.data))
    cols <- if (bazar::is.empty(cols)) names(.data) else cols
  }
  
  if (length(cols_selection) == 1L && cols_selection %in% c("all", "most", "some")) {
    if (cols_selection == "all") {
      cols <- names(.data)
    } else if (cols_selection == "some") {
      # do nothing
    } else if (cols_selection == "most") {
      b <- sapply(.data, FUN = function(x) is.character(x) || is.factor(x))
      cols <- unique(c(names(.data)[b], cols))
    }
  } else {
    cols <- cols_selection
  }

  .data <- .data[rows, cols]
  observations(.data) <- ob#structure(ob, class = c("obs_df", "tbl_df", "tbl", "data.frame"))
  .data
}
