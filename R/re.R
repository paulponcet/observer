
# on sq '.data' a ete mise a jour, 
# on refait passer les conditions dessus
# en supprimant les conditions qui passent

#' @title 
#' Update observations 
#' 
#' @description 
#' \code{reobserve} (resp. \code{recheck}, \code{reensure}) re-reruns the 
#' function \code{observe_if} (resp. \code{check_that}, \code{ensure_that}) on 
#' \code{.data} with respect to the predicates contained in its 
#' \code{observations} attribute created at a previous occasion.  
#' 
#' @param .data
#' A tbl or data frame. 
#' 
#' @param status 
#' character. One of \code{"failed"}, \code{"passed"}, or both. 
#' The \code{observations} attribute of \code{.data} is filtered according 
#' to the value of \code{status}. 
#' 
#' @return 
#' \code{.data} is returned, with its \code{observations} attribute udpated. 
#' 
#' @seealso 
#' \code{\link[observer]{observe_if}}, 
#' \code{\link[observer]{check_that}}, 
#' and \code{\link[observer]{ensure_that}} in this package. 
#' 
#' @importFrom bazar is.empty
#' @importFrom dplyr filter_
#' @importFrom dplyr mutate_
#' @importFrom magrittr %>%
#' @export
#' 
reobserve <- 
function(.data,
         status = c("failed", "passed"))
{
  obs <- observations(.data)
  observations(.data) <- NULL
  if (bazar::is.empty(obs)) return(.data)
  obs2 <- observe_if_(.data, .dots = obs[["Predicate"]]) %>% 
    observations() %>% 
    dplyr::mutate_(Number_of_trials = ~ obs[["Number_of_trials"]]+1L) %>% 
    dplyr::filter_(~ Status %in% status)
  observations(.data) <- obs2
  .data
}


#' @export
#' @rdname reobserve
#' 
recheck <- 
function(.data,
         status = c("failed", "passed"))
{
  .data <- reobserve(.data, status)
  check(.data)
}


#' @export
#' @rdname reobserve
#' 
reensure <- 
function(.data,
         status = c("failed", "passed"))
{
  .data <- reobserve(.data, status)
  ensure(.data)
}
