
# on sq '.data' a ete mise a jour, 
# on refait passer les conditions dessus
# en supprimant les conditions qui passent

#' @title 
#' Update observations 
#' 
#' @description 
#' blablablabla
#' 
#' @param .data
#' A tbl or data frame. 
#' 
#' @param status 
#' character. 
#' 
#' @return 
#' 
#' @seealso 
#' \code{\link[observer]{observe_if}}, 
#' \code{\link[observer]{check_that}}, 
#' and \code{\link[observer]{ensure_that}} in this package. 
#' 
#' @importFrom base2 is.empty
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
  if (base2::is.empty(obs)) return(.data)
  obs2 <- observe_(.data, .dots = obs[["Predicate"]]) %>% 
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
