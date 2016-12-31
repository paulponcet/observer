
#' @export
#' @rdname check_that
#' 
ensure_that <- 
function(.data, 
         ...)
{
  ensure_that_(.data, .dots = lazyeval::lazy_dots(...))  
}


#' @export
#' @rdname check_that
#' 
ensure_that_ <-
function (.data, 
          ..., 
          .dots) 
{
  dots <- lazyeval::all_dots(.dots, ...)

  ## Go through each condition one by one 
  ## and raise an error as soon as a condition fails
  for (i in seq_along(dots)) {
    .data <- observe_if_(.data, .dots = dots[i])
    obs <- observations(.data)
    ob <- obs[nrow(obs),]
    if (ob[["Status"]]=="failed") {
      rows <- which(as.logical(ob[["Rows"]][[1L]]))
      etc <- if (length(rows) > 6) "..." else ""
      stop(paste0("condition '", ob[["Predicate"]], "' failed on row(s) ", 
                  paste(head(rows), collapse = ", "), etc), 
           call. = FALSE)
    }
  }
  observations(.data) <- NULL
  .data
}


#' @export
#' @rdname check_that
#' 
ensure <- 
function(.data, 
         ...)
{
  .data <- observe(.data)
  obs <- observations(.data)
  ensure_that_(.data, .dots = obs[["Predicate"]])
}
