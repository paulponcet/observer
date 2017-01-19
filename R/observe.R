#' @title 
#' Observe your data 
#' 
#' @description 
#' 
#' @param .data
#' A tbl or data.frame.
#' 
#' @param ...
#' Logical predicates. 
#' Multiple conditions are considered as separate observations.  
#' 
#' @param .dots
#' Used to work around non-standard evaluation. 
#' 
#' @param .append
#' logical. If \code{TRUE} (the default), the \code{observations} 
#' attribute, in case it already exists before the call, is appended; 
#' if \code{FALSE} it is updated. 
#' 
#' @return 
#' \code{.data} is returned together with an \code{observations} attribute.
#' This attribute is a data frame (actually a \code{\link[tibble]{tibble}}) 
#' which contains all check runs. 
#' 
#' @importFrom lazyeval lazy_dots
#' @export
#' 
#' @examples 
#' library(magrittr)
#' 
#' df <- data.frame(x = 1:3, y = 2:4)
#' df %>% 
#'   observe_if(nrow(.)==4) %>% 
#'   obs()
#' df %>% 
#'   observe_if(y-x==1) %>% 
#'   observe_if(x < 3) %>% 
#'   observe_if(y < 4, x > 1) %>% 
#'   obs()
#' 
observe_if <- 
function(.data, 
         ..., 
         .append = TRUE)
{
  observe_if_(.data, 
              .dots = lazyeval::lazy_dots(...), 
              .append = .append)
}


#' @export
#' @rdname observe_if
#' 
observe_if_ <- 
function(.data, 
         ..., 
         .dots, 
         .append = TRUE)
{
  UseMethod("observe_if_")
}


#' @importFrom dplyr tbl_df
#' @importFrom lazyeval all_dots
#' @export
#' @rdname observe_if
#' 
observe_if_.data.frame <-
function (.data, 
          ..., 
          .dots, 
          .append = TRUE) 
{
  dots <- lazyeval::all_dots(.dots, ...)
  as.data.frame(observe_if_(dplyr::tbl_df(.data), 
                            .dots = dots, 
                            .append = .append))
}


#' @importFrom lazyeval all_dots
#' @export
#' @rdname observe_if
#' 
observe_if_.tbl_df <- 
function(.data, 
         ..., 
         .dots, 
         .append = TRUE)
{
  dots <- lazyeval::all_dots(.dots, ...)
  if (any(has_names(dots))) {
    stop("observe_if() takes unnamed arguments. Do you need `==`?", 
         call. = FALSE)
  }
  dots <- auto_name_long(dots)
  observe_if_impl(.data, dots, .append)
}


#' @importFrom dplyr do_
#' @importFrom dplyr select_
#' @importFrom dplyr ungroup
#' @importFrom lazyeval all_dots
#' @export
#' @rdname observe_if
#'
observe_if_.grouped_df <- 
function(.data, 
         ..., 
         .dots, 
         .append = TRUE)
{
###########################################
# ATTENTION NE MARCHE PAS, LES NUM DE LIGNES SONT CEUX DES GROUPES....
###########################################
 
  stop("'observe_if' is not implemented yet for 'grouped_df' objects") 
  
  dots <- lazyeval::all_dots(.dots, ...)
  if (any(has_names(dots))) {
    stop("observe_if() takes unnamed arguments. Do you need `==`?", 
         call. = FALSE)
  }
  dots <- auto_name_long(dots)
  
  vars <- as.character(attr(.data, "vars"))
  
  chs <- dplyr::do_(.data, ~ observations(observe_if_impl(., dots, .append)))
  chs <- dplyr::ungroup(chs)
  #chs <- dplyr::select_(chs, .dots = paste0("-", vars))
  observations(.data) <- chs
  .data
}



#' @importFrom bazar is.empty
#' @importFrom bit as.bit
#' @importFrom lazyeval lazy_eval
#' @importFrom tibble tibble_
#' 
observe_if_impl <- 
function(df, 
         dots,
         append = TRUE)
{
  old_obs <- obs(df)
  if (!append) old_obs <- NULL
  max_id <- if (bazar::is.empty(old_obs)) 0L else max(old_obs[["Id"]])
  p <- lazyeval::lazy_eval(dots, df)
  n <- length(dots)
  
  l <- lapply(p, FUN = function(x) has_failed(x, nrow(df)))
  
  new_obs <- tibble::tibble_(list(
    Id = ~ seq_len(n) + max_id, 
    Predicate = ~ names(p), 
    Passed = ~ sapply(l, FUN = function(x) sum(!x, na.rm = TRUE)), 
    Failed = ~ sapply(l, FUN = function(x) sum(x, na.rm = TRUE)), 
    Missing = ~ sapply(l, FUN = function(x) sum(is.na(x))),
    #Failed_rows = ~ lapply(p, FUN = function(x) which_fails(x, nrow(df))), 
    Rows = ~ lapply(l, FUN = bit::as.bit),
    Status = ~ sapply(Failed, FUN = function(x) if (x > 0) "failed" else "passed"),
    Number_of_trials = ~ rep(1L, n)
  ))
  
  new_obs <- rbind(old_obs, new_obs)
  observations(df) <- new_obs
  df
}


#' @export
#' @rdname observe_if
#' 
observe <- 
function(.data, 
         ...)
{
  UseMethod("observe")
}


#' @export
#' @rdname observe_if
#' 
observe.default <- 
function(.data, 
         ...)
{
  .data
}
