# REB API JSON 파싱 후 tibble 반환 (네임스페이스 명시)
#' Parsing function
#'
#' @param json_str
#' @param root_name
#'
#' @returns
#' @export
#'
#' @examples
parse_reb_api_json <- function(json_str, root_name = "SttsApiTbl") {
  x <- jsonlite::fromJSON(json_str)
  dat <- x[[root_name]]$row[[2]]
  if (is.null(dat) && !is.null(x[[root_name]]$row[[1]])) {
    dat <- x[[root_name]]$row[[1]]
  }
  if (!is.null(dat)) {
    tibble::as_tibble(dat)
  } else {
    tibble::tibble()
  }
}

#' Title
#'
#' @param json_str
#' @param root_name
#'
#' @returns
#' @export
#'
#' @examples
parse_reb_api_json_context <- function(json_str, root_name = "SttsApiTbl") {
  x <- jsonlite::fromJSON(json_str)
  dat <- x[[root_name]][[2]]$row
  if (is.null(dat)) return(tibble::tibble())
  tibble::as_tibble(dat)
}
