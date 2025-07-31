# REB API JSON 파싱 후 tibble 반환 (네임스페이스 명시)
parse_reb_api_json <- function(json_str, root_name = "SttsApiTbl") {
  x <- jsonlite::fromJSON(json_str)

  # API 구조에 따라 root name 조정
  dat <- x[[root_name]]$row[[2]]

  # 만약 row[[2]]가 없는 경우, row[[1]] 체크
  if (is.null(dat) && !is.null(x[[root_name]]$row[[1]])) {
    dat <- x[[root_name]]$row[[1]]
  }

  # 데이터가 있으면 tibble로 변환, 없으면 빈 tibble 반환
  if (!is.null(dat)) {
    tibble::as_tibble(dat)
  } else {
    tibble::tibble()
  }
}
