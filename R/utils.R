#시스템 환경 변수 REBONEKEY에서 데이터를 가져오고 만약에
#데이터가 없어 NULL이면 환경변수를 작성해야 한다고 알려주라.
#만약 key값을 파라미터로 넣었다면 이를 환경변수 대신 사용하고,
#환경변수에 해당 값을 등록한다.
#' Get API Key
#'
#' @param key A character string representing the API key. If provided, this will set the REBONEKEY environment variable.
#'
#' @returns A character string containing the API key.
#' @export
#'
#' @examples
get_api_key <- function(key = NULL) {
  if (!is.null(key)) {
    Sys.setenv(REBONEKEY = key)
    return(key)
  }
  api_key <- Sys.getenv("REBONEKEY", unset = "")

  if (is.null(api_key) || api_key == "") {
    stop("API key is not set. Please set the REBONEKEY environment variable or provide a key as a parameter.")
  }
  return(api_key)
}

#
