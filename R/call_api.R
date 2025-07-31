# 공통 base 함수 (httr 네임스페이스 사용, 기타 base 함수도 네임스페이스 명시)
#' Title
#'
#' @param base_url
#' @param key
#' @param Type
#' @param pIndex
#' @param pSize
#' @param ...
#'
#' @returns
#' @export
#'
#' @examples
#' res1 <- get_api_key() |>
#'         reb_tbl_list(Type = "json", pIndex = 1, pSize = 5)
#'
#' res1 |> parse_reb_api_json_context()
#'
#' res1 |> parse_reb_api_json()
#'
#'
call_reb_api <- function(
    base_url,
    key = "sample key",
    Type = "xml",
    pIndex = 1,
    pSize = 5,
    ...) {

  # ...에 추가 인자 (ex. STATBL_ID 등)를 리스트로 받아서 params에 추가
  params <- base::list(KEY = key, Type = Type, pIndex = pIndex, pSize = pSize, ...)

  # GET 요청
  res <- httr::GET(base_url, query = params)

  # 상태코드 확인
  if (res$status_code == 200) {
    result <- base::rawToChar(res$content)
    base::Encoding(result) <- "UTF-8"
    # 결과를 반환(일단 문자 그대로 반환)
    return(result)
  } else {
    base::stop(base::sprintf("API 호출 실패: status code %s", res$status_code))
  }
}

# 1. SttsApiTbl: 통계표 조회 함수
reb_tbl_list <- function(
    key = "sample key",
    Type = "xml",
    pIndex = 1,
    pSize = 5,
    STATBL_ID = NULL
) {
  base_url <- "https://www.reb.or.kr/r-one/openapi/SttsApiTbl.do"
  res <- call_reb_api(base_url, key = key, Type = Type, pIndex = pIndex, pSize = pSize, STATBL_ID = STATBL_ID)
  return(res)
}

# 2. SttsApiTblItm: 통계표 항목 정보 함수
reb_tbl_item <- function(
    key = "sample key",
    Type = "xml",
    pIndex = 1,
    pSize = 5,
    STATBL_ID,
    ITM_TAG = NULL
) {
  base_url <- "https://www.reb.or.kr/r-one/openapi/SttsApiTblItm.do"
  res <- call_reb_api(base_url, key = key, Type = Type, pIndex = pIndex, pSize = pSize, STATBL_ID = STATBL_ID, ITM_TAG = ITM_TAG)
  return(res)
}

# 3. SttsApiTblData: 통계표 데이터 조회 함수
reb_tbl_data <- function(
    key = "sample key",
    Type = "xml",
    pIndex = 1,
    pSize = 5,
    STATBL_ID,
    DTACYCLE_CD,
    WRTTIME_IDTFR_ID = NULL,
    GRP_ID = NULL,
    CLS_ID = NULL,
    ITM_ID = NULL,
    START_WRTTIME = NULL,
    END_WRTTIME = NULL
) {
  base_url <- "https://www.reb.or.kr/r-one/openapi/SttsApiTblData.do"
  res <- call_reb_api(
    base_url, key = key, Type = Type, pIndex = pIndex, pSize = pSize,
    STATBL_ID = STATBL_ID, DTACYCLE_CD = DTACYCLE_CD, WRTTIME_IDTFR_ID = WRTTIME_IDTFR_ID,
    GRP_ID = GRP_ID, CLS_ID = CLS_ID, ITM_ID = ITM_ID, START_WRTTIME = START_WRTTIME, END_WRTTIME = END_WRTTIME
  )
  return(res)
}

# --- 사용 예시 ---
# res1 <- reb_tbl_list(key = "sample key", Type = "xml", pIndex = 1, pSize = 5)
# res2 <- reb_tbl_item(key = "sample key", STATBL_ID = "YOUR_TABLE_ID")
# res3 <- reb_tbl_data(key = "sample key", STATBL_ID = "YOUR_TABLE_ID", DTACYCLE_CD = "YOUR_CODE")
