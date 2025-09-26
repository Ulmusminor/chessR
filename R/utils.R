#' Check if a string is valid PGN
#'
#' @keywords internal
is.pgn <- function(x) {
  has_numbers <- str_detect(x, "\\b\\d+\\.")
  has_legal_moves <- str_detect(x, "\\b(?:[KQRNB])?(?:[a-h1-8])?(?:x)?[a-h][1-8](?:=[QRNB])?[+#]?\\b")

  return(has_numbers & has_legal_moves)
}

#' Extract the moves from a PGN
#'
#' @keywords internal
moves_fun <- function(x) {
  y <- x |>
    str_replace_all("\\{.*?\\}", "") |>
    str_split("\\s+") |>
    unlist() |>
    as.numeric() |>
    suppressWarnings() |>
    max(na.rm = TRUE)

  return(y)
}

#' Check status function
#'
#' @param res Response from API
#' @keywords internal
check_status <- function(res) {
  x = httr::status_code(res)

  if(x != 200) stop("The API returned an error", call. = FALSE)
}

