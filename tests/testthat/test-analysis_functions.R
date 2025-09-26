## return_num_moves tests

test_that("return_num_moves() error with non-character objects", {
  testthat::skip_on_cran()
  negative_string <- 24

  expect_error(return_num_moves(negative_string))
})

test_that("return_num_moves() error with non-pgn text", {
  testthat::skip_on_cran()
  negative_string <- "abcdefg 12. 12"

  expect_error(return_num_moves(negative_string))
})

test_that("return_num_moves() works with pgn", {
  testthat::skip_on_cran()
  test_string <- chessdotcom_hikaru_recent |> subset(!is.na(Moves))

  expect_equal(return_num_moves(moves_string = test_string$Moves)[[1]], 19)
})


## get_game_ending/winner tests

test_that("get_game_ending() works", {
  testthat::skip_on_cran()
  chessdotcom_hikaru_recent <- get_raw_chessdotcom(usernames = "Hikaru", year_month = c(202104:202105))
  chessdotcom_hikaru_recent$Ending <- mapply(get_game_ending,
                                             termination_string = chessdotcom_hikaru_recent$Termination,
                                             white = chessdotcom_hikaru_recent$White,
                                             black = chessdotcom_hikaru_recent$Black)

  expect_type(chessdotcom_hikaru_recent$Ending, "character")
})

test_that("get_winner() works", {
  testthat::skip_on_cran()
  chessdotcom_hikaru_recent <- get_raw_chessdotcom(usernames = "Hikaru", year_month = c(202104:202105))
  chessdotcom_hikaru_recent$Winner <- mapply(get_winner,
                                             result_column = chessdotcom_hikaru_recent$Result,
                                             white = chessdotcom_hikaru_recent$White,
                                             black = chessdotcom_hikaru_recent$Black)

  expect_type(chessdotcom_hikaru_recent$Winner, "character")
})
