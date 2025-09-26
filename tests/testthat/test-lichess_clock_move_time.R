## lichess_clock_move_time tests

test_that("get_lichess_clock_move_time() works", {
  testthat::skip_on_cran()
  lichess_game_data <- get_raw_lichess("JaseZiv")

  expect_type(lichess_game_data$Moves, "character")

  lichess_game_data_with_times <- lichess_clock_move_time(games_list = lichess_game_data)

  expect_type(lichess_game_data_with_times$colour, "character")
})
