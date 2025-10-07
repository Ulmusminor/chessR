## chessdotcom_leaderboard tests

test_that("chessdotcomleaderboard() works", {
  testthat::skip_on_cran()
  chessdotcom_leaders <- chessdotcom_leaderboard(game_type = "daily")
  expect_type(chessdotcom_leaders, "list")
  expect_true(nrow(chessdotcom_leaders) != 0)
})


## lichess_leaderboard tests

test_that("lichess_leaderboard() works", {
  testthat::skip_on_cran()
  lichess_leaders <- lichess_leaderboard(top_n_players = 10, speed_variant = "blitz")
  expect_type(lichess_leaders, "list")
  expect_true(nrow(lichess_leaders) != 0)
})
