## get_each_player tests (TO BE DESIGNED)

test_that("get_each_player() works", {

  expect_silent(get_each_player("hikaru"))
})

## get_game_data tests

test_that("get_game_data() works", {
  testthat::skip_on_cran()
  game_data <- get_game_data(usernames = "JaseZiv")

  expect_type(game_data, "list")
  expect_true(nrow(game_data) != 0)
}) # fails so far, gives 400 warnings in the function
