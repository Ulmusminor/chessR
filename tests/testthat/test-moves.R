## extract_moves tests

test_that("extract_moves() errors on an empty input", {
  testthat::skip_on_cran()

  expect_error(extract_moves(""),  "the provided string must be a pgn")
})

test_that("extract_moves() errors with non pgn string", {
  testthat::skip_on_cran()

  expect_error(extract_moves(123), "the provided string must be a pgn")
  # probably want to change this error to throw something else
})

test_that("extract_moves() errors with multiple moves strings", {
  testthat::skip_on_cran()

  expect_error(extract_moves(chessdotcom_raw_hikaru[, "Moves"]), "only a single moves string can be provided")
})

test_that("extract_moves() produces a data.frame of moves", {
  move_df <- extract_moves(chessdotcom_raw_hikaru[1, "Moves"])

  testthat::skip_on_cran()
  expect_s3_class(move_df, "data.frame")
  expect_identical(colnames(move_df), c("white", "black"))
  expect_equal(nrow(move_df), 19)
  expect_true(grepl("e4", move_df[1, 1]))
  expect_true(grepl("g6", move_df[1, 2]))
})


## extract_moves_as_game tests (tbd)


## plot_moves tests (tbd)


## extract_moves_from_pgn tests (tbd)
