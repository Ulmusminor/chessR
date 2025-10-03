## Environment for avoiding calling each variable 20+ times

chessdotcom_hikaru_recent <- get_raw_chessdotcom(
  usernames = "Hikaru",
  year_month = c(202104:202105)
)

lichess_game_data <- get_raw_lichess("JaseZiv")

checkmate_test <- get_raw_lichess("checkmate")
