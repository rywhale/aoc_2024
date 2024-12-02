input <- readr::read_lines(
  "day2/input.txt"
)

is_safe <- purrr::map_lgl(
  input,
  ~{
    in_sp <- stringr::str_split_1(.x, " ") |> 
      as.numeric()
    
    diffs <- in_sp - dplyr::lag(in_sp, 1)
    diffs  <- diffs[!is.na(diffs)]
    
    change_okay <- all(abs(diffs) %in% seq(1, 3, 1))
    sign_okay <- all(diffs > 0) | all(diffs < 0)
    
    change_okay & sign_okay
  }
)

sum(is_safe)