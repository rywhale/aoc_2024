input <- readr::read_lines(
  "day4/input.txt"
) |>
  stringr::str_split(
    "",
    simplify = TRUE
  )

col_strs <- purrr::map_chr(
  1:ncol(input),
  ~{
    paste(input[, .x], collapse = "")
  }
)

row_strs <- purrr::map_chr(
  1:nrow(input),
  ~{
    paste(input[.x, ], collapse = "")
  }
)

diag_strs <- split(
  input,
  row(input) - col(input)
) |> 
  purrr::map_chr(paste, collapse ="")

revdiag_strs <- split(
  input,
  row(input) + col(input)
) |> 
  purrr::map_chr(paste, collapse ="")

all_pos <- c(
  col_strs,
  row_strs,
  diag_strs,
  revdiag_strs
)

all_pos <- c(
  all_pos,
  stringi::stri_reverse(all_pos)
)

all_pos |>
  stringr::str_count("XMAS") |> 
  sum()
