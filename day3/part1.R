input <- readr::read_lines(
  "day3/input.txt"
)

mul_ins <- stringr::str_extract_all(
  input,
  "mul\\(\\d{1,3},\\d{1,3}\\)"
) |> 
  unlist()

res <- purrr::map_int(
  mul_ins,
  ~{
    digs <- stringr::str_extract_all(
      .x, "\\d+"
    ) |> 
      unlist() |> 
      as.numeric()
    
    digs[[1]] * digs[[2]]
  }
)

sum(res)