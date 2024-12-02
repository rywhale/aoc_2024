input <- readr::read_delim(
  "day1/input.txt",
  delim = "   ",
  col_names = c("list_1", "list_2")
)

tots <- purrr::map_int(
  input$list_1,
  ~{
    occ <- sum(input$list_2 == .x)
    
    .x * occ
  }
)

sum(tots)