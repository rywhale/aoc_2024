input <- readr::read_delim(
  "day1/input.txt",
  delim = "   ",
  col_names = c("list_1", "list_2")
)

list_diffs <- abs(sort(input$list_1) - sort(input$list_2))
sum(list_diffs)