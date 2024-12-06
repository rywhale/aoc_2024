input <- readr::read_lines(
  "day6/input.txt"
) |> 
  stringr::str_split(
    "",
    simplify = TRUE
  )

# input <- c(
#   "....#.....",
#   ".........#",
#   "..........",
#   "..#.......",
#   ".......#..",
#   "..........",
#   ".#..^.....",
#   "........#.",
#   "#.........",
#   "......#..."
# ) |>
#   stringr::str_split(
#     "",
#     simplify = TRUE
#   )

check_bounds <- function(pos, input){
  
  x_good <- pos[[1]] %in% seq(0, nrow(input), 1)
  y_good <- pos[[2]] %in% seq(0, ncol(input), 1)
  
  y_good & x_good
}

pos <- which(
  input == "^",
  arr.ind = TRUE
)

direction <- "up"

all_pos <- tibble::tibble(
  x = pos[[1]],
  y = pos[[2]]
)

while(check_bounds(pos, input)){
  next_pos <- dplyr::case_when(
    direction == "up" ~ c(pos[[1]] - 1, pos[[2]]),
    direction == "down" ~ c(pos[[1]] + 1, pos[[2]]),
    direction == "left" ~ c(pos[[1]], pos[[2]] - 1),
    direction == "right" ~ c(pos[[1]], pos[[2]] + 1),
  )
  
  if(!check_bounds(next_pos, input)){
    break
  }else if(input[next_pos[[1]], next_pos[[2]]] == "#"){
    
    direction <- switch(
      direction,
      up = "right",
      down = "left",
      right = "down",
      left = "up"
    )
  }else{
    pos <- next_pos
    
    all_pos <- all_pos |> 
      tibble::add_row(
        x = pos[[1]],
        y = pos[[2]]
      )
    
    count <- count + 1
  }
}

all_pos |> 
  dplyr::distinct() |> 
  nrow()