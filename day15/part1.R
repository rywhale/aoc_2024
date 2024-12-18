input <- readr::read_lines(
  "day15/input.txt"
)

instructions <- input[stringr::str_detect(input, "\\^|\\<|\\>|v")] |> 
  paste(collapse = "") |> 
  stringr::str_split_1("")

map_in <- input[stringr::str_detect(input, "#")] |> 
  stringr::str_split("", simplify = TRUE)

mat_contains <- function(input, coord){
  
  row_good <- coord[[1]] %in% seq(2, nrow(input) - 1, 1)
  col_good <- coord[[2]] %in% seq(2, ncol(input) - 1, 1)
  
  row_good & col_good
}

move_boxes <- function(robo_map, box_pos, direction){
  
  delta <- switch(
    direction,
    "^" = c(-1, 0),
    "v" = c(1, 0),
    ">" = c(0, 1),
    "<" = c(0, -1)
  )
  
  potential_pos <- box_pos + delta
  
  if(!mat_contains(robo_map, potential_pos)){
    return(robo_map)
  }
  
  pot_pos_str <- robo_map[potential_pos[[1]], potential_pos[[2]]]
  
  while(pot_pos_str == "O"){
    potential_pos <- potential_pos + delta
    pot_pos_str <- robo_map[potential_pos[[1]], potential_pos[[2]]]
  }
  
  if(pot_pos_str != "#"){
    robo_map[potential_pos[[1]], potential_pos[[2]]] <- "O"
    robo_map[box_pos[[1]], box_pos[[2]]] <- "."
  }
  
  robo_map
}

move_robot <- function(robo_map, direction){
  
  # print("In: ")
  # print(robo_map)
  # print(paste0("Direction: ", direction))
  
  delta <- switch(
    direction,
    "^" = c(-1, 0),
    "v" = c(1, 0),
    ">" = c(0, 1),
    "<" = c(0, -1)
  )
  
  box_pos <- which(
    robo_map == "O", 
    arr.ind = TRUE
    ) |> 
    as.data.frame()
  
  
  current_pos <- which(robo_map == "@", arr.ind = TRUE)
  
  next_pos <- c(current_pos[[1]], current_pos[[2]]) + delta
  
  if(!mat_contains(robo_map, next_pos)){
    return(robo_map)
  }
  
  next_char <- robo_map[next_pos[[1]], next_pos[[2]]]
  
  if(next_char == "O"){
    robo_map <- move_boxes(robo_map, next_pos, direction)
  }
  
  next_char <- robo_map[next_pos[[1]], next_pos[[2]]]

  if(!next_char %in% c("O", "#")){
    robo_map[next_pos[[1]], next_pos[[2]]] <- "@"
    robo_map[current_pos[[1]], current_pos[[2]]] <- "."
  }
  
  # print("OUT:")
  # print(robo_map)
  
  robo_map
}

map_out <- map_in

instructions |> 
  purrr::walk(
    \(ins){
      map_out <<- move_robot(
        robo_map = map_out,
        direction = ins
      )
    }
  )

to_score <- which(
  map_out == "O",
  arr.ind = TRUE
) |> 
  as.data.frame() |> 
  dplyr::mutate(
    score = 100 * (row - 1) + (col - 1)
  )

sum(to_score$score)
