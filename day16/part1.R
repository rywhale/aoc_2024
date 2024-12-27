#### Inputs ####
input <- readr::read_lines(
  "day16/input.txt"
) |>
  stringr::str_split("", simplify = TRUE)

#### Util ####
mat_contains <- function(input, coord) {
  row_good <- coord[[1]] %in% seq(1, nrow(input), 1)
  col_good <- coord[[2]] %in% seq(1, ncol(input), 1)

  row_good & col_good
}

calc_delta <- function(direction) {
  switch(direction,
    "up" = c(-1, 0),
    "down" = c(1, 0),
    "right" = c(0, 1),
    "left" = c(0, -1)
  )
}

find_neighbours <- function(input, coord, start_dir) {
  backwards <- switch(
    up = "down",
    down = "up",
    left = "right",
    right = "left"
  )

  possible_moves <- purrr::map(
    c("up", "right", "left", "down"),
    \(direction){
      next_pos <- coord + calc_delta(direction)

      
      if(!mat_contains(input, next_pos)){
        NA
      }else{
        list(
          node_str = paste(next_pos, collapse = ","),
          value = input[next_pos[[1]], next_pos[[2]]]
        )
      }
    }
  ) |>
    setNames(c("up", "right", "left", "down"))

  possible_moves <- possible_moves[possible_moves != "#"]
  possible_moves <- possible_moves[possible_moves != backwards]
  possible_moves <- possible_moves[!is.na(possible_moves)]

  possible_moves
}

#### Do it ####
dj_khaled <- function(input_working) {
  all_paths <- which(
    input == ".",
    arr.ind = TRUE
  ) |>
    as.data.frame()

  start_pos <- which(
    input == "S",
    arr.ind = TRUE
  ) |>
    as.data.frame()

  end_pos <- which(
    input == "E",
    arr.ind = TRUE
  ) |>
    as.data.frame()

  bookie <- start_pos |>
    dplyr::bind_rows(
      all_paths,
      end_pos
    ) |>
    dplyr::mutate(
      direction = NA,
      visited = FALSE,
      score = Inf,
      node_str = paste0(row, ",", col)
    )

  bookie$visited[[1]] <- TRUE
  bookie$score[[1]] <- 0
  bookie$direction[[1]] <- "up"
  
  current_pos <- start_pos
  current_dir <- "up"
  
  prev_score <- 1001
  
  while (!all(bookie$visited)) {
    
    current_node_str <- paste(current_pos, collapse = ",")
    
    bookie$visited[bookie$node_str == current_node_str] <- TRUE
    
    # Find closest to current
    possible_neigh <- find_neighbours(
      input_working,
      current_pos,
      current_dir
    )

    if("E" %in% unlist(possible_neigh)){
      break
    }
    
    neigh_score <- ifelse(
      names(possible_neigh) == current_dir, 1, 1001 
    )
    
    # Update neighbor distances
    names(possible_neigh) |> 
      seq_along() |> 
      purrr::walk(
        \(n_count){
          
          if(possible_neigh[[n_count]]$value != "#"){
            loop_str <- possible_neigh[[n_count]]$node_str
            loop_score <- neigh_score[[n_count]] + prev_score
            
            if(loop_score < bookie$score[bookie$node_str == loop_str]){
              bookie$score[bookie$node_str == loop_str] <<- loop_score
              bookie$direction[bookie$node_str == loop_str] <<- names(possible_neigh)[[n_count]]
            }
          }
        }
      )
      
    next_pos <- bookie |> 
      dplyr::filter(!visited) |> 
      dplyr::arrange(score)
    
    # Update
    current_pos <- c(
      next_pos$row[[1]],
      next_pos$col[[1]]
    )
      
    current_dir <- next_pos$direction[[1]]
    prev_score <- next_pos$score[[1]]
  }

  bookie
}

dj_khaled(input) |> 
  dplyr::filter(
    visited
  ) |> 
  dplyr::pull(score) |> 
  max()
