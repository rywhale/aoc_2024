#' This is realllly not good but it worked \_0+0_/. 
#' 
#' Path to improvement:
#'   * Record past iteration paths
#'   * Pass this to route_it
#'   * Only calculate path if it hasn't already been calculated
 
input <- readr::read_lines(
  "day6/input.txt"
) |>
  stringr::str_split(
    "",
    simplify = TRUE
  )


check_straight <- function(pos, direction, input){
  
  if(direction == "up"){
    hehe <- input[1:(pos[[1]]-1), pos[[2]]]
  }else if(direction == "down"){
    hehe <- input[(pos[[1]]+1):nrow(input), pos[[2]]]
  }else if(direction == "left"){
    hehe <- input[pos[[1]], 1:(pos[[2]]-1)]
  }else{
    hehe <- input[pos[[1]], (pos[[2]]+1):ncol(input)]
  }
  
  !all(hehe == ".")
}

check_bounds <- function(pos, input){
  
  x_good <- pos[[1]] %in% seq(1, nrow(input), 1)
  y_good <- pos[[2]] %in% seq(1, ncol(input), 1)
  
  y_good & x_good
}

route_it <- function(pos, direction, input, check_s){
  
  all_pos <- tibble::tibble(
    row = pos[[1]],
    col = pos[[2]],
    direction = direction
  )
  
  is_loop <- FALSE
  
  s_check <- ifelse(
    check_s,
    check_straight(pos, direction, input),
    TRUE
  )
  
  while(check_bounds(pos, input) & s_check){
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
          row = pos[[1]],
          col = pos[[2]],
          direction = direction
        )
    }
    
    is_loop <- nrow(dplyr::distinct(all_pos)) != nrow(all_pos)
    
    if(is_loop){
      break
    }
    
    s_check <- ifelse(
      check_s,
      check_straight(pos, direction, input),
      TRUE
    )
  }
  
  list(
    all_pos,
    is_loop
  )
  
}

pos <- which(
  input == "^",
  arr.ind = TRUE
)

direction <- "up"

init_route <- route_it(
  pos,
  direction,
  input,
  check_s = FALSE
)[[1]] |> 
  dplyr::mutate(
    pos_str = paste0(row, "-", col)
  )

obs <- which(
  input == ".",
  arr.ind = TRUE
) |> 
  as.data.frame() |> 
  dplyr::mutate(
    pos_str = paste0(row, "-", col)
  ) |> 
  dplyr::filter(
    pos_str %in% init_route$pos_str
  )

future::plan(future::multisession, workers = 4)

progressr::with_progress({
  
  p <- progressr::progressor(steps = nrow(obs))
  
  loopy_loop <- furrr::future_map_lgl(
    1:nrow(obs),
    \(obs_count){
      
      p()
      
      input[obs$row[obs_count], obs$col[obs_count]] <- "#"
      
      relev_row <- which(
        init_route$row == obs$row[obs_count] & 
          init_route$col == obs$col[obs_count]
      ) - 1
      
      relev_row <- relev_row[[1]]
      
      pos <- c(init_route$row[relev_row], init_route$col[relev_row])
      direction <- init_route$direction[relev_row]
      
      route_it(
        pos = pos,
        direction = direction,
        input = input,
        check_s = TRUE
      )[[2]]
    }
  )
})

sum(loopy_loop)