#### Inputs ####
input <- readr::read_csv(
  "day18/input.txt",
  col_names = c("col", "row")
) |> 
  dplyr::mutate(
    row = row + 1,
    col = col + 1,
    node_str = paste0(row, ",", col)
  )

input <- input[1:1024, ]

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

find_neighbours <- function(input, coord) {
  
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
  possible_moves <- possible_moves[!is.na(possible_moves)]
  
  possible_moves
}

#### Get Nodes ####
board <- matrix(
  ".",
  nrow = 71,
  ncol = 71
)

all_coords <- tidyr::expand_grid(
  1:nrow(board),
  1:ncol(board)
)

names(all_coords) <- c("col", "row")

relev_coords <- all_coords |> 
  dplyr::anti_join(input) |> 
  dplyr::mutate(
    node_str = paste0(row, ",", col)
  )

connected <- 1:nrow(relev_coords) |> 
  purrr::map_df(
    \(c_count){
      all_neigh <- find_neighbours(
        board, 
        c(relev_coords$row[[c_count]], relev_coords$col[[c_count]])
      )
      
      tibble::tibble(
        from = relev_coords$node_str[[c_count]],
        to = purrr::map_chr(all_neigh, "node_str")
      )
    }
  ) |> 
  dplyr::filter(
    !from %in% input$node_str,
    !to %in% input$node_str
  )

#### Graph ####
graph_conn <- connected |> 
  igraph::graph_from_data_frame(directed = FALSE)

graph_p <- graph_conn |> 
  igraph::shortest_paths(
    from = "1,1",
    to = "71,71"
  )

p_len <- graph_p$vpath[[1]] |> 
  igraph::as_ids() |> 
  length()

message(p_len - 1)