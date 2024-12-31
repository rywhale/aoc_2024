#### Inputs ####
input <- readr::read_lines(
  "day20/input.txt"
) |>
  stringr::str_split("", simplify = TRUE)

#### Utils ####
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

find_neighbours <- function(input, coord, exclude_walls = TRUE, only_walls = FALSE) {
  possible_moves <- purrr::map(
    c("up", "right", "left", "down"),
    \(direction){
      next_pos <- coord + calc_delta(direction)


      if (!mat_contains(input, next_pos)) {
        NA
      } else if (exclude_walls & input[next_pos[[1]], next_pos[[2]]] == "#") {
        NA
      } else if (only_walls & input[next_pos[[1]], next_pos[[2]]] != "#") {
        NA
      } else {
        list(
          node_str = paste(next_pos, collapse = ","),
          value = input[next_pos[[1]], next_pos[[2]]]
        )
      }
    }
  ) |>
    setNames(c("up", "right", "left", "down"))

  possible_moves <- possible_moves[!is.na(possible_moves)]

  possible_moves
}

calc_path_len <- function(board, coord_df, from = "48,72", to = "64,60") {
  board[board %in% c("E", "S")] <- "."

  graph_conn <- coord_df |>
    igraph::graph_from_data_frame(directed = FALSE)

  graph_ids <- igraph::V(graph_conn) |>
    igraph::as_ids()

  if (!from %in% graph_ids | !to %in% graph_ids) {
    return(NA)
  }

  graph_p <- graph_conn |>
    igraph::shortest_paths(
      from = from,
      to = to
    )

  if (length(graph_p$vpath)) {
    path_ids <- graph_p$vpath[[1]] |>
      igraph::as_ids()

    if (!from %in% path_ids | !to %in% path_ids) {
      NA
    } else {
      length(graph_p$vpath[[1]]) - 1
    }
  } else {
    NA
  }
}

filter_walls <- function(input, wall_coord) {
  all_neigh <- find_neighbours(input, wall_coord)

  length(all_neigh) >= 2
}

#### Do it ####
all_coords <- which(
  input == ".",
  arr.ind = TRUE
) |>
  tibble::as_tibble() |>
  dplyr::mutate(
    node_str = paste0(row, ",", col)
  )

connected <- 1:nrow(all_coords) |>
  purrr::map_df(
    \(c_count){
      all_neigh <- find_neighbours(
        input,
        c(all_coords$row[[c_count]], all_coords$col[[c_count]]),
        exclude_walls = TRUE
      )

      tibble::tibble(
        from = all_coords$node_str[[c_count]],
        to = purrr::map_chr(all_neigh, "node_str")
      )
    }
  )

fastest_possible <- calc_path_len(input, connected)

all_poss <- which(
  input == "#",
  arr.ind = TRUE
) |>
  tibble::as_tibble() |>
  dplyr::rowwise() |>
  dplyr::filter(
    filter_walls(input, c(row, col))
  )

all_pairs <- 1:nrow(all_poss) |>
  purrr::map_df(
    \(node){
      node_row <- all_poss$row[[node]]
      node_col <- all_poss$col[[node]]

      input_working <- input
      input_working[node_row, node_col] <- "."

      all_neigh <- find_neighbours(
        input_working,
        c(node_row, node_col),
        exclude_walls = TRUE
      )
      
      new_conn_rows <- tibble::tibble(
        from = paste0(node_row, ",", node_col),
        to = purrr::map_chr(all_neigh, "node_str")
      )
      
      conn_temp <- connected |> 
        dplyr::bind_rows(new_conn_rows)
      
      tibble::tibble(
        node = paste0(node_row, ",", node_col),
        path_len = calc_path_len(input_working, conn_temp)
      )
    },
    .progress = TRUE
  )

scored <- all_pairs |>
  dplyr::mutate(
    len_diff = fastest_possible - path_len
  ) |>
  dplyr::filter(
    len_diff >= 100
  )

table(scored$len_diff) |>
  sum()
