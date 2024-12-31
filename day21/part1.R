#### Inputs ####
input <- readr::read_lines(
  "day21/input.txt"
)

# input <- c(
#   "029A",
#   "980A",
#   "179A",
#   "456A",
#   "379A"
# )

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


      if (!mat_contains(input, next_pos)) {
        NA
      } else if (is.na(input[next_pos[[1]], next_pos[[2]]])) {
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

make_dirpad_graph <- function() {
  pad_mat <- matrix(
    data = c(NA, "<", "^", "v", "A", ">"),
    nrow = 2,
    ncol = 3
  )

  all_coords <- which(
    !is.na(pad_mat),
    arr.ind = TRUE
  ) |>
    tibble::as_tibble() |>
    dplyr::rowwise() |>
    dplyr::mutate(
      # node_str = paste0(row, ",", col),
      node_val = pad_mat[row, col]
    )

  connected <- 1:nrow(all_coords) |>
    purrr::map_df(
      \(c_count){
        all_neigh <- find_neighbours(
          pad_mat,
          c(all_coords$row[[c_count]], all_coords$col[[c_count]])
        )

        tibble::tibble(
          from = all_coords$node_val[[c_count]],
          to = purrr::map_chr(all_neigh, "value")
        )
      }
    )

  g <- connected |>
    igraph::graph_from_data_frame(directed = FALSE)

  list(
    graph = g,
    mat = pad_mat
  )
}

make_numpad_graph <- function() {
  pad_mat <- matrix(
    data = c(7, 4, 1, NA, 8, 5, 2, 0, 9, 6, 3, "A"),
    nrow = 4,
    ncol = 3
  )

  all_coords <- which(
    !is.na(pad_mat),
    arr.ind = TRUE
  ) |>
    tibble::as_tibble() |>
    dplyr::rowwise() |>
    dplyr::mutate(
      node_val = pad_mat[row, col]
    )

  connected <- 1:nrow(all_coords) |>
    purrr::map_df(
      \(c_count){
        all_neigh <- find_neighbours(
          pad_mat,
          c(all_coords$row[[c_count]], all_coords$col[[c_count]])
        )

        tibble::tibble(
          from = all_coords$node_val[[c_count]],
          to = purrr::map_chr(all_neigh, "value")
        )
      }
    )

  g <- connected |>
    igraph::graph_from_data_frame(directed = FALSE)

  list(
    graph = g,
    mat = pad_mat
  )
}

build_pad_lookup <- function(relev_edges) {
  numpad <- make_numpad_graph()
  dirpad <- make_dirpad_graph()

  all_comb <- tidyr::expand_grid(
    c(0:9, "A"),
    c(0:9, "A")
  ) |>
    dplyr::rename(
      "from" = 1,
      "to" = 2
    ) |>
    dplyr::mutate(
      edge_name = paste0(from, "-", to)
    ) |>
    dplyr::filter(
      from != to,
      edge_name %in% relev_edges
    )

  # First abstraction
  update1 <- all_comb |>
    dplyr::rowwise() |>
    dplyr::mutate(
      path_str = list(combine_paths(calc_pad_path2(numpad, c(from, to))))
    ) |>
    tidyr::unnest_longer(path_str)

  # Second
  update2 <- update1 |>
    dplyr::rowwise() |>
    dplyr::mutate(
      path_str2 = list(combine_paths(calc_pad_path2(dirpad, c("A", stringr::str_split_1(path_str, "")))))
    ) |>
    tidyr::unnest_longer(path_str2)
  
  update3 <- update2 |>
    dplyr::rowwise() |>
    dplyr::mutate(
      path_str3 = list(combine_paths(calc_pad_path2(dirpad, c("A", stringr::str_split_1(path_str2, "")))))
    ) |>
    tidyr::unnest_longer(path_str3)

  update3
}


calc_pad_path2 <- function(pad_list, steps){
  1:(length(steps ) - 1) |>
    purrr::map(
      \(res_pos){
        
        if (steps[[res_pos]] == steps[[res_pos + 1]]) {
          return("A")
        }
        
        node_p <- igraph::all_shortest_paths(
          pad_list$graph,
          from = steps[[res_pos]],
          to = steps[[res_pos + 1]]
        )
        
        node_p$vpaths |>
          purrr::map(
            \(vpath){
              all_ids <- vpath |>
                igraph::as_ids()
              
              paste(c(get_direction(pad_list$mat, all_ids), "A"), collapse = "")
            }
          ) |>
          unique()
      }
    )
}

get_direction <- function(mat, nodes) {
  purrr::map_chr(
    1:(length(nodes) - 1),
    \(node_pos){
      id1_pos <- which(
        mat == nodes[[node_pos]],
        arr.ind = TRUE
      )

      id2_pos <- which(
        mat == nodes[[node_pos + 1]],
        arr.ind = TRUE
      )

      id_diffs <- id2_pos - id1_pos

      if (id_diffs[[1]] == 1) {
        "v"
      } else if (id_diffs[[1]] == -1) {
        "^"
      } else if (id_diffs[[2]] == 1) {
        ">"
      } else {
        "<"
      }
    }
  )
}

combine_paths <- function(path_list) {
  path_lens <- path_list |>
    purrr::map_int(length)

  if (all(path_lens == 1)) {
    return(c(paste(unlist(path_list), collapse = "")))
  }

  out <- c("")

  path_list |>
    purrr::walk(
      \(pchunk){
        out <<- purrr::map(out, ~ paste0(.x, pchunk)) |> unlist()
      }
    )

  out
}

#### Do it ####
pad_lookup <- paste0("A", input) |>
  purrr::map(
    \(in_line){
      in_nodes <- stringr::str_split_1(in_line, "")

      c(
        paste0(in_nodes[[1]], "-", in_nodes[[2]]),
        paste0(in_nodes[[2]], "-", in_nodes[[3]]),
        paste0(in_nodes[[3]], "-", in_nodes[[4]]),
        paste0(in_nodes[[4]], "-", in_nodes[[5]])
      )
    }
  ) |>
  unlist() |>
  build_pad_lookup()

all_scores <- paste0("A", input) |>
  purrr::map_int(
    \(code){

      code_nodes <- code |> 
        stringr::str_split_1("")
      
      num_portion <- code |> 
        stringr::str_extract_all(
          "\\d+"
        ) |> 
        purrr::pluck(1) |> 
        paste(collapse = "")
      
      edge_scores <- 1:(length(code_nodes) - 1) |>
        purrr::map_int(
          \(node_pos){

            relev_lookup <- pad_lookup |>
              dplyr::filter(
                from == code_nodes[[node_pos]],
                to == code_nodes[[node_pos + 1]]
              )

            min(nchar(relev_lookup$path_str3))
          }
        )

      sum(edge_scores) * as.numeric(num_portion)
    }
  )

sum(all_scores)
