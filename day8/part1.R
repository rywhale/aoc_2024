input <- readr::read_lines(
  "day8/input.txt"
) |>
  stringr::str_split("", simplify = TRUE)

# input <- c(
#   "............",
#   "........0...",
#   ".....0......",
#   ".......0....",
#   "....0.......",
#   "......A.....",
#   "............",
#   "............",
#   "........A...",
#   ".........A..",
#   "............",
#   "............"
# ) |>
#   stringr::str_split("", simplify = TRUE)

find_antinode <- function(node_name, input){
  tower_coords <- which(
    input == node_name,
    arr.ind = TRUE
  ) |> 
    as.data.frame() |> 
    dplyr::mutate(
      pos_str = paste0(row, "-", col)
    )
  
  all_comb <- combn(
    tower_coords$pos_str,
    m = 2,
    simplify = FALSE
  )
  
  all_nodes <- purrr::map_df(
    all_comb,
    \(comb_pair){
      
      print(comb_pair)
      
      coord1 <- comb_pair[[1]] |> 
        stringr::str_split_1("-") |>
        as.numeric()
      
      coord2 <- comb_pair[[2]] |> 
        stringr::str_split_1("-") |>
        as.numeric()
      
      xdiff <- abs(coord1[[1]] - coord2[[1]])
      ydiff <- abs(coord1[[2]] - coord2[[2]])
      
      node1 <- coord1
      node2 <- coord2
      # X axis
      if(node1[[1]] > node2[[1]]){
        node1[[1]] <- node1[[1]] + xdiff
        node2[[1]] <- node2[[1]] - xdiff
      }else{
        node1[[1]] <- node1[[1]] - xdiff
        node2[[1]] <- node2[[1]] + xdiff
      }
      
      # Y axis
      if(node1[[2]] > node2[[2]]){
        node1[[2]] <- node1[[2]] + ydiff
        node2[[2]] <- node2[[2]] - ydiff
      }else{
        node1[[2]] <- node1[[2]] - ydiff
        node2[[2]] <- node2[[2]] + ydiff
      }
      
      tibble::tibble(
        x = c(node1[[1]], node2[[1]]),
        y = c(node1[[2]], node2[[2]])
      )
    }
  )
  
  all_nodes
}

ant_names <- input |> 
  as.character() |> 
  unique()

ant_names <- ant_names[ant_names != "."]

all_of_em <- purrr::map_df(
  ant_names,
  find_antinode,
  input
) |> 
  dplyr::filter(
    x > 0,
    y > 0,
    x <= ncol(input),
    y <= nrow(input)
  ) |> 
  dplyr::distinct()

nrow(all_of_em)