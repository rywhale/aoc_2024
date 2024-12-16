input <- readr::read_lines(
  "day10/input.txt"
) |>
  stringr::str_split("", simplify = TRUE)

# input <- c(
#   "89010123",
#   "78121874",
#   "87430965",
#   "96549874",
#   "45678903",
#   "32019012",
#   "01329801",
#   "10456732"
# ) |> 
#   stringr::str_split("", simplify = TRUE)

trail_heads <- which(input == 0, arr.ind = TRUE) |> 
  as.data.frame() |> 
  dplyr::arrange(row, col)

mat_contains <- function(input, coord){
  
  row_good <- coord[[1]] %in% seq(1, nrow(input), 1)
  col_good <- coord[[2]] %in% seq(1, ncol(input), 1)
  
  row_good & col_good
}

find_route <- function(input, x, y, current_val){
  up <- c(x, y - 1)
  down <- c(x, y + 1)
  right <- c(x + 1, y)
  left <- c(x - 1, y)
  
  next_val <- current_val + 1
  
  trail_ends <- tibble::tibble(
    row = c(NA),
    col = c(NA)
  )
  
  if(mat_contains(input, up) && input[up[[1]], up[[2]]] == next_val){
    if(next_val == 9){
      trail_ends <- trail_ends |> 
        tibble::add_row(row = up[[1]], col = up[[2]])
    }else{
      trail_ends <- find_route(input, up[[1]], up[[2]], next_val) |> 
        dplyr::bind_rows(trail_ends)
    }
  }
  
  if(mat_contains(input, down) && input[down[[1]], down[[2]]] == next_val){
    if(next_val == 9){
      trail_ends <- trail_ends |> 
        tibble::add_row(row = down[[1]], col = down[[2]])
    }else{
      trail_ends <- find_route(input, down[[1]], down[[2]], next_val) |> 
        dplyr::bind_rows(trail_ends)
    }
  }
  
  if(mat_contains(input, left) && input[left[[1]], left[[2]]] == next_val){
    if(next_val == 9){
      trail_ends <- trail_ends |> 
        tibble::add_row(row = left[[1]], col = left[[2]])
    }else{
      trail_ends <- find_route(input, left[[1]], left[[2]], next_val) |> 
        dplyr::bind_rows(trail_ends)
    }
  }
  
  if(mat_contains(input, right) && input[right[[1]], right[[2]]] == next_val){
    if(next_val == 9){
      trail_ends <- trail_ends |> 
        tibble::add_row(row = right[[1]], col = right[[2]])
    }else{
      trail_ends <- find_route(input, right[[1]], right[[2]], next_val) |> 
        dplyr::bind_rows(trail_ends)
    }
  }
  
  trail_ends
}

all_counts <- purrr::map_df(
  seq_len(nrow(trail_heads)),
  \(head_row){
    find_route(
      input = input, 
      x = trail_heads[head_row, 1],
      y = trail_heads[head_row, 2],
      current_val = 0
    ) |> 
      dplyr::mutate(
        start = paste0(
          trail_heads[head_row, 1],
          "-",
          trail_heads[head_row, 2]
        )
      )
  }
)

all_counts |>
  dplyr::filter(!is.na(row)) |> 
  dplyr::distinct() |> 
  nrow()
