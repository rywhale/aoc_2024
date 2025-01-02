#### Inputs ####
input <- readr::read_lines(
  "day25/input.txt"
)

input <- c(input, "")

#### Do it ####
chunk_ends <- which(input == "")

all_mats <- seq_along(chunk_ends) |> 
  purrr::map(
    \(chunk){
      
      if(chunk == 1){
        relev_in <- input[1:(chunk_ends[[chunk]] - 1)]
      }else{
        relev_in <- input[(chunk_ends[[chunk -1]] + 1):(chunk_ends[[chunk]] - 1)]
      }
      
      mat_out <- relev_in |> 
        stringr::str_split("", simplify = TRUE)
      
      filled_pos <- which(
        mat_out == "#",
        arr.ind = TRUE
      ) |> 
        tibble::as_tibble() |> 
        dplyr::mutate(node_str = paste0(row, "-", col))
      
      type <- ifelse(
        all(mat_out[1, ] == "."),
        "key",
        "lock"
      )
      
      list(
        avoid = filled_pos$node_str,
        type = type
      )
    }
  )

is_key <- all_mats |> 
  purrr::map_lgl(\(mat) mat$type == "key")

all_keys <- all_mats[is_key]
all_locks <- all_mats[!is_key]

all_locks |>
  purrr::map(
    \(lock){

      all_checks <- all_keys |>
        purrr::map_lgl(
          ~!any(.x$avoid %in% lock$avoid)
        )

      all_checks
    },
    .progress = TRUE
  ) |>
  unlist() |>
  sum()
