input <- readr::read_lines(
  "day13/input.txt"
)

optimus_prime <- function(x1, x2, dest, axis = "x") {
  all_pos <- expand.grid(
    val1 = seq(1, 100, 1),
    val2 = seq(1, 100, 1)
  )

  scores <- purrr::map(
    1:nrow(all_pos),
    \(pos){
      sol <- x1 * all_pos$val1[[pos]] + (x2 * all_pos$val2[[pos]])

      if (sol == dest) {
        tibble::tibble(
          pos1 = all_pos$val1[[pos]],
          pos2 = all_pos$val2[[pos]]
        )
      } else {
        NA
      }
    }
  )

  if(all(is.na(scores))){
    return(NA)
  }else{
    scores <- scores[!is.na(scores)] |>
      dplyr::bind_rows() |>
      dplyr::mutate(
        poscost = 3 * pos1 + pos2
      )
    
    names(scores) <- names(scores) |>
      stringr::str_replace("pos", axis)
  }
  
  scores
}

meow <- optimus_prime(94, 22, 8400)

process_input <- function(input) {
  chunk_starts <- stringr::str_which(input, "Button A")

  purrr::map(
    chunk_starts,
    \(chunk_start){
      a_coords <- stringr::str_extract_all(
        input[chunk_start],
        "\\d+"
      )[[1]] |>
        as.numeric()

      b_coords <- stringr::str_extract_all(
        input[chunk_start + 1],
        "\\d+"
      )[[1]] |>
        as.numeric()

      prize_coords <- stringr::str_extract_all(
        input[chunk_start + 2],
        "\\d+"
      )[[1]] |>
        as.numeric()

      optimized_x <- optimus_prime(
        a_coords[[1]],
        b_coords[[1]],
        prize_coords[[1]],
        "x"
      )

      optimized_y <- optimus_prime(
        a_coords[[2]],
        b_coords[[2]],
        prize_coords[[2]],
        "y"
      )
      
      if(all(is.na(optimized_x)) | all(is.na(optimized_y))){
        return(NA)
      }
      
      all_opt <- optimized_x |>
        dplyr::left_join(
          optimized_y,
          by = c("x1" = "y1", "x2" = "y2")
          ) |> 
        dplyr::filter(!is.na(ycost))
    },
    .progress = TRUE
  )
}

did_it <- process_input(input)

did_it <- did_it[!is.na(did_it)] |> 
  dplyr::bind_rows()

sum(did_it$xcost)
