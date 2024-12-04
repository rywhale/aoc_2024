input <- readr::read_lines(
  "day4/input.txt"
) |>
  stringr::str_split(
    "",
    simplify = TRUE
  )

find_christmas_spirit <- function(input, a_x, a_y){
  
  x_start <- a_x - 1
  x_end <- a_x + 1
  y_start <- a_y - 1
  y_end <- a_y + 1
  
  relev_mat <- input[x_start:x_end, y_start:y_end]
  
  relev_mat
  
}

a_loc <- which(
  input == "A",
  arr.ind = TRUE
) |> 
  as.data.frame() |> 
  dplyr::filter(
    row %in% seq(2, nrow(input) -1, 1),
    col %in% seq(2, ncol(input) -1, 1)
  )

all_mat <- purrr::map(
  1:nrow(a_loc),
  ~{
    find_christmas_spirit(input, a_loc[.x, 1], a_loc[.x, 2])
  }
)

xmas_count <- purrr::map_int(
  all_mat,
  ~{
    
    diag_strs <- split(
      .x,
      row(.x) - col(.x)
    ) |> 
      purrr::map_chr(paste, collapse ="")
    
    rev_diag_strs <- split(
      .x,
      row(.x) + col(.x)
    ) |> 
      purrr::map_chr(paste, collapse ="")
    
    all_strs <- c(
      diag_strs,
      rev_diag_strs
    )
    
    sum(all_strs %in% c("MAS", "SAM")) == 2
  }
)

sum(xmas_count)
