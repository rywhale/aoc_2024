#### Input ####
input <- readr::read_lines(
  "day19/input.txt"
)

#### Util ####
recursive_check <- function(design, pattern_cache){

  if(design %in% pattern_cache$pattern){
    return(list(is_valid = TRUE, pattern_cache = pattern_cache))
  }
  
  start_df <- pattern_cache |> 
    dplyr::arrange(dplyr::desc(patt_len)) |> 
    dplyr::filter(
      stringr::str_detect(design, paste0("^", pattern))
    )
  
  if(nrow(start_df)){
    
    working_strs <- stringr::str_remove(design, paste0("^", start_df$pattern))
    
    if(any(working_strs == "")){
      pattern_cache <- pattern_cache |> 
        tibble::add_row(
          pattern = design, 
          patt_len = nchar(design)
          )
      
      return(list(is_valid = TRUE, pattern_cache = pattern_cache))
    }
    
    for(str in working_strs){
      deep <- recursive_check(str, pattern_cache)
      
      if(deep$is_valid){
        return(list(is_valid = TRUE, pattern_cache = deep$pattern_cache))
      }
    }
  }
  
  list(
    is_valid = FALSE,
    pattern_cache = pattern_cache
  )
}

#### Do it ####
towel_patterns <- input[[1]] |> 
  stringr::str_extract_all("[a-z]+") |> 
  purrr::pluck(1) |>
  tibble::tibble() |>
  dplyr::rename("pattern" = 1) |> 
  dplyr::rowwise() |> 
  dplyr::mutate(
    patt_len = nchar(pattern)
  )

all_designs <- input[3:length(input)]

checks <- all_designs |>
  purrr::map_lgl(
    \(des){
      res <- recursive_check(des, towel_patterns)
      towel_patterns <<- res$pattern_cache
      
      res$is_valid
    },
    .progress = TRUE
)

sum(checks)
