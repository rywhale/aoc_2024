input <- readr::read_lines(
  "day2/input.txt"
) |> 
  stringr::str_split(" ") |> 
  purrr::map(as.numeric)

check_valid <- function(in_vec){
  
  diffs <- in_vec - dplyr::lag(in_vec, 1)
  diffs <- diffs[!is.na(diffs)]
  
  roc_okay <- all(abs(diffs) %in% seq(1,3,1))
  sign_okay <- all(diffs > 0) | all(diffs < 0)
  
  roc_okay & sign_okay
}

fix_invalid <- function(in_vec){
 
  if(check_valid(in_vec)){
    return(TRUE)
  }
  
  all_pos <- purrr::map_lgl(
    1:length(in_vec),
    ~{
      check_valid(in_vec[-.x])
    }
  )
  
  any(all_pos)
}

is_safe <- purrr::map_lgl(
  input,
  fix_invalid
)

sum(is_safe)