input <- readr::read_lines(
  "day5/input.txt"
)

rules <- tibble::tibble(
  rule_in = input[stringr::str_detect(input, "\\|")]
) |> 
  tidyr::separate(
    rule_in,
    c("first", "second"),
    sep = "\\|"
  )

updates <- input[stringr::str_detect(input, ",")] |> 
  stringr::str_split(",") |> 
  purrr::map(as.numeric)

check_valid <- function(x, before, after, rulebook){
  
  after_rules <- rulebook |> 
    dplyr::filter(
      first == x
    )
  
  before_rules <- rulebook |> 
    dplyr::filter(
      second == x
    )
  
  all(after %in% after_rules$second) & all(before %in% before_rules$first)
}

fix_update <- function(update, rulebook){
  
  rulebook <- rulebook |> dplyr::filter(
    first %in% update
  )
  
  is_first <- purrr::map_lgl(
    update,
    \(num){
      rem <- update[update != num]
      
      num_rules <- rulebook |> 
        dplyr::filter(
          first == num
        )
      
      all(rem %in% num_rules$second)
    }
  )
  
  fixed <- c(update[is_first])
  
  while(length(fixed) < length(update)){
    
    relev <- update[!update %in% fixed]
    
    is_next <- purrr::map_lgl(
      relev,
      \(num){
        rem <- relev[relev != num]
        
        num_rules <- rulebook |> 
          dplyr::filter(
            first == num
          )
        
        all(rem %in% num_rules$second)
      }
    )
    
    fixed <- c(
      fixed,
      relev[is_next]
    )
    
  }
  
  fixed
}

valid_centers <- purrr::map_int(
  updates,
  \(update){
    
    is_valid <- purrr::map_lgl(
      1:length(update),
      \(x){
        
        before <- c()
        after <- c()
        
        if(x != 1){
          before <- update[1:(x -1)]
        }
        
        if(x != length(update)){
          after <- update[(x+1):length(update)]
        }
        
        is_valid <- check_valid(
          update[[x]],
          before = before,
          after = after,
          rulebook = rules
        )
      }
    )
    
    if(all(is_valid)){
      0
    }else{
      fixed <- fix_update(update, rulebook = rules)
      fixed[length(fixed) / 2 + 1]
    }
    
  },
  .progress = TRUE
)

sum(valid_centers)