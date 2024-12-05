input <- readr::read_lines(
  "day5/input.txt"
)

# input <- c(
#   "47|53",
#   "97|13",
#   "97|61",
#   "97|47",
#   "75|29",
#   "61|13",
#   "75|53",
#   "29|13",
#   "97|29",
#   "53|29",
#   "61|53",
#   "97|53",
#   "61|29",
#   "47|13",
#   "75|47",
#   "97|75",
#   "47|61",
#   "75|61",
#   "47|29",
#   "75|13",
#   "53|13",
#   "",
#   "75,47,61,53,29",
#   "97,61,53,29,13",
#   "75,29,13",
#   "75,97,47,61,53",
#   "61,13,29",
#   "97,13,75,29,47"
# )

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
      update[length(update) / 2 + 1]
    }else{
      0
    }
    
  }
)

sum(valid_centers)