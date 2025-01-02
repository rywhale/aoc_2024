#' This is ungodly slow
#' Ideas: cache intermediate vals to skip calc? 

#### Inputs ####
input <- readr::read_lines(
  "day22/input.txt"
)

#### Util ####
mix_num <- function(num, sec_num){
  xor_it(num, sec_num)
}

prune_num <- function(sec_num){
  sec_num %% 16777216
}

in_da_bin <- function(num){
  
  out <- c()

  while(num != 0){
    out <- append(
      out,
      num %% 2
    )
    
    num <- floor(num / 2)
  }
  
  rev(out)
}

out_da_bin <- function(bin_nums){
  
  pow <- length(bin_nums) - 1
  pos <- 1
  
  out <- 0
  
  while(pos <= length(bin_nums)){
    out <- out + bin_nums[[pos]] * 2 ^ pow
    pow <- pow - 1
    pos <- pos + 1
  }
  
  out
}

xor_it <- function(x, y){
  
  x <- in_da_bin(x)
  y <- in_da_bin(y)
  
  while(length(x) > length(y)){
    y <- append(y, 0, after = 0)
  }
  
  while(length(y) > length(x)){
    x <- append(x, 0, after = 0)
  }
  
  out_bin <- as.numeric(x != y)
  
  out_da_bin(out_bin)
}

solve <- function(init_num, max_iter = 10){

  iter <- 0
  num <- init_num

  relev_cache <- tibble::tibble()
  
  
  while(iter < max_iter){
    
    # relev_cache <- cache |> 
    #   dplyr::filter(
    #     from == num
    #   )
    if(nrow(relev_cache)){
      
      message("Using cache")
      
      num_out <- relev_cache$to[relev_cahce$from == num]
    }else{
      num1 <- mix_num(num * 64, num) |>
        prune_num()
      
      num2 <- (num1  / 32) |>
        floor() |>
        mix_num(num1) |>
        prune_num()
      
      num_out <- (num2 * 2048) |>
        mix_num(num2) |> 
        prune_num()
    }
  
    num <- num_out
    iter <- iter + 1
  }

  # cache <<- cache |> 
  #   tibble::add_row(
  #     from = init_num,
  #     to = num
  #   )
  
  num
}

#### Solve ####
# cache <- tibble::tibble(
#   from = NA,
#   to = NA
# )

all_scores <- input |>
  as.numeric() |> 
  purrr::map_int(solve, max_iter = 2000, .progress = TRUE)

sum(all_scores)