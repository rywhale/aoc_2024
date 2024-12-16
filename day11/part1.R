input <- readr::read_lines("day11/input.txt") |>
  stringr::str_split_1(" ") |>
  as.numeric()

stone_it <- function(stone){
  if(stone == 0){
    1
  }else if(nchar(stone) %% 2 == 0){

    if(nchar(stone) == 2){
      first <- substr(stone, 1, 1)
      second <- substr(stone, 2, 2)
    }else{
      first <- substr(stone, 1, nchar(stone) / 2)
      second <- substr(stone, nchar(stone) / 2 + 1, nchar(stone))
    }

    as.numeric(c(first, second))
  }else{
    stone * 2024
  }

}

# Part 1
blinks <- 0
stones <- input

while(blinks < 25){
  message("You've blinked ", blinks, " times")

  stones <- stones |>
    purrr::map(
      stone_it
    ) |>
    unlist()

  blinks <- blinks + 1
}

message("Total stones: ", length(stones))
