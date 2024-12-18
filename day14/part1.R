input <- readr::read_lines(
  "day14/input.txt"
)

calc_end_pos <- function(start_col, start_row, 
                         delta_col, delta_row, t_max){
  board <- matrix(
    0,
    nrow = 103,
    ncol = 101
  )
  
  t <- 0
  current_col <- start_col
  current_row <- start_row
  
  while(t < t_max){
    new_col <- current_col + delta_col
    new_row <- current_row + delta_row
    
    if(new_row == 0){
      new_row <- nrow(board)
    }else if(new_row < 0){
      new_row <- nrow(board) - abs(new_row)
    }else if(new_row > nrow(board)){
      new_row <- new_row - nrow(board)
    }
    
    if(new_col == 0){
      new_col <- ncol(board)
    }else if(new_col < 0){
      new_col <- ncol(board) - abs(new_col)
    }else if(new_col > ncol(board)){
      new_col <- new_col - ncol(board)
    }
    
    current_col <- new_col
    current_row <- new_row

    t <- t + 1
  }
  
  board[current_row, current_col] <- 1
  
  board
}

all_mats <- input |>
  purrr::map(
    \(in_line){
      digs <- stringr::str_extract_all(in_line, "-\\d+|\\d+")[[1]] |>
        as.numeric()
      
      calc_end_pos(
        # Plus one because 0 indexed
        start_col = digs[[1]] + 1,
        start_row = digs[[2]] + 1,
        delta_col = digs[[3]],
        delta_row = digs[[4]],
        t_max = 100
      )
    }
  )

all_pos <- Reduce(`+`, all_mats)

col_break <- median(1:ncol(all_pos))
row_break <- median(1:nrow(all_pos))

q1 <- all_pos[1:(row_break - 1), 1:(col_break) - 1]
q2 <- all_pos[(row_break + 1):nrow(all_pos), 1:(col_break) - 1]
q3 <- all_pos[1:(row_break - 1), (col_break + 1):ncol(all_pos)]
q4 <- all_pos[(row_break + 1):nrow(all_pos), (col_break + 1):ncol(all_pos)]

sum(q1) * sum(q2) * sum(q3) * sum(q4)
