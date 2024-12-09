input <- readr::read_lines(
  "day7/input.txt"
)

insert_brackets <- function(eval_str){
  input_nums <- stringr::str_split_1(eval_str, "==") |> 
    stringr::str_extract_all("\\d+\\*|\\d+\\+|\\d+$")
  
  brac_count <- length(input_nums[[2]]) - 1

  bracketed_nums <- purrr::map_chr(
    1:length(input_nums[[2]]),
    \(num_count){
      if(num_count == 1){
        paste0(
          paste(rep("(", brac_count), collapse = ""),
          input_nums[[2]][num_count]
          )
      }else if(num_count == length(input_nums[[2]])){
        paste0(
          stringr::str_extract(input_nums[[2]][num_count], "\\d+"),
          ")"
        )
      }else{
        paste0(
          stringr::str_extract(input_nums[[2]][num_count], "\\d+"), 
          ")",
          stringr::str_extract(input_nums[[2]][num_count], "\\*|\\+")
          )
      }
    }
  )
  
  paste0(
    input_nums[[1]],
    "==",
    paste(bracketed_nums, collapse = "")
  )
}

find_operators <- function(in_line) {

  heh <- stringr::str_replace(
    in_line,
    ": ",
    "=="
  )
  
  op_pos <- stringr::str_locate_all(heh, "\\s")[[1]]

  pos_subs <- expand.grid(
    rep(list(c("*", "+")), nrow(op_pos)),
    stringsAsFactors = FALSE
  )

  count  <- 1
  found_valid <- FALSE
  
  while(!found_valid & count <= nrow(pos_subs)){
    eval_str <- heh
    
    purrr::walk(
      1:nrow(op_pos),
      \(blank_row){
        substr(
          eval_str,
          start = op_pos[blank_row, ][[1]],
          stop = op_pos[blank_row, ][[2]]
        ) <<- pos_subs[count, blank_row]
      }
    )
    
    eval_str <- insert_brackets(eval_str)
    
    found_valid <- eval(parse(text = eval_str))
    count <- count + 1
  }

  if (found_valid) {
    sp <- stringr::str_split_1(
      in_line, ":"
    )

    vals <- stringr::str_extract_all(sp[[1]], "\\d+") |>
      unlist() |>
      as.numeric()

    sum(vals)
  } else {
    0
  }
}

all_vals <- purrr::map_dbl(
  input,
  find_operators,
  .progress = TRUE
)

sum(all_vals)
