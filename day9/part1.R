input <- readr::read_lines(
  "day9/input.txt"
)

# input <- "2333133121414131402"

pre_process <- function(input) {
  free_space_pos <- seq(2, nchar(input), 2)

  input_sp <- stringr::str_split_1(input, "")

  id_count <- 0

  out <- purrr::map(
    seq_along(input_sp),
    \(sp_pos){
      if (sp_pos %in% free_space_pos) {
        rep(".", input_sp[[sp_pos]])
      } else {
        id_count <<- id_count + 1
        rep(id_count - 1, input_sp[[sp_pos]])
      }
    }
  )

  unlist(out)
}

shift_block <- function(ready_in){
  
  all_digs <- rev(ready_in[stringr::str_detect(ready_in, "\\d+")])
  
  dot_pos <- which(ready_in == ".")
  
  working_digs <- all_digs
  
  while(min(dot_pos) < length(all_digs)){
    
    ready_in[min(dot_pos)] <- working_digs[1]
    working_digs <- working_digs[-1]
    
    dot_pos <- which(ready_in == ".")

    # For sanity
    message("Remaining dots: ", length(dot_pos))
  }
  
  ready_in[1:length(all_digs)]
}

meow <- pre_process(input) |>
  shift_block() |>
  as.numeric()

score <- purrr::map_int(
  seq_along(meow),
  \(pos){
    meow[[pos]] * (pos - 1)
  }
)

msesage(sum(score))
