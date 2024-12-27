#### Inputs ####
input <- readr::read_lines(
  "day17/input.txt"
)

# input <- c(
#   "Register A: 729",
#   "Register B: 0",
#   "Register C: 0",
#   "",
#   "Program: 0,1,5,4,3,0"
# )

#### Instructions ####
instruct_the_peasants <- function(ins, regs){
  
  output <- c()
  
  op_lookup <- function(op, regs){
    switch(
      op,
      "0" = 0,
      "1" = 1,
      "2" = 2,
      "3" = 3,
      "4" = regs$a,
      "5" = regs$b,
      "6" = regs$c
    )
  }
  
  adv <- function(op){
    regs$a <<- floor(regs$a / (2 ** op_lookup(op, regs)))
  }
  
  bxl <- function(op){
    regs$b <<- bitwXor(regs$b, as.numeric(op))
  }
  
  bst <- function(op){
    regs$b <<- op_lookup(op, regs) %% 8
  }
  
  bxc <- function(op){
    regs$b <<- bitwXor(regs$b, regs$c)
  }
  
  out <- function(op){
    output <<- append(output, op_lookup(op, regs) %% 8)
  }
  
  bdv <- function(op){
    regs$b <<- floor(regs$a / (2 ** op_lookup(op, regs)))
  }
  
  cdv <- function(op){
    regs$c <<- floor(regs$a / (2 ** op_lookup(op, regs)))
  }

  ins_sp <- stringr::str_split_1(ins, ",")
  
  ins_paired <- seq(
    from = 1,
    to = length(ins_sp),
    by = 2
  ) |> 
    purrr::map(
      \(pos){
        list(
          opcode = ins_sp[pos],
          operand = ins_sp[pos + 1]
        )
      }
    )
  
  pos <- 1
  
  while(pos <= length(ins_paired)){
    ins <- ins_paired[[pos]]
    
    if(ins$opcode == "3" & regs$a != 0){
      pos <- as.numeric(ins$operand) + 1
    }else{
      switch(
        ins$opcode,
        "0" = adv(ins$operand),
        "1" = bxl(ins$operand),
        "2" = bst(ins$operand),
        "4" = bxc(ins$operand),
        "5" = out(ins$operand),
        "6" = bdv(ins$operand),
        "7" = cdv(ins$operand)
      )
      
      pos <- pos + 1
    }
    
  }
  
  paste(output, collapse = ",")
}

#### Solve ####
regs <- list(
  a = stringr::str_extract(input[[1]], "\\d+"),
  b = stringr::str_extract(input[[2]], "\\d+"),
  c = stringr::str_extract(input[[3]], "\\d+")
) |> 
  purrr::map(as.numeric)

program <- stringr::str_extract(
  input[[5]],
  "\\d.+"
)

instruct_the_peasants(program, regs)