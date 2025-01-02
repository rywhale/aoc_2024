#### Inputs ####
input <- readr::read_lines(
  "day24/input.txt"
)

#### Utils ####
out_da_bin <- function(bin_nums) {
  pow <- length(bin_nums) - 1
  pos <- 1

  out <- 0

  while (pos <= length(bin_nums)) {
    out <- out + bin_nums[[pos]] * 2^pow
    pow <- pow - 1
    pos <- pos + 1
  }

  out
}

compute_the_number <- function(wire_1, wire_2, ins) {
  out_val <- switch(ins,
    AND = wire_1 == 1 & wire_2 == 1,
    OR = wire_1 == 1 | wire_2 == 1,
    XOR = wire_1 != wire_2
  )

  as.numeric(out_val)
}

#### Do it ####
ins_lines <- input[stringr::str_detect(input, "->")]

ins_df <- tibble::tibble(
  wire_1 = stringr::str_extract(ins_lines, "[a-zA-Z0-9]+"),
  wire_2 = stringr::str_extract(ins_lines, "[a-zA-Z0-9]+(?=\\s->)"),
  ins = stringr::str_extract(ins_lines, "AND|OR|XOR"),
  results = stringr::str_extract(ins_lines, "(?<=->\\s)[a-zA-Z0-9]+")
)

init_vals <- input[stringr::str_detect(input, ":")] |>
  tibble::tibble() |>
  tidyr::separate(1, into = c("wire", "value"), ": ") |>
  dplyr::mutate(value = as.numeric(value))


remaining_ins <- ins_df
current_row <- 1

while (nrow(remaining_ins)) {
  if (current_row > nrow(remaining_ins)) {
    current_row <- 1
  }

  current_ins <- remaining_ins[current_row, ]

  can_compute <- all(c(current_ins$wire_1, current_ins$wire_2) %in% init_vals$wire)

  if (can_compute) {
    wire_1_val <- init_vals$value[init_vals$wire == current_ins$wire_1]
    wire_2_val <- init_vals$value[init_vals$wire == current_ins$wire_2]

    calc_value <- compute_the_number(
      wire_1_val,
      wire_2_val,
      current_ins$ins
    )

    remaining_ins <- remaining_ins[-current_row, ]

    init_vals <- init_vals |>
      tibble::add_row(
        wire = current_ins$results,
        value = calc_value
      )
  } else {
    current_row <- current_row + 1
  }
}

init_vals |>
  dplyr::filter(
    stringr::str_detect(wire, "^z[0-9]+")
  ) |>
  dplyr::arrange(wire) |>
  dplyr::pull(value) |>
  rev() |>
  out_da_bin()
