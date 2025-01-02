#### Inputs ####
input <- readr::read_lines(
  "day23/input.txt"
)

# input <- c(
#   "kh-tc",
#   "qp-kh",
#   "de-cg",
#   "ka-co",
#   "yn-aq",
#   "qp-ub",
#   "cg-tb",
#   "vc-aq",
#   "tb-ka",
#   "wh-tc",
#   "yn-cg",
#   "kh-ub",
#   "ta-co",
#   "de-co",
#   "tc-td",
#   "tb-wq",
#   "wh-td",
#   "ta-ka",
#   "td-qp",
#   "aq-cg",
#   "wq-ub",
#   "ub-vc",
#   "de-ta",
#   "wq-aq",
#   "wq-vc",
#   "wh-yn",
#   "ka-de",
#   "kh-ta",
#   "co-tc",
#   "wh-qp",
#   "tb-vc",
#   "td-yn"
# )

#### Utils ####
check_connects <- function(node1, node2, node_conn) {
  relev_conns <- node_conn |>
    dplyr::filter(
      (from == node1 & to == node2) | (from == node2 & to == node1)
    )

  nrow(relev_conns) != 0
}

find_groups <- function(node, node_conn, group_size = 3) {
  relev_conns <- node_conn |>
    dplyr::filter(
      from == node | to == node
    )

  relev_nodes <- unique(c(relev_conns$from, relev_conns$to))

  relev_nodes <- relev_nodes[relev_nodes != node]

  all_comb <- combn(relev_nodes, m = group_size - 1) |>
    t() |>
    tibble::as_tibble() |>
    dplyr::rename(
      "node_2" = 1,
      "node_3" = 2
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      node_1 = node,
      node_str = paste(sort(c(node_1, node_2, node_3)), collapse = "-")
    ) |>
    dplyr::distinct(node_str, .keep_all = TRUE) |>
    dplyr::mutate(
      node_1to2 = check_connects(node_1, node_2, node_conn),
      node_2to3 = check_connects(node_2, node_3, node_conn),
      node_1to3 = check_connects(node_1, node_3, node_conn)
    )

  all_comb |>
    dplyr::filter(
      node_1to2 & node_2to3 & node_1to3 &
        stringr::str_detect(node_str, "^t|-t")
    ) |>
    dplyr::pull(node_str)
}

#### Do it ####
input <- input |>
  tibble::tibble() |>
  tidyr::separate(
    input,
    into = c("from", "to"),
    sep = "-"
  )

all_nodes <- unique(c(input$from, input$to))

all_cons <- all_nodes |>
  purrr::map(
    find_groups,
    node_conn = input,
    .progress = TRUE
  ) |>
  unlist() |>
  unique()

all_cons |>
  stringr::str_detect("^t|-t") |>
  sum()
