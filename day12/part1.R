input <- readr::read_lines(
  "day12/input.txt"
)|>
  stringr::str_split("", simplify = TRUE)

input_poly <- input |>
  terra::rast() |>
  terra::as.polygons() |>
  sf::st_as_sf()

fixed_polys <- purrr::map_df(
  seq_len(nrow(input_poly)),
  \(poly_row){
    if (sf::st_geometry_type(input_poly$geometry[[poly_row]]) == "POLYGON") {
      input_poly[poly_row, ]
    } else {
      sf::st_cast(input_poly[poly_row, ], "POLYGON") |>
        dplyr::mutate(
          `lyr.1` = paste0(`lyr.1`, "-", dplyr::row_number())
        )
    }
  }
)

# Part 1
scored <- fixed_polys |>
  dplyr::mutate(
    score = sf::st_area(geometry) * sf::st_perimeter(geometry)
  )

sum(scored$score)

# Part 2
scored2 <- fixed_polys |> 
  dplyr::group_by(lyr.1) |> 
  dplyr::mutate(
    perim = sf::st_coordinates(geometry) |> 
      as.data.frame() |> 
      dplyr::distinct() |> 
      nrow(),
    score = perim * sf::st_area(geometry)
  )

sum(scored2$score)
