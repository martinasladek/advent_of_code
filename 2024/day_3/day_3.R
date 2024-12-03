 

# Part 1 ------------------------------------------------------------------

puzzle_input <- readLines("2024/day_3/day_3.txt")

mul_regex <- "mul\\([0-9.]+,[0-9.]+\\)"


answer_1 <- tibble::tibble(
  valid_strings = unlist(stringr::str_extract_all(puzzle_input, pattern = mul_regex))
) |> 
  tidyr::separate(
    valid_strings, into = c("num_1", "num_2"), sep = ","
  ) |> 
  dplyr::mutate(
    dplyr::across(
      .cols = c(num_1, num_2), .fns = readr::parse_number
    ), 
    mul = num_1*num_2
  ) |> 
  dplyr::pull(mul) |> sum()


# Part 2 ------------------------------------------------------------------

mul_do_regex <- "mul\\([0-9.]+,[0-9.]+\\)|do\\(\\)|don\\'t\\(\\)"

answer_2 <- tibble::tibble(
  valid_strings = unlist(stringr::str_extract_all(puzzle_input, pattern = mul_do_regex))
) |> 
  dplyr::mutate(
    do = dplyr::case_when(
      dplyr::row_number() == 1 ~ 1,
      valid_strings == "do()" ~ 1, 
      valid_strings == "don't()" ~ 0,
    ) 
  )|> 
  tidyr::fill(do, .direction = "down") |> 
  dplyr::filter(do == 1, valid_strings != "do()") |> 
  tidyr::separate(
    valid_strings, into = c("num_1", "num_2"), sep = ","
  ) |> 
  dplyr::mutate(
    dplyr::across(
      .cols = c(num_1, num_2), .fns = readr::parse_number
    ), 
    mul = num_1*num_2
  ) |> 
  dplyr::pull(mul) |> sum()
