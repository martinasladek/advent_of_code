
options(scipen = 999)

# Part 1 ------------------------------------------------------------------

puzzle_input <- readLines("2024/day_7/day_7_example.txt")

calib_tib <- tibble::tibble(
  result = puzzle_input |> stringr::str_extract_all(pattern = "^.+?(?=:)") |> unlist() |> as.numeric(),
  numbers = puzzle_input |> stringr::str_extract_all(pattern = "(?<=: ).+")
) 

create_math_string <- function(numbers_i, replacements){
  
  numbers_i_split <- numbers_i |> stringr::str_split("") |> unlist()
  space_loc <- which(numbers_i_split == " ")
  numbers_i_split[space_loc] <- replacements
  numbers_i_split |>
    unlist() |>
    paste0(collapse = "")
  
}


ignore_math_rules <- function(math_string_i){
  
  steps <- stringr::str_split(math_string_i, "(?<=\\d)(?=[+*_])")[[1]] # adapted to deal with part 2
  result <- as.numeric(steps[1])
    
    for (i in 2:length(steps)) {
      symbol <- substr(steps[i], 1, 1)
      number <- as.numeric(substr(steps[i], 2, nchar(steps[i])))
      if (symbol == "+") result <- result + number
      else if (symbol == "_") result <- paste0(result, number) |> as.numeric() # part 2 only 
      else if (symbol == "*") result <- result * number
    }
    
    return(result)
  
}

eval_numbers <- function(numbers_i, symbols = c("+", "*")){
  
  n_spaces <- stringr::str_count(numbers_i, " ") |> sum()
  all_opts <- expand.grid(rep(list(symbols), n_spaces), stringsAsFactors = FALSE)
  
  math_strings <- purrr::map(
    1:nrow(all_opts), ~create_math_string(numbers_i, all_opts[.x, ])) |> 
    unlist() 
  
  purrr::map(math_strings, ignore_math_rules) |> unlist()
  
}


calib_tib <- calib_tib |> 
  dplyr::rowwise() |> 
  dplyr::mutate(
    eval = list(eval_numbers(numbers))
  ) 

answer_1 <- calib_tib |> 
  dplyr::filter(result %in% eval) |>
  dplyr::pull(result) |>
  sum()


# Part 2 ------------------------------------------------------------------


calib_tib <- calib_tib |> 
  dplyr::rowwise() |> 
  dplyr::mutate(
    eval = list(eval_numbers(numbers, symbols = c("+", "*", "_")))
  ) 


answer_2 <- calib_tib |> 
  dplyr::filter(result %in% eval) |>
  dplyr::pull(result) |>
  sum()

