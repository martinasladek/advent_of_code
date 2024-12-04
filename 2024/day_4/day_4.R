

# Task 1 ------------------------------------------------------------------

puzzle_input <- readr::read_csv(file = "2024/day_4/day_4.txt", col_names = FALSE) |> 
  dplyr::mutate(
    X1 = stringr::str_split(X1, pattern = "", simplify = TRUE)
  ) |> tidyr::unnest(cols = X1)

sum_from_list <- function(list){
  list |> unlist() |> sum()
}

search_xmas <- function(x){

   x_string <- x |> paste0(collapse = "") 
   xmas_loc <- x_string |> stringr::str_locate_all(pattern = "XMAS") 
   samx_loc <- x_string |> stringr::str_locate_all(pattern = "SAMX") 
   
   nrow(xmas_loc[[1]]) + nrow(samx_loc[[1]])
}


puzzle_mat <- puzzle_input$X1

n_rows <- nrow(puzzle_mat)
n_cols <- ncol(puzzle_mat)


# search rows

dir_1 <- purrr::map(
  .x = 1:n_rows, 
  .f = ~search_xmas(x = puzzle_mat[.x, ])
  ) |> sum_from_list()

# search cols
dir_2 <- purrr::map(
  .x = 1:n_cols, 
  .f = ~search_xmas(x = puzzle_mat[ ,.x])
) |> sum_from_list()


# diag top left bottom right

dir_3 <- purrr::map(
  .x = 1:n_cols, 
  .f = ~search_xmas(x = puzzle_mat[,.x:n_cols] |> diag())
) |> sum_from_list()

dir_4 <- purrr::map(
  .x = 2:n_rows, 
  .f = ~search_xmas(x = puzzle_mat[.x:n_rows, ] |> diag())
) |> sum_from_list()


# diag top right bottom left
# puzzle_mat[ ,10:1] # flips the columns so diag() grabs the right direction

dir_5 <- purrr::map(
  .x = 1:n_cols, 
  .f = ~search_xmas(x = puzzle_mat[ ,.x:1] |> diag())
) |> sum_from_list()


dir_6 <- purrr::map(
  .x = 2:n_rows, 
  .f = ~search_xmas(x = puzzle_mat[.x:n_rows, n_cols:1] |> diag())
) |> sum_from_list()


answer_1 <- dir_1 + dir_2 + dir_3 + dir_4 + dir_5 + dir_6


# Task 2 ------------------------------------------------------------------

collapse_detect <- function(x, collapse = "", pattern = "MAS|SAM"){
  x |> paste0(collapse = collapse) |> 
    stringr::str_detect(pattern = pattern) 
}

search_mas_square <- function(matrix = puzzle_mat, start_row, start_col){
  
  lil_square <- matrix[start_row:(start_row+2), start_col:(start_col+2)]
  
  diags <- purrr::map(
    .x = list(c(lil_square |> diag()), c(lil_square[,3:1] |> diag())), 
    .f = collapse_detect
  ) |> sum_from_list()
  
  diags == 2
  
}

start_row = 1
start_col = 1

max_start_row = n_rows - 2
max_start_col = n_cols - 2

location_grid <- tidyr::expand_grid(
  start_row = start_row:max_start_row, 
  start_col = start_col:max_start_col
)

answer_2 <- purrr::pmap(
  .l = location_grid,
  .f = search_mas_square
) |>  sum_from_list()
