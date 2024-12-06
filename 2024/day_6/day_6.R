
# Part 1 ------------------------------------------------------------------

puzzle_input <- data.frame(
  x = readLines("2024/day_6/day_6.txt") 
) 
n_col <- nchar(puzzle_input$x[1])
n_row <- nrow(puzzle_input)

puzzle_input <- puzzle_input |>   
  tidyr::separate(x, into = paste0("c", 0:(n_col)), sep = "") |> 
  dplyr::select(-1)

unlist_sum <- function(x) x |> unlist() |> sum() 

current_row = purrr::map(1:(n_row), ~which(puzzle_input[,.x]=="^")) |> unlist_sum()
current_col = purrr::map(1:(n_col), ~which(puzzle_input[.x,]=="^")) |> unlist_sum()

current_dir = "up"

row_log = c(current_row)
col_log = c(current_col)
dir_log = c(current_dir)


while (current_row %in% 2:(n_row-1) & current_col %in% 2:(n_col-1)) {
  
  if (current_dir == "up") {
    if (puzzle_input[current_row-1, current_col] == "#") current_dir <- "right"
    else current_row <- current_row-1
  }
  
  else if (current_dir == "right") {
    if (puzzle_input[current_row, current_col+1] == "#") current_dir <- "down"
    else current_col <- current_col+1
  }
  
  else if (current_dir == "down") {
    if (puzzle_input[current_row+1, current_col] == "#") current_dir <- "left"
    else current_row <- current_row+1
  }
  
  else if (current_dir == "left") {
    if (puzzle_input[current_row, current_col-1] == "#") current_dir <- "up"
    else current_col <- current_col-1
  }
  
  
  row_log <- c(row_log, current_row)
  col_log <- c(col_log, current_col)
  dir_log <- c(dir_log, current_dir)
  
}


position_tib <- tibble::tibble(row_log, col_log, dir_log) |> 
  dplyr::filter(!duplicated(paste0(row_log, ",", col_log))) |> 
  dplyr::mutate(
    loc_string = paste0(row_log, ",", col_log)
  )

answer_1 <- position_tib |> nrow()



# Part 2 ------------------------------------------------------------------

# existing_locs <- which(puzzle_input == "#", arr.ind=TRUE) |> 
#   as.data.frame() |> 
#   dplyr::arrange(row) |> 
#   dplyr::mutate(loc_string = paste0(row, ",", col))
# 
# cand_cols <- existing_locs[which(existing_locs$col - 1 > 0), ]$col - 1
# cand_rows <- existing_locs$row
# 
# cand_locs <- expand.grid(row = cand_rows, col = cand_cols) |> 
#   dplyr::mutate(loc_string = paste0(row, ",", col))
# 
# cand_locs <- cand_locs |> 
#   dplyr::filter(loc_string != existing_locs$loc_string) |> 
#   dplyr::arrange(row)


## no definitely not the code below. All fun and games with example until we get too 700K 
## iterations. 

# cand_locs$col_obs_check <- NULL
# for(i in 1:nrow(cand_locs)){
# 
#   cand_locs$col_obs_check[i] <- 
#     paste0(cand_locs[i, ]$row, ",", cand_locs[i, ]$col-1) %in% existing_locs$loc_string |
#     paste0(cand_locs[i, ]$row, ",", cand_locs[i, ]$col+1) %in% existing_locs$loc_string
#   print(i)
# }
# 
# cand_locs |> 
#   dplyr::filter(col_obs_check) |> 
#   dplyr::filter(loc_string %in% position_tib$loc_string) |> 
#   dplyr::arrange(row)
