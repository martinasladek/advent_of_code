

# Task 1 ------------------------------------------------------------------


puzzle_input <- readLines("2024/day_2/day_2.txt") |> 
  purrr::map(
    .f = \(x) {
      stringr::str_split(x, pattern = " ")
    }
  ) |> 
  purrr::map(
    .f = \(x) {x |> unlist() |> as.numeric()}
  )


is_safe <- function(x){
  
  # create sorted versions of reports 
  sorted_report_asc <- sort(x)
  sorted_report_desc <- sort(x, decreasing = TRUE)
  
  # needs to be true: compare whether report is identical to either sorted version 
  rise_or_fall <- sum(x == sorted_report_asc) == length(x) | sum(x == sorted_report_desc) == length(x)
  
  # create a shifted vector c(x[-1], 0) to calculate differences: 
  report_diffs <- abs(x-c(x[-1], 0))
  report_diffs <- report_diffs[-length(report_diffs)] # ignore last value 
  
  # needs to be false: is any difference too large or is any difference 0
  identical_or_too_large <- any(report_diffs > 3 | report_diffs == 0)
  
  # return true/false for safe/unsafe
  ifelse(rise_or_fall == TRUE & identical_or_too_large == FALSE, TRUE, FALSE)
  
}

answer_1 <- puzzle_input |> purrr::map(
  .f = is_safe
) |> 
  unlist() |> 
  sum()


# Task 2 ------------------------------------------------------------------

remove_element <- function(x, index){
  x[-index]
}

gen_if_missing <- function(x){
  
  indices <- 1:length(x)
  
  report_options <- purrr::map(
    .x = indices, 
    .f = ~remove_element(x = x, index = .x)
  )
  
  report_options[[length(x)+1]] <- x
  report_options
}

# basically expand each element to contain all version of the levels with one
# element removed. Then apply the same function (at list depth 2). If either 
# is safe, the whole set at depth 2 gets evaluated as safe. Unlist and sum.  

answer_2 <-  puzzle_input |> 
  purrr::map(
    .f = gen_if_missing 
  ) |> 
  purrr::map_depth(
    .depth = 2,
    .f = is_safe
  ) |> 
  purrr::map(
    .f = \(x) {(x |> unlist() |> sum()) != 0}
  ) |> 
  unlist() |>
  sum()
