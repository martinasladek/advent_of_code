

# Part 1 ------------------------------------------------------------------

puzzle_input <- readLines("2024/day_5/day_5.txt")

n_rules <- sum(stringr::str_detect(puzzle_input, "\\|"))

rules <- tibble::tibble(rules = puzzle_input[1:n_rules]) |> 
  tidyr::separate(rules, into = c("x", "y"), sep = "\\|", convert = TRUE, remove = FALSE)

updates <- puzzle_input[(n_rules + 2):length(puzzle_input)] |> 
  tibble::as_tibble(rownames = "id") |> 
  dplyr::rowwise() |> 
  dplyr::mutate(
    update = stringr::str_split(value, pattern = ",") |> 
      unlist() |> as.numeric() |> list()
    ) 


find_broken_rules <- function(update_i, rules){
  
  comb_mat <- combn(update_i, m = 2) |> t() 
  
  tibble::tibble(
    x = comb_mat[,1], 
    y = comb_mat[,2]
  ) |> 
    dplyr::left_join(rules, by = c("x", "y")) |> 
    dplyr::filter(is.na(rules))
  
}

check_rules <- function(update_i, rules){
  rules_broken = find_broken_rules(update_i, rules)
  nrow(rules_broken) != 0
}

mid_value <- function(x){
  x[(length(x)/2) |> ceiling()]
}

puzzle_eval <- updates |> # note - updates is currently rowwise
  dplyr::mutate(
    rules_broken = check_rules(update, rules)
  )

pull_and_sum <- function(tibble, col){
  tibble |> dplyr::pull(col) |> sum()
}

answer_1 <- puzzle_eval |> 
  dplyr::filter(!rules_broken) |> 
  dplyr::mutate(mid_value = mid_value(update)) |> 
  pull_and_sum("mid_value")


# Part 2 ------------------------------------------------------------------


swap_pages <- function(update_i, rules){
  
  n_broken_rules = 1 # initial starting value 
  
  while(n_broken_rules != 0){
    
    rules_broken <- find_broken_rules(update_i, rules)
    n_broken_rules <- nrow(rules_broken)
    
    if(n_broken_rules == 0) break 
    
    else{
      
      x_swap <- rules_broken[1, ]$x
      y_swap <- rules_broken[1, ]$y
      
      x_swap_index <- which(update_i == x_swap)
      y_swap_index <- which(update_i == y_swap)
      
      update_i[x_swap_index] <- y_swap
      update_i[y_swap_index] <- x_swap
      
    }
  }
  return(update_i)
}


answer_2 <- puzzle_eval |> # note: puzzle eval is rowwise
  dplyr::filter(rules_broken) |> 
  dplyr::mutate(
    update_fixed = list(swap_pages(update, rules = rules)), 
    mid_value = mid_value(update_fixed)
  ) |> 
  pull_and_sum("mid_value")


