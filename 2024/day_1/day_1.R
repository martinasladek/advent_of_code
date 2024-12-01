
# Puzzle A ----------------------------------------------------------------

puzzle_input <- readr::read_csv("2024/day_1/day_1.txt", col_names = "lists") |> 
  tidyr::separate(col = lists, into = c("list_1", "list_2"), sep = "   ") |> 
  dplyr::mutate(
    dplyr::across(
      .fns = \(x) {x |> as.numeric() |> sort()} 
    ), 
    distance = abs(list_1 - list_2)
  ) 
  
answer_1 <- sum(puzzle_input$distance)


# Puzzle B ----------------------------------------------------------------

puzzle_input <- puzzle_input |> 
  dplyr::rowwise() |> 
  dplyr::mutate(
    l_count = sum(list_1 == puzzle_input$list_2), 
  ) 

answer_2 <- sum(puzzle_input$list_1 * puzzle_input$l_count)

