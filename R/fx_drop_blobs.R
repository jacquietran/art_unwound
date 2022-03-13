drop_blobs <- function(seed_num, bg, palette){
  
  # Requires {scales} and {tibble}
  
  set.seed(seed_num)
  num_groups <- sample(seq(30, 120, by = 5), 1)
  
  num_points <- num_groups * 4
  
  set.seed(initial_seed)
  seed_vec <- sample(seq(1, 100000, by = 1), 2, replace = FALSE)
  
  set.seed(seed_vec[1])
  x_weights <- scales::rescale(
    sample(seq(1, 500000, by = 1), 10, replace = TRUE))
  
  set.seed(seed_vec[2])
  y_weights <- scales::rescale(
    sample(seq(1, 500000, by = 1), 10, replace = TRUE))
  
  integer_vec <- seq(1, 10, by = 1)
  
  outline_vec <- c(colour_vec, NA)
  
  set.seed(seed_num)
  data <- tibble::tibble(
    x = sample(integer_vec, num_points, replace = TRUE, prob = x_weights),
    y = rep(
      sample(c(1,10), num_points / 2, replace = TRUE), each = 2),
    group = rep(seq(1, num_groups, by = 1), each = 4),
    hex_fill = sample(colour_vec, num_points, replace = TRUE),
    hex_outline = sample(outline_vec, num_points, replace = TRUE))
  
  return(data)
  
}