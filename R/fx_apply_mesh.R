apply_mesh <- function(seed_num, num_diag_lines){
  
  # Requires {tibble}
  
  set.seed(seed_num)
  end_points <- tibble::tibble(
    diags_x1 = sample(seq(-13, 13, by = 1), 1),
    diags_x2 = sample(c(seq(-15, -8, by = 1), seq(8, 15, by = 1)), 1),
    diags_y1 = sample(seq(-15, 15, by = 1), 1),
    diags_y2 = sample(c(seq(-5, 2, by = 1), seq(8, 15, by = 1)), 1),
    diags_xend1 = diags_x1 + sample(seq(-5, 5, by = 1), 1),
    diags_xend2 = sample(c(seq(-15, -8, by = 1), seq(8, 15, by = 1)), 1),
    diags_yend1 = -diags_y1 + sample(seq(-3, 3, by = 1), 1),
    diags_yend2 = sample(c(seq(-15, -8, by = 1), seq(8, 15, by = 1)), 1),
    strength = sample(seq(0.5, 2, by = 0.1), 1),
    size = sample(seq(0.2, 1.5, by = 0.1), 1))
  
  diags <- tibble::tibble(
    x = seq(end_points$diags_x1, end_points$diags_x2,
            length.out = num_diag_lines),
    y = seq(end_points$diags_y1, end_points$diags_y2,
            length.out = num_diag_lines),
    xend = seq(end_points$diags_xend1, end_points$diags_xend2,
               length.out = num_diag_lines),
    yend = seq(end_points$diags_yend1, end_points$diags_yend2,
               length.out = num_diag_lines))
  
  return(list(
    "end_points" = end_points,
    "diags" = diags))
  
}