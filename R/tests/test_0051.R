# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ggforce)

# Set parameters ---------------------------------------------------------------

# General params
iteration_id <- "test_0051"
initial_seed <- 23751
bg_colour <- "#e9e9e9"
colour_vec <- c(
  "#DB5375", "#E86C5F", "#F58549", "#F2A65A", "#EEC170", "#DDC993", "#CCD0B5",
  "#BBD7D7")

# Lines params
num_diag_lines <- 50
diags_x1 <- 8
diags_y1 <- -8

# diags_xend1
# diags_xend2
# diags_yend1
# diags_yend2

# Create data: blobs -----------------------------------------------------------

set.seed(initial_seed)
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

set.seed(initial_seed)
data <- tibble::tibble(
  x = sample(integer_vec, num_points, replace = TRUE, prob = x_weights),
  y = rep(
    sample(c(1,10), num_points / 2, replace = TRUE), each = 2),
  group = rep(seq(1, num_groups, by = 1), each = 4),
  hex_fill = sample(colour_vec, num_points, replace = TRUE),
  hex_outline = sample(outline_vec, num_points, replace = TRUE))

# Create data: horizontal diagonals --------------------------------------------

set.seed(initial_seed)
end_points <- tibble::tibble(
  diags_x1 = diags_x1,
  diags_x2 = sample(c(seq(-15, -8, by = 1), seq(8, 15, by = 1)), 1),
  diags_y1 = diags_y1,
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

# Build plot -------------------------------------------------------------------

p <- ggplot() +
  geom_diagonal_wide(
    data = data,
    aes(x = x, y = y, group = group, fill = hex_fill),
    strength = 0.5, size = 0, alpha = 1, radius = unit(4, 'mm')) +
  scale_alpha_identity() +
  geom_diagonal0(
    data = diags,
    aes(x, y, xend = xend, yend = yend),
    strength = end_points$strength, size = end_points$size,
    colour = bg_colour) +
  scale_fill_identity() +
  scale_colour_identity() +
  coord_fixed(xlim = c(1,10), ylim = c(1,10), expand = TRUE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = bg_colour, colour = bg_colour))

# Export to file ---------------------------------------------------------------

ggsave(
  here::here(glue::glue("img/tests/{`iteration_id`}.png")),
  last_plot(), width = 10, height = 10, units = "cm", dpi = 300,
  device = ragg::agg_png)
