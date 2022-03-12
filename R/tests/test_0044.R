# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ggforce)

# Set parameters ---------------------------------------------------------------

# General params
iteration_id <- "test_0044"
initial_seed <- 870144
bg_colour <- "#fbdfd1"

# Blobs params
num_groups <- 60
colour_vec <- c(
  "#f7b296", "#f87d7f", "#ce9e5c", "#819a7a", "#425225")

# Lines params
num_diag_lines <- 120
diags_x1 <- -1
diags_y1 <- 13

# Create data: blobs -----------------------------------------------------------

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

diags <- tibble::tibble(
  x = seq(diags_x1, diags_x1 + 10, length.out = num_diag_lines),
  y = seq(diags_y1, -5, length.out = num_diag_lines),
  xend = seq(diags_x1 + 2, 15, length.out = num_diag_lines),
  yend = seq(15, -3, length.out = num_diag_lines))

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
    strength = 0.75, size = 0.6, colour = bg_colour) +
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
