# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ggforce)

# Set parameters ---------------------------------------------------------------

# General params
iteration_id <- "unwound_0000"
initial_seed <- 870126

# Blobs params
num_groups <- 90
colour_vec <- c(
  "#4F517D", "#1A3A3A", "#EE4266", "#FFD23F", "#61A0AF")

# Lines params
num_diag_lines <- 50

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
  x = rep(1, times = num_diag_lines),
  y = seq(1, 10, length.out = num_diag_lines),
  xend = seq(2, 9, length.out = num_diag_lines),
  yend = seq(2, 12, length.out = num_diag_lines))

# Build plot -------------------------------------------------------------------

p <- ggplot() +
  geom_diagonal_wide(
    data = data,
    aes(x = x, y = y, group = group, fill = hex_fill),
    strength = 0.5, size = 0, alpha = 1, radius = unit(2, 'mm')) +
  scale_alpha_identity() +
  geom_diagonal0(
    data = diags,
    aes(x, y, xend = xend, yend = yend),
    size = 0.5, colour = "#DADBE7") +
  scale_fill_identity() +
  scale_colour_identity() +
  coord_fixed(xlim = c(1,10), ylim = c(1,10), expand = TRUE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#DADBE7", colour = "#DADBE7"),
    plot.margin = margin(40,40,40,40, unit = "pt"))

# Export to file ---------------------------------------------------------------

ggsave(
  here::here(glue::glue("img/{`iteration_id`}.png")),
  last_plot(), width = 3500, height = 3500, units = "px", dpi = 600)
