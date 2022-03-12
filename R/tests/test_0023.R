# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ggforce)

# Set parameters ---------------------------------------------------------------

iteration_id <- "test_0023"
initial_seed <- 753423
num_groups <- 20
colour_vec <- c(
  "#4F517D", "#1A3A3A", "#EE4266", "#FFD23F", "#61A0AF")

# Create data ------------------------------------------------------------------

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
  hex_outline = sample(outline_vec, num_points, replace = TRUE),
  group_alpha = rep(
    sample(seq(0.3, 0.8, by = 0.05), num_groups, replace = TRUE), each = 4))

# Build plot -------------------------------------------------------------------

p <- ggplot() +
  geom_diagonal_wide(
    data = data,
    aes(x = x, y = y, group = group, fill = hex_fill, alpha = group_alpha),
    strength = 2, size = 0, radius = unit(4, 'mm')) +
  scale_fill_identity() +
  scale_colour_identity() +
  scale_alpha_identity() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#DADBE7", colour = "#DADBE7"))

# Export to file ---------------------------------------------------------------

ggsave(
  here::here(glue::glue("img/tests/{`iteration_id`}.png")),
  last_plot(), width = 10, height = 10, units = "cm", dpi = 300)
