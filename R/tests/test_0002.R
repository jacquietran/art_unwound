# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ggforce)

# Set parameters ---------------------------------------------------------------

iteration_id <- "test_0002"
initial_seed <- 127542
num_groups <- 5

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

set.seed(initial_seed)
data <- tibble::tibble(
  x = sample(integer_vec, num_points, replace = TRUE, prob = x_weights),
  y = sample(integer_vec, num_points, replace = TRUE, prob = y_weights),
  group = rep(seq(1, num_groups, by = 1), each = 4))

# Build plot -------------------------------------------------------------------

p <- ggplot() +
  geom_diagonal_wide(
    data = data,
    aes(x = x, y = y, group = group),
    strength = 1.5, colour = "#000000", size = 2, fill = "#FFFFFF") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"))

# Export to file ---------------------------------------------------------------

ggsave(
  here::here(glue::glue("img/tests/{`iteration_id`}.png")),
  last_plot(), width = 10, height = 10, units = "cm", dpi = 300)
