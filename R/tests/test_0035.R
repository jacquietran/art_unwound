# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ggforce)

# Set parameters ---------------------------------------------------------------

iteration_id <- "test_0035"
initial_seed <- 801335
num_groups <- 12
num_tiles <- 5 # number produces a square, e.g., 5x5
colour_vec <- c(
  "#B7094C", "#892B64", "#5C4D7D", "#2E6F95", "#0091AD")

# Define custom function -------------------------------------------------------

create_blobs <- function(
  initial_seed, num_groups, num_tiles, colour_vec){
  
  num_points <- num_groups * 4
  
  set.seed(initial_seed)
  seed_vec1 <- sample(seq(1, 100000, by = 1), 2, replace = FALSE)
  
  set.seed(seed_vec1[1])
  seed_vec2 <- sample(seq(1, 100000, by = 1), num_tiles^2, replace = FALSE)
  
  set.seed(seed_vec1[1])
  x_weights <- scales::rescale(
    sample(seq(1, 500000, by = 1), 10, replace = TRUE))
  
  set.seed(seed_vec1[2])
  y_weights <- scales::rescale(
    sample(seq(1, 500000, by = 1), 10, replace = TRUE))
  
  integer_vec <- seq(1, 10, by = 1)
  
  purrr::map_df(seed_vec2, function(i) {
    
    set.seed(i)
    data <- tibble::tibble(
      tile_id = i,
      x = sample(integer_vec, num_points, replace = TRUE, prob = x_weights),
      y = rep(
        sample(c(1,10), num_points / 2, replace = TRUE), each = 2),
      group = rep(seq(1, num_groups, by = 1), each = 4)) %>%
      group_by(group) %>%
      dplyr::mutate(tile_group_id = glue::glue("{tile_id}_{group}")) %>%
      ungroup()
  
  }) -> data
  
  data_with_hex <- data %>%
    distinct(tile_group_id) %>%
    mutate(hex_fill = sample(colour_vec, n(), replace = TRUE))
  
  data_tidy <- left_join(data, data_with_hex)
  
}

# Create data ------------------------------------------------------------------

data <- create_blobs(
  initial_seed = initial_seed, num_groups = num_groups, num_tiles = num_tiles,
  colour_vec = colour_vec)

# Build plot -------------------------------------------------------------------

p <- ggplot() +
  facet_wrap(~tile_id, nrow = num_tiles) +
  geom_diagonal_wide(
    data = data,
    aes(x = x, y = y, group = tile_group_id, fill = hex_fill),
    strength = 1, size = 0, alpha = 1, radius = unit(2, 'mm')) +
  scale_fill_identity() +
  scale_colour_identity() +
  theme_void() +
  theme(
    strip.text.x = element_blank(),
    plot.background = element_rect(fill = "#f9dcc4", colour = "#f9dcc4"),
    panel.spacing = unit(30, "pt"),
    plot.margin = margin(30, 30, 30, 30, unit = "pt"))

# Export to file ---------------------------------------------------------------

ggsave(
  here::here(glue::glue("img/tests/{`iteration_id`}.png")),
  last_plot(), width = num_tiles * 10, height = num_tiles * 10,
  units = "cm", dpi = 600)
