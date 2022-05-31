# Source custom functions ------------------------------------------------------

source(here::here("R/fx_drop_blobs.R"))
source(here::here("R/fx_apply_mesh2.R"))
source(here::here("R/fx_make_noise.R"))

# Set parameters ---------------------------------------------------------------

# General params
iteration_id <- "unwound_0031"
initial_seed <- 55131
bg_colour <- "#DADBE7"

# For blobs
colour_vec <- c(
  "#320A28", "#511730", "#8E443D", "#CB9173", "#E0D68A", "#26547C", "#242038")

# For mesh
num_diag_lines <- 60

# For noise
starter_colours <- c("#202232", "#F3F3F7")
freq <- 1000

# Create data ------------------------------------------------------------------

# Drop blobs
blobs <- drop_blobs(
  seed_num = initial_seed, bg = bg_colour, palette = colour_vec)

set.seed(initial_seed)
rel_groups_sets <- blobs |>
  dplyr::distinct(group) |>
  dplyr::mutate(
    set_num = sample(c(1, 2), dplyr::n(), replace = TRUE))
blobs_mod <- dplyr::left_join(blobs, rel_groups_sets, by = "group")

blobs_set1 <- blobs_mod |>
  dplyr::filter(set_num == 1)

blobs_set2 <- blobs_mod |>
  dplyr::filter(set_num == 2)

# Apply mesh
mesh <- apply_mesh2(
  seed_num = initial_seed, num_diag_lines = num_diag_lines)
end_points <- mesh$end_points
diags <- mesh$diags

# Create noise data
noise <- make_noise(
  seed_num = initial_seed, colours = starter_colours, frequency = freq)
noise_data <- noise$noise
noise_gradient <- noise$noise_gradient

# Build plot -------------------------------------------------------------------

p <- ggplot2::ggplot() +
  ggforce::geom_diagonal_wide(
    data = blobs_set1,
    ggplot2::aes(x = x, y = y, group = group, fill = hex_fill),
    strength = 0.5, size = 0, alpha = 0.8, radius = ggplot2::unit(4, 'mm')) +
  ggforce::geom_diagonal_wide(
    data = blobs_set2,
    ggplot2::aes(x = x, y = y, group = group, fill = hex_fill),
    strength = 0.5, size = 0, alpha = 0.8) +
  ggplot2::scale_alpha_identity() +
  ggplot2::scale_fill_identity() +
  ggnewscale::new_scale_fill() +
  # Noise layer
  ggplot2::geom_raster(
    data = noise_data,
    ggplot2::aes(x, y, fill = noise),
    alpha = 0.2) +
  ggplot2::scale_fill_gradientn(colours = noise_gradient) +
  ggforce::geom_diagonal(
    data = diags,
    ggplot2::aes(x, y, xend = xend, yend = yend),
    strength = end_points$strength, size = end_points$size,
    colour = bg_colour, n = 500) +
  ggplot2::scale_colour_identity() +
  ggplot2::coord_fixed(xlim = c(-1,12), ylim = c(-1,12), expand = FALSE) +
  ggplot2::theme_void() +
  ggplot2::theme(
    legend.position = "none",
    plot.background = ggplot2::element_rect(
      fill = bg_colour, colour = bg_colour),
    plot.margin = ggplot2::margin(10,10,10,10, unit = "pt"))

# Export to file ---------------------------------------------------------------

ggplot2::ggsave(
  here::here(glue::glue("img/{`iteration_id`}.png")),
  ggplot2::last_plot(), width = 10, height = 10, units = "cm", dpi = 600,
  device = ragg::agg_png)

beepr::beep(2)