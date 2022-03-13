make_noise <- function(seed_num, colours, frequency){
  
  # Requires {grDevices}, {ambient}, and {dplyr}
  
  noise_gradient <- (grDevices::colorRampPalette(colours))(50)
  
  set.seed(seed_num)
  noise <- ambient::long_grid(
    x = seq(-1, 12, length.out = 2000),
    y = seq(-1, 12, length.out = 2000)) |>
    dplyr::mutate(
      noise = ambient::fracture(
        ambient::gen_value, ambient::fbm, octaves = 8, frequency = frequency,
        x = x, y = y))
  
  return(
    list(
      "noise_gradient" = noise_gradient,
      "noise" = noise))
  
}