library(dplyr)
library(glue)


pudding_shades_data = readr::read_csv(url('https://raw.githubusercontent.com/the-pudding/data/master/makeup-shades/shades.csv'))


shade_palette = pudding_shades_data %>%
  mutate(shades = paste0("#", hex)) %>%
  select(hex, shades) %>%
  tibble::deframe()


shade_values = pudding_shades_data %>%
  filter(is.na(H) == FALSE) %>%
  mutate(S = S * 100,
         V = V * 100) %>%
  rename(Hue = H,
         Saturation = S,
         Brightness = V,
         Lightness = L) %>%
  mutate(label = glue("Brand: {brand}
                 Product: {product}"))


readr::write_rds(shade_values, 
                 here::here("Data", "shade_values.rds"))

readr::write_rds(shade_palette,
                 here::here("Data", "shade_palette.rds"))






