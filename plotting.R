library(ggmap)

sample = read.csv("sample.csv")
sample = sample[-c(22, 26, 42, 54),]

world_coords = c(-180,-80, 180, 80)
world_map = get_stamenmap(world_coords, zoom = 1, maptype = "toner-lite")

ggmap(world_map) +
  geom_point(aes(x = center_long - 180, y = center_lat, color = big$ice), data = sample, size = 3) +
  labs(title = "Actual", color = "Ice")

ggmap(world_map) +
  geom_point(aes(x = center_long - 180, y = center_lat, color = rf_cv$predicted$`228`), data = sample, size = 3) +
  labs(title = "Predicted", color = "Ice")