library(sf)
library(rnaturalearth)
library(ggplot2)

europe <- ne_countries(scale = "medium",
                       continent = "Europe",
                       returnclass = "sf")

ggplot(data = europe) +
  geom_sf(fill = "grey90", color = "white", size = 0.3) +
  coord_sf(
    xlim = c(-25, 45),
    ylim = c(34, 72),
    expand = FALSE
  ) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "aliceblue"),
    plot.background = element_rect(fill = "aliceblue", color = NA),
    panel.border = element_blank()
  ) +
  labs(title = "Mapa da Europa (limpo e sem eixos)")
