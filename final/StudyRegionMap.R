library("maps")
library(sf)
library(ggplot2)
library(rnaturalearth)

world = ne_countries(scale = "medium", returnclass = "sf")
states = st_as_sf(map("state", plot = FALSE, fill = TRUE))

selected.states = c("massachusetts", "new hampshire", "vermont", "new york", "rhode island",
                    "connecticut", "new jersey", "maine", "pennsylvania")

states$group = NA
states[states$ID %in% selected.states,]$group = "lightblue"

ggplot(data = world) +
  theme_classic() +
  geom_sf() +
  geom_sf(data = states, fill = states$group) + 
  coord_sf(xlim = c(-81, -66.5), ylim = c(38.5, 47.75), expand = FALSE) +
  theme(axis.text=element_text(size=24))
