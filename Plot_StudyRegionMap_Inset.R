library("maps")
library(sf)
library(ggplot2)
library(rnaturalearth)
library(grid)

world = ne_countries(scale = "medium", returnclass = "sf")
states = st_as_sf(map("state", plot = FALSE, fill = TRUE))

selected.states = c("massachusetts", "new hampshire", "vermont", "new york", "rhode island",
                    "connecticut", "new jersey", "maine", "pennsylvania")

states$group = NA
states[states$ID %in% selected.states,]$group = "lightblue"

study.base = ggplot(data = world) +
  theme_classic() +
  geom_sf() +
  geom_sf(data = states, fill = states$group) + 
  coord_sf(xlim = c(-81, -66.5), ylim = c(38.5, 47.75), expand = FALSE) +
  theme(axis.text=element_text(size=24))
print(study.base)

insetmap <- ggplot(data = world) +
  theme_bw() +
  geom_sf(linetype = "blank") +
  geom_sf(data = states, size=0.1) + 
  coord_sf(xlim = c(-125, -65), ylim = c(24, 50), expand = FALSE) +
  theme(panel.grid.major = element_line(colour = 'transparent'),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
print(insetmap)

# vp_inset width/height arguments set the size of the inset; x and y arguments set the position (from 0 to 1) of the left, top corner of the inset along each axis (i.e. not map coordinates as you have in your annotation custom). You can adjust these as you see fit.
vp_inset <- grid::viewport(width = 0.25, height = 0.35, x = 0.65, y = 0.5, just = c("left", "top"))
print(study.base)
print(insetmap, vp = vp_inset)
dev.off()
