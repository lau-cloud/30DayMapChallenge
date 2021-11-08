# Libraries
library(tidyverse)
library(viridis)
library(hrbrthemes)
library(geojsonio)
library(sf)
library(broom)
library(cowplot)



# Load dataset from github
data <- read.csv("TRAMS.csv", sep=",", header=T)

#load boundaries
spdf <- geojson_read("shapefiles_barcelona_distrito.geojson",  what = "sp")

spdf_fortified <- tidy(spdf)

#filtre trams

trams_verda <- c("VR", "VM")

tram_AZL <- data %>% 
  filter(TIPUS_TRAM == "AZL")

tram_VR <- data %>% 
  filter(TIPUS_TRAM %in% trams_verda)

tram_DUM <- data %>% 
  filter(TIPUS_TRAM == "DUM")


# plot
mapa_AZL <- ggplot() +
  geom_hex(data = tram_AZL, aes(x = LONGITUD_I, y = LATITUD_I),  
           color = "white", alpha = 0.8, bins = 35) +
  geom_polygon(data = spdf_fortified, 
               aes( x = long, y = lat, group = group),
               size = 0.25,  
               fill="transparent", 
               color="black") +
  labs(
    title = "Blue Zone",
    caption = ""
  ) +
  theme_void() +
  scale_fill_gradient(
    low = "#D1F0E5",
    high = "#306D75",
   breaks = c(1,20,40),
    name="Blue spaces", 
    guide = guide_legend( keyheight = unit(2.5, units = "mm"), keywidth=unit(10, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) 
   )  +
  theme(
    legend.position = "bottom",
    legend.title=element_text(color="black", size=8),
    text = element_text(color = "black"),
    plot.subtitle = element_text(hjust = 0.5, size = 8, color = "black"),
    plot.title = element_text(hjust = 0.5, size = 10, family = "Gadugi"),
    plot.caption = element_text(hjust = 1, size = 7, color = "black")
    ) 
 

mapa_AZL


mapa_VR <- ggplot() +
  geom_hex(data = tram_VR, aes(x = LONGITUD_I, y = LATITUD_I),  
           color = "white", alpha = 0.8, bins = 35) +
  geom_polygon(data = spdf_fortified, 
               aes( x = long, y = lat, group = group),
               size = 0.25,  
               fill="transparent", 
               color="black") +
  labs(
    title = "Green Zone",
    caption = ""
  ) +
  theme_void() +
  scale_fill_gradient(
    low = "#E1FCC1",
    high = "#5DAA01",
    breaks = c(1,60, 120),
    name="Green spaces", 
    guide = guide_legend( keyheight = unit(2.5, units = "mm"), keywidth=unit(10, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) 
  )  +
  theme(
    legend.position = "bottom",
    legend.title=element_text(color="black", size=8),
    text = element_text(color = "black"),
    plot.subtitle = element_text(hjust = 0.5, size = 8, color = "black"),
    plot.title = element_text(hjust = 0.5, size = 11, family = "Gadugi"),
    plot.caption = element_text(hjust = 0.5, size = 7, color = "black")
  ) 


mapa_VR

mapa_DUM <- ggplot() +
  geom_hex(data = tram_DUM, aes(x = LONGITUD_I, y = LATITUD_I),  
           color = "white", alpha = 0.8, bins = 35) +
  geom_polygon(data = spdf_fortified, 
               aes( x = long, y = lat, group = group),
               size = 0.25,  
               fill="transparent", 
               color="black") +
  labs(
    title = "Urban Distribution of Goods Zone",
    caption = "Data: OpenData BCN | Visualization: Laura Navarro"
  ) +
  theme_void() +
  scale_fill_gradient(
    low = "#FFD2D2",
    high = "#CD0000",
    breaks = c(1,12,25),
    name="DUM spaces", 
    guide = guide_legend( keyheight = unit(2.5, units = "mm"), keywidth=unit(10, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) 
  )  +
  theme(
    legend.position = "bottom",
    legend.title=element_text(color="black", size=8),
      text = element_text(color = "black"),
    plot.subtitle = element_text(hjust = 0.5, size = 8, color = "black"),
    plot.title = element_text(hjust = 0.5, size = 11, family = "Gadugi"),
    plot.caption = element_text(hjust = 1, size = 7, color = "black")
  ) 


mapa_DUM


#Title and subtitle
ggdraw() +
  draw_text("Parking Areas in Barcelona",
            size = 16, family = "Gadugi", fontface = "bold")  -> header_plot


#grid plots
grid_plots <- plot_grid(
  mapa_AZL,
  mapa_VR,
  mapa_DUM,
  nrow = 1,
  ncol = 3
) 

#final plot
final_plot <- plot_grid(
  
  header_plot,
  
  grid_plots,
  
  ## plot settings
  nrow = 2,
  ncol = 1,
  rel_heights = c(1,5)
) 

final_plot



ggsave("parkings.png", 
       final_plot,
       height = 4, width = 7.3, 
       units = "in", dpi = 300)



