# Libraries that we need
library(tidyverse)
library(viridis)
library(hrbrthemes)
library(geojsonio)
library(sf)
library(broom)
library(cowplot)
library(hexbin)
library(showtext)


# Loading fonts
font_add_google("DM Serif Display", "abril")
font_add_google("Tajawal", "tawa")
showtext_auto()


# Loading dataset
data <- read.csv("https://raw.githubusercontent.com/lau-cloud/30DayMapChallenge/main/parking_areas/TRAMS.csv", sep=",", header=T)

# Loading Barcelona's district boundaries
districts <- st_read("https://raw.githubusercontent.com/lau-cloud/30DayMapChallenge/main/parking_areas/0301040100_Districtes_UNITATS_ADM.json",
                  stringsAsFactors = FALSE, 
                  as_tibble = TRUE)


# Filtering types
trams_verda <- c("VR", "VM")

tram_AZL <- data |>  
  filter(TIPUS_TRAM == "AZL")

tram_VR <- data |>  
  filter(TIPUS_TRAM %in% trams_verda)

tram_DUM <- data |>  
  filter(TIPUS_TRAM == "DUM")


# Plotting
mapa_AZL <- ggplot() +
  geom_hex(data = tram_AZL, aes(x = LONGITUD_I, y = LATITUD_I),  
           color = "white", alpha = 0.8, bins = 35) +
  geom_sf(
    data = districts, fill = "transparent",
    color = "black", linewidth = .25) +
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
    plot.title = element_text(hjust = 0.5, size = 16, family = "tawa"),
    plot.caption = element_text(hjust = 1, size = 7, color = "black")
  ) 


mapa_AZL


mapa_VR <- ggplot() +
  geom_hex(data = tram_VR, aes(x = LONGITUD_I, y = LATITUD_I),  
           color = "white", alpha = 0.8, bins = 35) +
  geom_sf(
    data = districts, fill = "transparent",
    color = "black", linewidth = .25) +
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
    plot.title = element_text(hjust = 0.5, size = 16, family = "tawa"),
    plot.caption = element_text(hjust = 0.5, size = 7, color = "black")
  ) 


mapa_VR

mapa_DUM <- ggplot() +
  geom_hex(data = tram_DUM, aes(x = LONGITUD_I, y = LATITUD_I),  
           color = "white", alpha = 0.8, bins = 35) +
  geom_sf(
    data = districts, fill = "transparent",
    color = "black", linewidth = .25) +
  labs(
    title = "Urban Distribution of Goods Zone"
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
    plot.title = element_text(hjust = 0.5, size = 16, family = "tawa"),
    plot.caption = element_text(hjust = 1, size = 7, color = "black")
  ) 


mapa_DUM


# Title
ggdraw() +
  draw_text("Parking Areas in Barcelona",
            size = 24, family = "abril", fontface = "bold")  -> header_plot

# Caption
ggdraw() +
  draw_text("Data: OpenData BCN | Visualization: Laura Navarro",
            size = 16, family = "tawa", color = "grey", hjust = 0.5)  -> caption_plot

# Grid plots
grid_plots <- plot_grid(
  mapa_AZL,
  mapa_VR,
  mapa_DUM,
  nrow = 1,
  ncol = 3
) 

# Final plot
final_plot <- plot_grid(
  
  header_plot,
  
  grid_plots,
  
  caption_plot,
  
  ## plot settings
  nrow = 3,
  ncol = 1,
  rel_heights = c(1,5,1)
) 

final_plot


# Saving plot
ggsave("parkings.png", 
       final_plot,
       height = 4, width = 7.3, 
       units = "in", dpi = 300)


