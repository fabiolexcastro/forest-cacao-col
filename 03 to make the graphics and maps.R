

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(colourpicker, geodata, ggpubr, gfcanalysis, glue, tidyterra, rnaturalearthdata, rnaturalearthdata, rnaturalearth, hrbrthemes, ggthemes, ggspatial, showtext, extrafont, ggrepel, stringr, sf, readxl, tidyverse, fs, gtools, terra)

options(scipen = 999, warn = -1)
g <- gc(reset = T)
rm(list = ls())

# Load data ----------------------------------------------------------------

## Tabular data ----------------
pnts <- read_csv('./tbl/Sitios_Cacao_ClimaLoCa_forests-noforests.csv', show_col_types = FALSE)
freq <- read_csv('./tbl/forest-count_v1.csv', show_col_types = FALSE)

## Vector data ----------------
dpto <- st_read('D:/DATA/colombia/bsae/DPTO.shp')
wrld <- ne_countries(returnclass = 'sf', scale = 50)
amrc <- filter(wrld, region_un == 'Americas')

## Raster data ----------------
gfc.r <- terra::rast('./tif/forest/gfc2020/ForestCover_Colombia_2020.tif')
hns.r <- terra::rast('./tif/forest/hansen_2020.tif')
ide.r <- terra::rast('./tif/forest/ideam/bosque-nobosque_2017.tif')


# Location points ---------------------------------------------------------

## Ubication
g.map.pnt <- ggplot() + 
  geom_sf(data = dpto, fill = NA, col = 'grey30') + 
  geom_point(data = pnts, aes(x = lon, y = lat), size = 0.6, col = 'brown') + 
  geom_sf(data = amrc, fill = NA, col = 'grey30') +
  coord_sf(xlim = c(-79, -67), ylim = c(-5, 13)) + 
  labs(x = 'Lon', y = 'Lat') +
  theme_minimal() + 
  theme(
    axis.text.y = element_text(angle = 90, hjust = 0.5, size = 5), 
    axis.text.x = element_text(size = 5)
  ) +
  annotation_scale(location =  "bl", width_hint = 0.5, text_family = 'georg', text_col = 'grey60', bar_cols = c('grey60', 'grey99'), line_width = 0.2) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"), 
                         style = north_arrow_fancy_orienteering(text_family = 'georg', text_col = 'grey40', line_col = 'grey60', fill = c('grey60', 'grey99'))) 

g.map.pnt

## To save the map
ggsave(plot = g.map.pnt, filename = './png/maps/points-cocoa_ubication.jpg', units = 'in', width = 4, height = 6, dpi = 300, create.dir = TRUE)


# Forest maps -------------------------------------------------------------

## Hansen -----------------------------------
hns.r <- as.factor(hns.r)
levels(hns.r) <- data.frame(value = c(0, 1), label = c("No Bosque", "Bosque"))

g.map.hansen <- ggplot() +
  geom_spatraster(data = hns.r, aes(fill = label)) +  
  scale_fill_manual(
    values = c("No Bosque" = "white", "Bosque" = "forestgreen"),  # Colores personalizados
    name = "Cobertura"  # Nombre de la leyenda
  ) +
  geom_sf(data = dpto, fill = NA, color = 'grey30') +  
  geom_sf(data = amrc, fill = NA, color = 'grey30') + 
  coord_sf(xlim = c(-78.5, -71), ylim = c(0.5, 11)) + 
  labs(x = 'Lon', y = 'Lat') +
  theme_minimal() +
  theme(
    axis.text.y = element_text(angle = 90, hjust = 0.5, size = 5), 
    axis.text.x = element_text(size = 5), 
    legend.position = 'bottom', 
    axis.title = element_text(size = 6), 
    text = element_text(family = 'Segoe UI')
  ) +
  annotation_scale(location =  "bl", width_hint = 0.5, text_family = 'georg', text_col = 'grey60', bar_cols = c('grey60', 'grey99'), line_width = 0.2) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"), 
                         style = north_arrow_fancy_orienteering(text_family = 'georg', text_col = 'grey40', line_col = 'grey60', fill = c('grey60', 'grey99'))) 


ggsave(plot = g.map.hansen, filename = './png/maps/forest-hansen.jpg', units = 'in', width = 4, height = 6, dpi = 300, create.dir = T)

## IDEAM ------------------------------
ide.r <- as.factor(ide.r)
levels(ide.r) <- data.frame(value = c(1, 2, 3), label = c('Bosque', 'No bosque', 'Sin información'))

g.map.ideam <- ggplot() +
  geom_spatraster(data = ide.r, aes(fill = label)) +  
  scale_fill_manual(
    values = c("No bosque" = "white", "Bosque" = "forestgreen", 'Sin información' = '#ffa500'),  # Colores personalizados
    name = "Cobertura", 
    na.value = 'transparent', 
    na.translate = FALSE
  ) +
  geom_sf(data = dpto, fill = NA, color = 'grey30') +  
  geom_sf(data = amrc, fill = NA, color = 'grey30') + 
  coord_sf(xlim = c(-78.5, -71), ylim = c(0.5, 11)) + 
  labs(x = 'Lon', y = 'Lat') +
  theme_minimal() +
  theme(
    axis.text.y = element_text(angle = 90, hjust = 0.5, size = 5), 
    axis.text.x = element_text(size = 5), 
    legend.position = 'bottom', 
    axis.title = element_text(size = 6), 
    text = element_text(family = 'Segoe UI')
  ) +
  annotation_scale(location =  "bl", width_hint = 0.5, text_family = 'georg', text_col = 'grey60', bar_cols = c('grey60', 'grey99'), line_width = 0.2) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"), 
                         style = north_arrow_fancy_orienteering(text_family = 'georg', text_col = 'grey40', line_col = 'grey60', fill = c('grey60', 'grey99'))) 


ggsave(plot = g.map.ideam, filename = './png/maps/forest-ideam.jpg', units = 'in', width = 4, height = 6, dpi = 300, create.dir = T)


## Global Forest Cover 2020 ---------------------
gfc.r <- terra::crop(gfc.r, vect(dpto))
gfc.r <- terra::mask(gfc.r, vect(dpto))
terra::writeRaster(x = gfc.r, filename = './tif/forest/gfc2020/ForestCover_Colombia_2020_masked.tif', overwrite = TRUE)

gfc.r <- as.factor(gfc.r)
levels(gfc.r) <- data.frame(value = c(0, 1), label = c('No bosque', 'Bosque'))

g.map.gfc <- ggplot() +
  geom_spatraster(data = gfc.r, aes(fill = label)) +  
  scale_fill_manual(
    values = c("No bosque" = "white", "Bosque" = "forestgreen"),  
    name = "Cobertura", 
    na.value = 'transparent', 
    na.translate = FALSE
  ) +
  geom_sf(data = dpto, fill = NA, color = 'grey30') +  
  geom_sf(data = amrc, fill = NA, color = 'grey30') + 
  coord_sf(xlim = c(-78.5, -71), ylim = c(0.5, 11)) + 
  labs(x = 'Lon', y = 'Lat') +
  theme_minimal() +
  theme(
    axis.text.y = element_text(angle = 90, hjust = 0.5, size = 5), 
    axis.text.x = element_text(size = 5), 
    legend.position = 'bottom', 
    axis.title = element_text(size = 6), 
    text = element_text(family = 'Segoe UI')
  ) +
  annotation_scale(location =  "bl", width_hint = 0.5, text_family = 'georg', text_col = 'grey60', bar_cols = c('grey60', 'grey99'), line_width = 0.2) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"), 
                         style = north_arrow_fancy_orienteering(text_family = 'georg', text_col = 'grey40', line_col = 'grey60', fill = c('grey60', 'grey99'))) 


ggsave(plot = g.map.gfc, filename = './png/maps/forest-gfc.jpg', units = 'in', width = 4, height = 6, dpi = 300, create.dir = T)


# Function for making the graphs ----------------------------------------------------------
make.graph <- function(src, col){
  
  # src <- 'IDEAM 2017'
  # col <- 'ide2017'
  
  ## Filtering
  cat('To process: ', src, '\n')
  frq <- filter(freq, source == src)
  frq <- mutate(frq, type = factor(type, levels = c('Bosque', 'No bosque')))
  
  ## To make the graph 
  ggc <- ggplot(data = frq %>% drop_na(), aes(x = type, y = count, fill = type)) + 
    geom_col() + 
    scale_fill_manual(values = c('forestgreen', 'grey50')) + 
    labs(x = '', y = 'Número de fincas de cacao (n)', fill = '') + 
    theme_minimal() + 
    theme(
      axis.text.y = element_text(angle = 90, hjust = 0.5), 
      legend.position = 'bottom'
    )
  
  ## To build the data for the map
  pnt <- dplyr::select(pnts, id, lon, lat, dpto, col)
  colnames(pnt)[5] <- 'type'
  
  ## To make the map 
  gpn <-  ggplot() +
    geom_point(data = pnt, aes(x = lon, y = lat, col = type), size = 0.5) +  
    scale_color_manual(
      values = c("No bosque" = "grey50", "Bosque" = "forestgreen"),  # Colores personalizados
      name = '', 
      na.translate = FALSE
    ) +
    geom_sf(data = dpto, fill = NA, color = 'grey30') +  
    geom_sf(data = amrc, fill = NA, color = 'grey30') + 
    coord_sf(xlim = c(-78.5, -71), ylim = c(0.5, 11)) + 
    labs(x = 'Lon', y = 'Lat') +
    guides(col = guide_legend(override.aes = list(size = 5))) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(angle = 90, hjust = 0.5, size = 5), 
      axis.text.x = element_text(size = 5), 
      legend.position = 'bottom', 
      axis.title = element_text(size = 6)
    ) +
    annotation_scale(location =  "bl", width_hint = 0.5, text_family = 'georg', text_col = 'grey60', bar_cols = c('grey60', 'grey99'), line_width = 0.2) +
    annotation_north_arrow(location = "tr", which_north = "true", 
                           pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"), 
                           style = north_arrow_fancy_orienteering(text_family = 'georg', text_col = 'grey40', line_col = 'grey60', fill = c('grey60', 'grey99'))) 
  
  ggsave(plot = gpn, filename = glue('./png/maps/points_{src}.jpg'), units = 'in', width = 4, height = 6, dpi = 300, create.dir = TRUE)
  
  ## To build just one graph 
  gga <- ggpubr::ggarrange(gpn, ggc, ncol = 2, nrow = 1, common.legend = TRUE, legend = 'bottom')
  ggsave(plot = gga, filename = glue('./png/{src}.jpg'), units = 'in', width = 9, height = 5.5, dpi = 300, create.dir = TRUE)
  
  ## Statistics 
  smr <- pnt %>% 
    group_by(dpto, type) %>% 
    reframe(count = n()) %>% 
    spread(type, count) %>% 
    arrange(desc(`No bosque` )) # `No bosque` 
  
  return(smr)
  
  
}

## To apply the function -------------------------------------------------------------------
idem <- make.graph(src = 'IDEAM 2017', col = 'ide2017')
hnsn <- make.graph(src = 'Hansen 2020', col = 'hns2020')
gfcv <- make.graph(src = 'GFC 2020', col = 'gfc2020')

freq %>% filter(source == 'GFC 2020')

gfcv %>% arrange(desc(Bosque))
gfcv %>% arrange(desc(`No bosque`))

freq %>% filter(source == 'IDEAM 2017')
freq %>% filter(source == 'IDEAM 2017') %>% mutate(porc = count / sum(count) * 100)
