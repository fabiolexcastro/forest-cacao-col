
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, colourpicker, geodata, gfcanalysis, rnaturalearthdata, hrbrthemes, ggthemes, ggspatial, showtext, extrafont, ggrepel, stringr, sf, readxl, tidyverse, fs, gtools, terra)

options(scipen = 999, warn = -1)
g <- gc(reset = T)
rm(list = ls())

# Data  -------------------------------------------------------------------

## Vector data 
col0 <- gadm(country = 'COL', level = 0, path = './tmpr')

## Tabular data 
pnts <- read_excel('./tbl/Sitios_Cacao_ClimaLoCa.xlsx')
colnames(pnts) <- c('id', 'lon', 'lat', 'fuente', 'pais', 'dpto', 'mapeo', 'mapeo_cd')

## Points to shapefile 
pnts.sftr <- st_as_sf(x = pnts, coords = c('lon', 'lat'), crs = st_crs(4326))
extn <- ext(pnts.sftr)
extn.poly <- st_as_sf(st_as_sfc(st_bbox(extn, crs = st_crs(4326))))
st_write(extn.poly, './gpkg/zone_pnts.gpkg')

# Download data -----------------------------------------------------------

## MAPBIOMAS
## Source: https://colombia.mapbiomas.org/segunda-coleccion-de-mapbiomas-colombia/
root.mapb <- 'https://storage.googleapis.com/mapbiomas-public/initiatives/colombia/collection_2/lulc/mapbiomas_colombia_collection2_integration_v1/mapbiomas_colombia_collection2_integration_v1-classification_'
name <- basename(root.idea)
years <- 1986:2023

##
map(.x = years, .f = function(y){
  download.file(url = paste0(root.mapb, y, '.tif'), destfile = paste0('./tif/forest/ideam/', name, y, '.tif'), mode = 'wb')
})

## Global Forest Change - Maryland University
## Source: https://storage.googleapis.com/earthenginepartners-hansen/GFC-2023-v1.11/download.html
tles <- gfcanalysis::calc_gfc_tiles(aoi = extn.poly)
tles <- st_transform(tles, crs = st_crs(4326))
tles <- mutate(tles, gid = 1:nrow(tles))
down <- gfcanalysis::download_tiles(tiles = tles, output_folder = './tif/forest/hansen')

## Intersection
tles.intr <- st_intersection(pnts.sftr, tles) # Tiles: 2 & 5

## IDEAM
## Download from: https://visualizador.ideam.gov.co/CatalogoObjetos/geo-open-data?theme=&group=&search=&page=67&size=9

# Hansen deforestation ----------------------------------------------------
hnsn <- as.character(dir_ls('./tif/forest/hansen'))

## Extract Colombia forest
frst.hnsn <- gfcanalysis::extract_gfc(aoi = extn.poly, data_folder = './tif/forest/hansen')
plot(frst.hnsn)
terra::writeRaster(x = frst.hnsn, filename = './tif/forest/hansen_extr.tif', overwrite = TRUE)

## Threshold 
trhs.hnsn <- gfcanalysis::threshold_gfc(gfc = frst.hnsn)
terra::writeRaster(x = trhs.hnsn, filename = './tif/forest/hansen_thrs.tif')


















