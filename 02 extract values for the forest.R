


# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(colourpicker, geodata, gfcanalysis, rnaturalearthdata, hrbrthemes, ggthemes, ggspatial, showtext, extrafont, ggrepel, stringr, sf, readxl, tidyverse, fs, gtools, terra)

options(scipen = 999, warn = -1)
g <- gc(reset = T)
rm(list = ls())

# Load data ----------------------------------------------------------------

## Tabular data ------------------------------------------------------------
pnts <- read_excel('./tbl/Sitios_Cacao_ClimaLoCa.xlsx')
colnames(pnts) <- c('id', 'lon', 'lat', 'fuente', 'pais', 'dpto', 'mapeo', 'mapeo_cd')

## Raster data -------------------------------------------------------------

## GFC 2020
gfc.r <- terra::rast('./tif/forest/gfc2020/ForestCover_Colombia_2020.tif')

## IDEAM 
ide.r <- terra::rast('./tif/forest/ideam/Bosque_No_Bosque_2017/Bosque_No_Bosque_2017/Geotiff/SBQ_SMBYC_BQNBQ_V7_2017.tif')
ide.r <- ide.r * 1
terra::writeRaster(x = ide.r, filename = './tif/forest/ideam/bosque-nobosque_2017.tif', overwrite = TRUE)
ide.r <- terra::rast('./tif/forest/ideam/bosque-nobosque_2017.tif')

## Hansen
hns.r <- terra::rast('./tif/forest/hansen_thrs.tif')

### Forest / Not forest 2020
forest2000 <- hns.r[[1]]  
lossyear <- hns.r[[2]]  
loss_mask <- (lossyear > 0) & (lossyear <= 20)  

###
forest2020 <- forest2000
forest2020[loss_mask] <- 0
terra::writeRaster(x = forest2020, filename = './tif/forest/hansen_2020.tif')

hns.r <- forest2020
hns.r <- terra::rast('./tif/forest/hansen_2020.tif')

# Extract the values ------------------------------------------------------

## GFC 2020
gfc.v <- cbind(pnts, terra::extract(gfc.r, pnts[,c('lon', 'lat')]))
gfc.v <- dplyr::select(gfc.v, id, lon, lat, fuente, dpto, mapeo, mapeo_cd, gfc2020 = Map)
gfc.v <- as_tibble(gfc.v)
gfc.v <- mutate(gfc.v, gfc2020 = ifelse(gfc2020 == '0', 'No bosque', 'Bosque'))
gfc.v <- mutate(gfc.v, gfc2020 = factor(gfc2020, levels = c('No bosque', 'Bosque')))
table(gfc.v$gfc2020)

## IDEAM 
ide.v <- cbind(pnts, terra::extract(ide.r, pnts[,c('lon', 'lat')]))
ide.v <- dplyr::select(ide.v, id, lon, lat, fuente, dpto, mapeo, mapeo_cd, ide2017 = Tipo_Cobertura)
ide.v <- as_tibble(ide.v)
ide.v <- mutate(ide.v, ide2017 = ifelse(ide2017 == 1, 'Bosque', ifelse(ide2017 == 2, 'No bosque', 'Sin información')))
table(ide.v$ide2017)

## Hansen
hns.v <- cbind(pnts, terra::extract(hns.r, pnts[,c('lon', 'lat')]))
hns.v <- dplyr::select(hns.v, id, lon, lat, fuente, dpto, mapeo, mapeo_cd, hns2020 = hansen_thrs_1)
hns.v <- as_tibble(hns.v)
hns.v <- mutate(hns.v, hns2020 = ifelse(hns2020 == '0', 'No bosque', 'Bosque'))
hns.v <- mutate(hns.v, hns2020 = factor(hns2020, levels = c('No bosque', 'Bosque')))

# To join the results into only one table ---------------------------------
tbl <- list(gfc.v, ide.v, hns.v) %>% reduce(inner_join)
write.csv(tbl, './tbl/Sitios_Cacao_ClimaLoCa_forests-noforests.csv', row.names = FALSE)

# To make the graph histogram ---------------------------------------------

qnt <- tbl %>% 
  dplyr::select(gfc2020, ide2017, hns2020) %>% 
  gather(var, type) %>% 
  inner_join(., tibble(var = c('gfc2020', 'ide2017', 'hns2020'), source = c('GFC 2020', 'IDEAM 2017', 'Hansen 2020')), by = 'var') %>% 
  dplyr::select(source, type) %>% 
  group_by(source, type) %>% 
  reframe(count = n()) 

write.csv(qnt, './tbl/forest-count_v1.csv', row.names = FALSE)


