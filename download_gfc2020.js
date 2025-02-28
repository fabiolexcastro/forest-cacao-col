// Cargar el dataset EC JRC Global Forest Cover 2020, V2
var dataset = ee.ImageCollection("JRC/GFC2020/V2");

// Definir el área de Colombia usando fronteras oficiales
var colombia = ee.FeatureCollection("USDOS/LSIB_SIMPLE/2017")
  .filter(ee.Filter.eq("country_na", "Colombia"));

// Verificar las bandas disponibles en el dataset
print("Bandas disponibles en el dataset:", dataset.max().bandNames());

// Seleccionar la banda de cobertura forestal en 2020
var forestCover = dataset.max().select(dataset.max().bandNames());

// Recortar la imagen al área de Colombia //
var forestColombia = forestCover.clip(colombia);

// Visualizar la imagen en el mapa
Map.centerObject(colombia, 6);
Map.addLayer(forestColombia, 
  {min: 0, max: 100, palette: ["ffffff", "006400"]},  // Blanco a verde oscuro
  "Forest Cover 2020");

// Exportar la imagen a Google Drive
Export.image.toDrive({
  image: forestColombia,
  description: "ForestCover_Colombia_2020",
  scale: 30,  // Resolución en metros
  region: colombia.geometry(),
  maxPixels: 1e13,
  fileFormat: "GeoTIFF"
});
