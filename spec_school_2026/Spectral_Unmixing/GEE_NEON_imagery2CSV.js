var coordref = 'EPSG:32617'
var resolution = 1
var colormap = {min:200, max:2000}

// Get Jornada NEAT hyperspectral image //HSI_REFL
// ee.ImageCollection("projects/neon-prod-earthengine/assets/DEM/001")
var jorn_hyp = ee.ImageCollection("projects/neon-prod-earthengine/assets/DEM/001")
  .filterDate('2023-01-01', '2023-12-31')
  .filterMetadata('NEON_SITE', 'equals', 'MLBS')
  .first();
  
//print(jorn_hyp.projection());
// Create a 3-band true-color image of hyperspectral image
var jorn_hyp_RGB = jorn_hyp //.select(['B053', 'B035', 'B019']);

// Show RGB and hyperspectral (reduced to RGB) images
Map.addLayer(jorn_hyp_RGB , colormap , 'MLBS');

/*var table4 = table3.map(function(feature) { // change CRS of trails
  return feature.setGeometry(
    feature.geometry().transform(coordref)
  );
});
*/

// Create a FeatureCollection of points
// Create a FeatureCollection of all points
var points = ee.FeatureCollection([
  ee.Feature(ee.Geometry.Point([-80.518088, 37.37222])),
  ee.Feature(ee.Geometry.Point([-80.518084, 37.37218])),
  ee.Feature(ee.Geometry.Point([-80.517837, 37.371816])),
  ee.Feature(ee.Geometry.Point([-80.517035, 37.370071])),
  ee.Feature(ee.Geometry.Point([-80.517014, 37.370088])),
  ee.Feature(ee.Geometry.Point([-80.517025, 37.370087])),
  ee.Feature(ee.Geometry.Point([-80.517010, 37.370097])),
  ee.Feature(ee.Geometry.Point([-80.517019, 37.370056])),
  ee.Feature(ee.Geometry.Point([-80.517014, 37.370065])),
  ee.Feature(ee.Geometry.Point([-80.517024, 37.370103])),
  ee.Feature(ee.Geometry.Point([-80.517052, 37.370045])),
  ee.Feature(ee.Geometry.Point([-80.517073, 37.370044])),
  ee.Feature(ee.Geometry.Point([-80.517064, 37.370064])),
  ee.Feature(ee.Geometry.Point([-80.517061, 37.370063])),
  ee.Feature(ee.Geometry.Point([-80.517040, 37.370012])),
  ee.Feature(ee.Geometry.Point([-80.517029, 37.370015])),
  ee.Feature(ee.Geometry.Point([-80.517044, 37.369928])),
  ee.Feature(ee.Geometry.Point([-80.517059, 37.369907])),
  ee.Feature(ee.Geometry.Point([-80.517105, 37.369841])),
  ee.Feature(ee.Geometry.Point([-80.517091, 37.369806])),
  ee.Feature(ee.Geometry.Point([-80.517192, 37.370257])),
  ee.Feature(ee.Geometry.Point([-80.517169, 37.370229])),
  ee.Feature(ee.Geometry.Point([-80.517160, 37.370207])),
  ee.Feature(ee.Geometry.Point([-80.517165, 37.370152])),
  ee.Feature(ee.Geometry.Point([-80.517175, 37.370094])),
  ee.Feature(ee.Geometry.Point([-80.517174, 37.370080])),
  ee.Feature(ee.Geometry.Point([-80.517178, 37.370062])),
  ee.Feature(ee.Geometry.Point([-80.517195, 37.370023])),
  ee.Feature(ee.Geometry.Point([-80.517201, 37.369982])),
  ee.Feature(ee.Geometry.Point([-80.517218, 37.369882]))
]);

var points = points.map(function(feature) {
  return feature.setGeometry(
    feature.geometry().transform(coordref)
  );
});

var firstPoint = points.first().geometry();

print(firstPoint.projection());
print(firstPoint.projection().crs());// Display points on map


// Export hyperspectral image as tiff
var bandsToExport = ["B001","B002","B003","B004","B005","B006","B007","B008","B009","B010","B011","B012","B013","B014","B015","B016","B017","B018","B019","B020","B021","B022","B023","B024","B025","B026","B027","B028","B029","B030","B031","B032","B033","B034","B035","B036","B037","B038","B039","B040","B041","B042","B043","B044","B045","B046","B047","B048","B049","B050","B051","B052","B053","B054","B055","B056","B057","B058","B059","B060","B061","B062","B063","B064","B065","B066","B067","B068","B069","B070","B071","B072","B073","B074","B075","B076","B077","B078","B079","B080","B081","B082","B083","B084","B085","B086","B087","B088","B089","B090","B091","B092","B093","B094","B095","B096","B097","B098","B099","B100","B101","B102","B103","B104","B105","B106","B107","B108","B109","B110","B111","B112","B113","B114","B115","B116","B117","B118","B119","B120","B121","B122","B123","B124","B125","B126","B127","B128","B129","B130","B131","B132","B133","B134","B135","B136","B137","B138","B139","B140","B141","B142","B143","B144","B145","B146","B147","B148","B149","B150","B151","B152","B153","B154","B155","B156","B157","B158","B159","B160","B161","B162","B163","B164","B165","B166","B167","B168","B169","B170","B171","B172","B173","B174","B175","B176","B177","B178","B179","B180","B181","B182","B183","B184","B185","B186","B187","B188","B189","B190","B191","B192","B193","B194","B195","B196","B197","B198","B199","B200","B201","B202","B203","B204","B205","B206","B207","B208","B209","B210","B211","B212","B213","B214","B215","B216","B217","B218","B219","B220","B221","B222","B223","B224","B225","B226","B227","B228","B229","B230","B231","B232","B233","B234","B235","B236","B237","B238","B239","B240","B241","B242","B243","B244","B245","B246","B247","B248","B249","B250","B251","B252","B253","B254","B255","B256","B257","B258","B259","B260","B261","B262","B263","B264","B265","B266","B267","B268","B269","B270","B271","B272","B273","B274","B275","B276","B277","B278","B279","B280","B281","B282","B283","B284","B285","B286","B287","B288","B289","B290","B291","B292","B293","B294","B295","B296","B297","B298","B299","B300","B301","B302","B303","B304","B305","B306","B307","B308","B309","B310","B311","B312","B313","B314","B315","B316","B317","B318","B319","B320","B321","B322","B323","B324","B325","B326","B327","B328","B329","B330","B331","B332","B333","B334","B335","B336","B337","B338","B339","B340","B341","B342","B343","B344","B345","B346","B347","B348","B349","B350","B351","B352","B353","B354","B355","B356","B357","B358","B359","B360","B361","B362","B363","B364","B365","B366","B367","B368","B369","B370","B371","B372","B373","B374","B375","B376","B377","B378","B379","B380","B381","B382","B383","B384","B385","B386","B387","B388","B389","B390","B391","B392","B393","B394","B395","B396","B397","B398","B399","B400","B401","B402","B403","B404","B405","B406","B407","B408","B409","B410","B411","B412","B413","B414","B415","B416","B417","B418","B419","B420","B421","B422","B423","B424","B425","B426"]
//var bandsToExport = ['R','G','B']
//var bandsToExport = ['B053', 'B035', 'B019']
var selectedImage = jorn_hyp.select(bandsToExport);
var imageWithCoords = selectedImage.addBands(ee.Image.pixelLonLat());
//var roi = ee.FeatureCollection('projects/ee-simonng100/assets/NEAT_B3T4D_shapefile');
var roi = ee.FeatureCollection('projects/ee-simonng100/assets/roi_ferns_v3');

var pixelFeatures = imageWithCoords.sample(roi, resolution) // 1m scale, adjust as needed
    .map(function(feature) {
        return feature.set('latitude', feature.geometry().coordinates().get(1))
                      .set('longitude', feature.geometry().coordinates().get(0));
    });
    
var exportParams = {
  image: selectedImage,
  description: '2023_MLBS_hyp_raster_v3',
  folder: 'Google_Earth', // Optional: specify a folder in Drive
  //fileNamePrefix: 'landsat_rgb', // Optional: prefix for the output file
  scale: resolution, // Export at 0.1-meter resolution
  region: roi,
  crs: coordref, // Optional: specify WGS84 CRS
  maxPixels: 1e9 // Allow up to 1 billion pixels
};

// Start the export task
Export.image.toDrive(exportParams);

// Export hyperspectral image as csv
//Convert each pixel to a feature with band values as properties
var fc = selectedImage.sample({
  region: roi,
  scale: resolution,            // set to image resolution
  projection: coordref,
  geometries: true     // set to true if you want pixel lat/long
});

// Export the FeatureCollection to CSV
Export.table.toDrive({
  collection: fc,
  description: '2023_MLBS_hyp_csv_v3',
  fileFormat: 'CSV'
});

Map.addLayer(roi, {} , 'ROI');
var clippedImage = jorn_hyp_RGB.clip(roi); // clip hyperspectral image (all wavelengths)
Map.addLayer(clippedImage , colormap , 'clipped hyp');
Map.addLayer(points, {color: 'red'}, 'Sampling Points');
Map.addLayer(table3 , {color:'red'}, 'Trails');
print(table3.first().geometry().projection());

Map.centerObject(roi,19);
