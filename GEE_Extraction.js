//Load HCDN watershed shapefiles

var shedShapes = ee.FeatureCollection("users/edethier/usgs-gages/HCDN_watersheds_nldi");

var shedList = shedShapes.toList(shedShapes.size());

var subsetSheds = ee.FeatureCollection([shedList.get(0), shedList.get(1)]);

//Load the PRISM Monthly Averaged dataset

var prismDataset = ee.ImageCollection("OREGONSTATE/PRISM/AN81m");

//Select the temp or ppt band (mm/month)

var prismTemp = prismDataset.select('tmean');

var prismPcpt = prismDataset.select('ppt');

// Get selectedWatershed

  var curShed = shedShapes.filter(ee.Filter.eq('identifier', 'USGS-01013500'));

// Set shed Id

  var curShedID = ee.Feature(curShed.toList(curShed.size()).get(0)).get('identifier');

// Center map on selected state

  Map.centerObject(curShed, 5);

// Define visParams dictionary and display for selected watershed

  var shown = true; // true or false

  var opacity = 0.5; // number [0-1]

  var shedVis = {color: 'red'};

// Add the shed to the map display

  Map.addLayer(curShed, shedVis, 'curShed', shown, opacity);

 // print(curShed)

  //print(curShed.first.get("identifier"))

//Define reduce region parameters

var reduceRegionParams = {

    reducer: ee.Reducer.mean(),

    geometry: curShed.geometry(),

    scale: 100,

  };

//------------------------------------------------------------

//------------------------------------------------------------

// Function to calculate mean pcpt (mm) in

// each watershed for given year

//------------------------------------------------------------

var monthlyPcptTemp = function(passedYear) {

  var year = passedYear


  // Filter image collection for given water year

  var startDate = ee.Date.fromYMD(year, 1, 1)

  var endDate = startDate.advance(1, 'year')

  var filteredPcpt = prismPcpt.filter(ee.Filter.date(startDate, endDate));

  var filteredTemp = prismTemp.filter(ee.Filter.date(startDate, endDate));

 

  // Convert monthly images to list

  var monthlyPcptImages = filteredPcpt.toList(filteredPcpt.size());

  var monthlyTempImages = filteredTemp.toList(filteredPcpt.size());

 

// Create feature to return

  var featureToReturn = ee.Feature(null, {

    'calYear': year,

    'shedID': curShedID,

 

    'Pcpt01': ee.Image(monthlyPcptImages.get(0)).reduceRegion(reduceRegionParams).get("ppt"),

    'Pcpt02': ee.Image(monthlyPcptImages.get(1)).reduceRegion(reduceRegionParams).get("ppt"),

    'Pcpt03': ee.Image(monthlyPcptImages.get(2)).reduceRegion(reduceRegionParams).get("ppt"),

    'Pcpt04': ee.Image(monthlyPcptImages.get(3)).reduceRegion(reduceRegionParams).get("ppt"),

    'Pcpt05': ee.Image(monthlyPcptImages.get(4)).reduceRegion(reduceRegionParams).get("ppt"),

    'Pcpt06': ee.Image(monthlyPcptImages.get(5)).reduceRegion(reduceRegionParams).get("ppt"),

    'Pcpt07': ee.Image(monthlyPcptImages.get(6)).reduceRegion(reduceRegionParams).get("ppt"),

    'Pcpt08': ee.Image(monthlyPcptImages.get(7)).reduceRegion(reduceRegionParams).get("ppt"),

    'Pcpt09': ee.Image(monthlyPcptImages.get(8)).reduceRegion(reduceRegionParams).get("ppt"),

    'Pcpt10': ee.Image(monthlyPcptImages.get(9)).reduceRegion(reduceRegionParams).get("ppt"),

    'Pcpt11': ee.Image(monthlyPcptImages.get(10)).reduceRegion(reduceRegionParams).get("ppt"),

    'Pcpt12': ee.Image(monthlyPcptImages.get(11)).reduceRegion(reduceRegionParams).get("ppt"),

 

    'Temp01': ee.Image(monthlyTempImages.get(0)).reduceRegion(reduceRegionParams).get("tmean"),

    'Temp02': ee.Image(monthlyTempImages.get(1)).reduceRegion(reduceRegionParams).get("tmean"),

    'Temp03': ee.Image(monthlyTempImages.get(2)).reduceRegion(reduceRegionParams).get("tmean"),

    'Temp04': ee.Image(monthlyTempImages.get(3)).reduceRegion(reduceRegionParams).get("tmean"),

    'Temp05': ee.Image(monthlyTempImages.get(4)).reduceRegion(reduceRegionParams).get("tmean"),

    'Temp06': ee.Image(monthlyTempImages.get(5)).reduceRegion(reduceRegionParams).get("tmean"),

    'Temp07': ee.Image(monthlyTempImages.get(6)).reduceRegion(reduceRegionParams).get("tmean"),

    'Temp08': ee.Image(monthlyTempImages.get(7)).reduceRegion(reduceRegionParams).get("tmean"),

    'Temp09': ee.Image(monthlyTempImages.get(8)).reduceRegion(reduceRegionParams).get("tmean"),

    'Temp10': ee.Image(monthlyTempImages.get(9)).reduceRegion(reduceRegionParams).get("tmean"),

    'Temp11': ee.Image(monthlyTempImages.get(10)).reduceRegion(reduceRegionParams).get("tmean"),

    'Temp12': ee.Image(monthlyTempImages.get(11)).reduceRegion(reduceRegionParams).get("tmean"),

    });

 

  return featureToReturn;


  };

 

 

// Function to calculate mean pcpt (mm) in

//  watershed for each year

//------------------------------------------------------------

 

var GetShedPcptTemp = function(passedShed) {

 

//Set global variable curShedID to current shed

//curShedID = ee.Feature(passedShed.toList(passedShed.size()).get(0)).get('identifier');

curShedID = passedShed.get('identifier');


//Redefine reduce region parameters

reduceRegionParams = {

    reducer: ee.Reducer.mean(),

    geometry: passedShed.geometry(),

    scale: 100,

  };

 

// Create list of years for which there are PRISM data

 

var years = ee.List.sequence(1950, 2021);

//var years = ee.List.sequence(1960, 1960);

 

// Map the yearlyPcpt function to each element in the years list and

// organize the returned ee.Features (from the function)

// into a Feature Collection

 

var shedRecord =

  ee.FeatureCollection(years.map(monthlyPcptTemp));

 

 

  return(shedRecord)

}

 

 

//var yearlyCollection = ee.FeatureCollection(GetyearlyPcpt(subsetSheds.first()))

//var tmp = subsetSheds.map(GetyearlyPcpt)

//print (yearlyCollection)

 

// Map the yearlyPcpt function to each element in the years list and

// organize the returned ee.Features (from the function)

// into a Feature Collection

var shedAnnRecord =

  ee.FeatureCollection(shedShapes.map(GetShedPcptTemp));

 

// Since each year within the FeatureCollection shedAnnRecord is also a Feature Collection

// we need to "flattend" the yearly FeatureCollection so that each will get printed.

shedAnnRecord = shedAnnRecord.flatten();

 

 

Export.table.toDrive({

  collection: shedAnnRecord,

  folder: 'earthengine/Erikson',

  fileNamePrefix: 'hcdnSeasonalPT',

  fileFormat: 'CSV'});