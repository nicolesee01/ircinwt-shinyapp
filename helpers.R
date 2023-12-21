m = function(map) {
  leaflet() %>% 
    
    #3 base maps
    addProviderTiles(providers$OpenStreetMap, group = "OSM") %>% 
    addProviderTiles(providers$CartoDB.Positron, group = "Carto") %>% 
    addProviderTiles(providers$Esri.WorldPhysical, group = "ESRI World Physical") %>% 
    
    addPolygons(data=map, color = "black", weight=1.5, fillColor = "#D4D4D4", 
                fillOpacity = 0.1, group = "Area")
}

#function to generate proportion of wetlands in an area
generate_wtprop = function(cifor, area) {
  ciforinarea = mask(crop(cifor, extent(area)), area) #clip raster to mask layer
  ciforinarea[ciforinarea==0 | ciforinarea==20] = NA #remove 0 and 20, which is the mangroves layer
  
  df = data.frame(exact_extract(ciforinarea, area)) 
  sum_df = aggregate(coverage_fraction~value, df, sum)
  
  wtprop = sum(sum_df$coverage_fraction)/ sum(df$coverage_fraction) * 100
  wtprop = round(wtprop, 2)
  print(paste0(wtprop, "%"))
}

#function to generate amount of irrecoverable carbon in an area
generate_icamt = function(ic, area) {
  icinarea = mask(crop(ic, extent(area)), area)
  res = res(icinarea)[1]
  xval = (extent(icinarea)[1] + extent(icinarea)[2])/2
  yval = (extent(icinarea)[3] + extent(icinarea)[4])/2
  
  celllength = distance(cbind(xval, yval), cbind(xval, yval+res), lonlat=TRUE)
  areainha = (celllength^2)/10000 #finding the area of each cell
  icval = sum(values(icinarea), na.rm = TRUE)
  final_val = round(((icval * areainha)/1000000), 3)
  return(final_val)
}

#function to generate an irrecoverable carbon raster layer in areas with wetlands
generate_icinwt = function(cifor, ic, area) {
  icinarea = mask(crop(ic, extent(area)), area)
  ciforinarea = mask(crop(cifor, extent(area)), area)
  
  icinarea[icinarea==0] = NA #remove areas without any irrecoverable carbon
  ciforinarea[ciforinarea==0 | ciforinarea==20] = NA
  icinarea = resample(icinarea, ciforinarea, method="ngb")
  
  #raster calculator to extract areas where irrecoverable carbon and wetlands overlap
  icinwtinarea = overlay(icinarea, ciforinarea, fun=function(icinarea, ciforinarea){return(icinarea - ciforinarea/ciforinarea + 1)})
  return(icinwtinarea)
}





