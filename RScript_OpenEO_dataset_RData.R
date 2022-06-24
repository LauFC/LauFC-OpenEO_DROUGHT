#Once the data has been downloaded from the Open EO platform and pre-processed, 
#it is saved in *.RData format for further processing.
save(result_fapar,
     result_ndvi,
     file="OpenEO_platform_indices.RData")
save(result_prec,
     result_tdew,
     result_tmax,
     result_tmed,
     result_tmin,
     result_radi,
     result_pres,
     result_wind,
     file="OpenEO_platform_meteo_data.RData")
save(result_map,
     result_dem,
     file="OpenEO_platform_landcover_dem.RData")



