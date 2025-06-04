#' Digital Bathymetric Model disaggregation
#'
#' Disaggregate of a DBM to match a lower resolution one.
#'
#' @param poly study area shape file
#' @param gpred The raster grid to disaggregate
#' @param gref The raster grid to match
#' @param crsInf Projection information : The EPSG code
#'
#' @return The new raster grid at the desired resolution.
#' @export
#'
#' @examples

disagRast = function(poly, gpred, gref, crsInf){

  rasT = raster::raster(xmn = raster::extent(poly)[1], # min longitude
                     xmx = raster::extent(poly)[2], # max longitude
                     ymn = raster::extent(poly)[3], # min latitude
                     ymx = raster::extent(poly)[4], # max latitude
                     res = c(raster::res(gref)[1], raster::res(gref)[2]));

  raster::crs(rasT) = crdref

               r_sp = sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(gpred),as_points = FALSE, merge = TRUE));

               r_sf = sf::st_as_sf(r_sp, coords=c("x","y"));

               sf::st_crs(r_sf) = sf::st_crs(crsInf);

               names(r_sf)[1] = c("target")

               newpred2 = fasterize::fasterize(r_sf, rasT, field= "target");

  return(newpred2);

}
