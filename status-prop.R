#' Raster grid status and their proportion in the study area
#'
#' @param vs_class A raster grid containing predictions
#' @param shp_re A raster grid with reference values at given resolution
#' @param outRast A raster grid where cells outside a study area are labelled NA and cells inside the study are not
#' @param shp A shape file of the study area
#' @param crsInf The EPSG code
#' @param precision Number of decimals
#'
#' @return A list containing :
#' 1. out_df - data frame containing raster (cells) outside the study area
#' 2. inP_df - data frame containing raster (cells) predictable cell inside the study area
#' 3. inNP_df  - data frame containing raster (cells) non predictable cell inside the study area
#' 4. PropNL - proportion of non predictable cells inside the study area
#' 5. propL - proportion of predictable cells inside the study area
#' 6. outCells - Cell numbers (in the raster grid) that are outside the study area
#' 7. inNLCells  - Non predictable cell numbers (in the raster grid) inside the study area
#' 8. inLCells - Predictable cell numbers (in the raster grid) inside the study area
#' @export
#'
#' @examples
#' 
statusProp = function(vs_class, shp_re, outRast, shp, crsInf, precision=3){

  #vs_class = newpred2; shp_re=shp_ref; outRast=outstudy[[1]]; shp=shp3;  spcrs=sp_wgs84; precision=2;

  names(outRast)= c("layer");

  outCells = which(is.na(raster::getValues(outRast))); # cells numbers outside the study site

  outCellsV = as.data.frame(raster::values(outRast)); # All cells values (inside and outside the study site)

  cels = raster::ncell(outRast); # Total number cells


  cordv = as.data.frame(raster::xyFromCell(outRast, 1:cels));# cells coordinates
  outCellsV_df = cbind(cordv, layer = outCellsV[,1]); # concatenate cells coordinates and values
  outCellsV_df = outCellsV_df[!stats::complete.cases(outCellsV_df), ] # remove na values rows

  outCellsV_df$layer=0; #

  outpts = sp::SpatialPoints(outCellsV_df);
  raster::crs(outpts) = sp::CRS(SRS_string = paste("EPSG:", crsInf, sep=""));

                    out_df = cbind(as.data.frame(outpts), loc = "Outside")

                    # Cells inside the study area and that contain data
                    #pred_sp_dis_mask = mask(vs_class, shp, updateNA=T);

                    #inLCells = which(!is.na(getValues(pred_sp_dis_mask))); # cells numbers

                    inLCells = which(!is.na(raster::getValues(vs_class))); # cells numbers

                    #inptsNE = SpatialPoints(rasterToPoints(pred_sp_dis_mask));
                    inptsNE = sp::SpatialPoints(raster::rasterToPoints(vs_class));
                    raster::crs(inptsNE) = sp::CRS(SRS_string = paste("EPSG:", crsInf, sep=""));

                                       inP_df = cbind(as.data.frame(inptsNE), loc = "Inside-P")

                                       # Cells labelled inside the reference raster grid
                                       names(shp_re) = "layer";
                                       allpts = sp::SpatialPoints(raster::rasterToPoints(shp_re));
                                       raster::crs(allpts) = sp::CRS(SRS_string = paste("EPSG:", crsInf, sep=""));

                                                         allCells = 1:cels; # cells numbers

                                                         # No data cells inside the study area of the predicted map

                                                         inNLCells = as.numeric(allCells[!allCells %in% append(inLCells, outCells)])


                                                         if (length(inNLCells) == 0){

                                                           # Case 1 : There is no data cells inside the study area

                                                           inNLCells = NA
                                                           inNP_df = stats::setNames(data.frame(matrix(ncol = 4, nrow = 0)),  c("x", "y", "layer", "loc"))
                                                           propL = 1
                                                           PropNL = 0

                                                           return(list(out_df, inP_df, inNP_df, PropNL, propL, outCells, inNLCells, inLCells))

                                                         } else {

                                                           inptsE = sp::SpatialPoints(raster::xyFromCell(vs_class, inNLCells));
                                                           raster::crs(inptsE) = sp::CRS(SRS_string = paste("EPSG:", crsInf, sep=""));

                                                                             inNLCells = allCells[!allCells %in% append(outCells,inLCells)];

                                                                             inNP_df = cbind(as.data.frame(inptsE), layer = NA, loc = "Inside-Non-P")

                                                                             PropNL = round(dim(inNP_df)[1]/(dim(inNP_df)[1] + dim(inP_df)[1]), precision)

                                                                             propL = round(dim(inP_df)[1]/(dim(inNP_df)[1]+dim(inP_df)[1]), precision)

                                                                             return(list(out_df, inP_df, inNP_df, PropNL, propL, outCells, inNLCells, inLCells))

                                                         }


}
