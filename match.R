#' Match score
#'
#' Compute the proportion of cells that where predicted and reference typologies are the same
#'
#' @param vs_classified A raster grid with predicted typologies
#' @param shp_ref A raster grid of reference typologies
#' @param precision The number of decimal for the match score
#' @param lostCells Default value to affect to rasters located outside the study area: 99
#' @param inCcode Default value to affect to empty rasters located inside the study area
#' @param correct Logical indicating whether or not to remove empty rasters inside the study area before calculations (default is TRUE).
#'
#' @return A list containing :
#' 1. cross-tabulation of reference and predicted typologies with associated statistics.
#' 2. The Match score.
#' @export
#'
#' @importFrom stats na.omit
#'
#' @examples

matchRast = function(vs_classified, shp_ref, precision=2, lostCells = 999, inCcode = 99,  correct=TRUE){


  if (correct==FALSE){

    #################################################
    #              UNCORRECTED MATCH                #
    #################################################

    # Impredictible cells

    NaCells0 = which(is.na(raster::getValues(shp_ref)));

    shp_ref0 = shp_ref;

    vs_classified0 = vs_classified;

    # Replace their values with NA to ignore them when computing the match

    vs_classified0[[1]][NaCells0] = NA;

    # Compare the predicted and the reference raster grids

    diff = raster::overlay(shp_ref0, vs_classified0, fun = function(a,b) return(a==b));

    # Compute match, mismatch
    ## Warning : NA values inside the study area are not yet corrected, they are just ignored
    diff_df = as.data.frame(raster::values(diff))
    names(diff_df) = c("layer")

    staTIS = dplyr::count(stats::na.omit(diff_df), layer);

    staTIS$perc = round((staTIS$n/sum(staTIS$n)), precision);

    return(list(staTIS, diff))

  } else {

    #################################################
    #              CORRECTED MATCH                #
    #################################################

    # Cell numbers outside the study area

    NaCells0 = which(is.na(raster::getValues(shp_ref)))

    shp_ref0 = shp_ref

    vs_classified0 = vs_classified

    # Replace their values by the same value in reference and predicted raster grids

    shp_ref0[[1]][NaCells0] = lostCells

    vs_classified0[[1]][NaCells0] = lostCells


    # Cells inside the study area that get NA values after the desaggregation

    NaCells1 = which(is.na(raster::getValues(vs_classified0)))

    ####################################################

    # Replace values of cells outside the study area to ignore them when computing the Match

    shp_ref1 = shp_ref

    shp_ref1[[1]][NaCells0] = NA

    vs_classified1 = vs_classified

    vs_classified1[[1]][NaCells0] = NA

    # Replace NA cells values inside the study area in order to consider them are wrong predictions and
    # to take them into account whem computing the corrected Match

    vs_classified1[[1]][NaCells1] = inCcode

    # Compare the predicted and the reference raster grids

    diff = raster::overlay(shp_ref1, vs_classified1, fun = function(a,b) return(a==b))

    diff_df = as.data.frame(raster::values(diff))
    names(diff_df) = c("layer")

    staTIS = dplyr::count(stats::na.omit(diff_df), layer)

    staTIS$perc = round((staTIS$n/sum(staTIS$n)), precision)

    return(list(staTIS, diff))

  };

}
