#' "Balanced Match" score.
#'
#' The confusion matrix between predicted typologies raster (cell) grid and expert labels raster grid is used to calculated a Balanced Accuracy score (Grandini, 2020).
#' Note : Empty rasters inside and outside the study area are excluded from calculations.
#'
#' @param vs_classified DTM as a SpatRaster or RasterLayer in a projected coordinate system containing the predicted typologies.
#' @param shp_ref DTM as a SpatRaster or RasterLayer in a projected coordinate system containing the reference typologies.
#' @param correct Logical indicating whether or not to remove empty rasters inside the study area before calculations (default is TRUE).
#'
#' @return A list with :
#' 1. cross-tabulation of reference and predicted typologies with associated statistics.
#' 2. The Balanced Match score.
#' @export
#'
#' @examples
#'
#' @references
#' Grandini, M., Bagli, E., & Visani, G. (2020). Metrics for multi-class classification: an overview. arXiv preprint arXiv:2008.05756.
#'
balmatchRast = function(vs_classified, shp_ref, correct=T){

  # Cell numbers outside the study area

  NaCells0 = which(is.na(raster::getValues(shp_ref)))

  shp_ref0 = shp_ref

  vs_classified0 = vs_classified

  # Replace their values by the same value in reference et predicted raster grids

  shp_ref0[[1]][NaCells0] = 999

  vs_classified0[[1]][NaCells0] = 999

  ####################################################

  # Cells numbers inside the study area that has no label in the predicted map

  NaCells1 = which(is.na(raster::getValues(vs_classified0)))

  ####################################################

  vs_classified0[[1]][NaCells1] = 99

  ####################################################
  if (correct==FALSE){

    # vs_classified=pred_sp_dis; shp_ref=ref_sp_mask;

    shp_ref0[[1]][NaCells1] = 99

    dfcartExp = as.data.frame(raster::values(shp_ref0));
    names(dfcartExp) = c("ref");
    dfcartExp$ref = as.factor(dfcartExp$ref);

    # Turn predicted raster to data frame

    dfcartPred = as.data.frame(raster::values(vs_classified0));
    names(dfcartPred) = c("pred");
    dfcartPred$pred = as.factor(dfcartPred$pred);

    dfcombine= data.frame(ref= dfcartExp$ref, pred=dfcartPred$pred);

    dfcombine = subset(dfcombine, pred!="999" & pred!="99")

    dfcombine=droplevels(dfcombine)


    # Levels in the two data frames should be the same

    levels(dfcombine$pred) = append(levels(dfcombine$pred), levels(dfcombine$ref)[!levels(dfcombine$ref) %in% levels(dfcombine$pred)]);

    # Levels in the two data frames should be in the same order for comparison (without changing levels values)

    dfcombine$predfin = factor(dfcombine$pred, levels=levels(dfcombine$ref)); # by this way only order in levels object is changed

    # Confusion matrix

    StatByClassbis = caret::confusionMatrix(data = dfcombine$predfin, reference = dfcombine$ref, mode = "prec_recall");

    # Get the balanced match (Accuracy)

    if (length(unique((dfcombine$predfin)))>2){

      balMatch = round(colMeans(as.data.frame(StatByClassbis$byClass))[[6]], 2)

    } else {

      balMatch = round(StatByClassbis$byClass[["Balanced Accuracy"]], 2)

    }

  } else {

    shp_ref0[[1]][NaCells0] = NA

    vs_classified0[[1]][NaCells0] = NA

    dfcartExp = as.data.frame(raster::values(shp_ref0));
    names(dfcartExp) = c("ref");
    dfcartExp$ref = as.factor(dfcartExp$ref);

    # predictions: from raster to data frame

    dfcartPred = as.data.frame(raster::values(vs_classified0));
    names(dfcartPred) = c("pred");
    dfcartPred$pred = as.factor(dfcartPred$pred);


    dfcombine= data.frame(ref= dfcartExp$ref, pred=dfcartPred$pred);


    n = length(levels(dfcombine$ref))

    # Levels in the two data frames should be the same

    levels(dfcombine$pred) = append(levels(dfcombine$pred), levels(dfcombine$ref)[!levels(dfcombine$ref) %in% levels(dfcombine$pred)]);

    levels(dfcombine$ref) = append(levels(dfcombine$ref), levels(dfcombine$pred)[!levels(dfcombine$pred) %in% levels(dfcombine$ref)]);

    # Levels in the two data frames should be in the same order for comparison (without changing levels values)

    dfcombine$predfin = factor(dfcombine$pred, levels=levels(dfcombine$ref)); # by this way only order in levels object is changed

    # Confusion matrix

    StatByClassbis = caret::confusionMatrix(data = dfcombine$predfin, reference = dfcombine$ref, mode = "prec_recall");

    # Get the balanced match (Accuracy)

    if (length(unique((dfcombine$predfin)))>2){

      balMatch = round(sum(as.data.frame(StatByClassbis$byClass)[1:n,][[6]])/(n+1), 2)

    } else {

      balMatch = round(StatByClassbis$byClass[["Balanced Accuracy"]], 2)

    }
}

  return(list(StatByClassbis, balMatch));


}
