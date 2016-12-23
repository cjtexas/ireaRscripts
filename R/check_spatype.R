#' check_spatype 
#' 
#' Wrapper to check if an object passed to the function corresponds to wither a Spatial Object, 
#' an R raster object, a file corresponding to a vector, or a file corresponding to a raster
#'
#' @param object either an ]SP object or a character vector pointing to a vector or raster layer
#'
#' @return character vector equal to spobject, rastobject, vectfile or rastfile
#' @export
#' 
#' @importFrom rgdal readOGR
#' @importFrom tools file_path_sans_ext
#'
#' @examples

check_spatype <- function(object) {
  
  if (class(object) %in% c("SpatialPolygonsDataFrame", "SpatialPolygons",
                           "SpatialPointsDataFrame", "SpatialPoints", "SpatialLines", "SpatialLinesDataFrame", 
                           "RasterLayer", "RasterStack", "RasterBrick")) {
    
    sp_type <- ifelse(class(object) %in% c("RasterLayer", "RasterStack", "RasterBrick"), "rastobject", "spobject")
    
  } else {
    
    if (class(object) == "character") {
      
      vecttry <- try(readOGR(dirname(object), basename(file_path_sans_ext(object))), silent = TRUE)
      
      if (class(vecttry) == "try-error") { 
        
        rastry <- try(raster(object))
        
        if (class(rastry) == "try-error") {
          
          sp_type = "none"
          
        } else {
          sp_type = "rastfile"
        }
      } else {
        sp_type = "vectfile"
      }
    } else {
      
      sp_type = "none"
      
    }
  }
  return(sp_type)
}
