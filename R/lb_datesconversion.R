#' Title lb_doytodate
#'
#' @param doys to be converted
#' @param year year to be used as basis (careful with LEAP years !!!)
#'
#' @return NULL
#' @export
#'

lb_doytodate = function(doys = doys, year = NULL){

  dates = as.Date(doys - 1, origin = paste0(year, "-01-01"))
  return(dates)
}


#' Title lb_datetodoy
#'
#' @param dates dates to be converted. class = date, posIx, etc....
#'
#' @return NULL
#' @export
#'

lb_datetodoy = function(dates = dates){

  doys =  as.numeric(strftime(dates, format = '%j'))
  return(doys)

}
