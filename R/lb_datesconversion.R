#' Title lb_doytodate
#'
#' @description Converts a doy (or array of doys) to date format. Year has to be specified
#'
#' @param doys to be converted
#' @param year year to be used as basis (careful with LEAP years !!!)
#'
#' @return date / array of dates
#' @export
#'

lb_doytodate = function(doys = doys, year = NULL){

  dates = as.Date(doys - 1, origin = paste0(year, "-01-01"))
  return(dates)
}


#' Title lb_datetodoy
#'
#' Converts a date (or array of dates) to doy
#'
#' @param dates dates to be converted. class = date, posIx, etc....
#'
#' @return doy / array of doys
#' @export
#'

lb_datetodoy = function(dates = dates){

  doys =  as.numeric(strftime(dates, format = '%j'))
  return(doys)

}
