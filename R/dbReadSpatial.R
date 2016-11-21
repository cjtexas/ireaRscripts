#' dbReadSpatial
#' @description Import a spatial table / view / materialised view from a PostgreSQL
#' database into a \code{Spatial*DataFrame} object.
#' @details The function reads a PostGIS table / view / materialised view from a database PostgreSQL,
#' in which a spatial column (format geom) must be present, and import it in \code{R} as a \code{sp} object
#' (\code{\linkS4class{SpatialPixelsDataFrame}}, \code{\linkS4class{SpatialLinesDataFrame}} or
#' \code{\linkS4class{SpatialPolygonsDataFrame}}).
#' The name of the geometric column is automatically retrieved, while the primary key is retrieved
#' only reading a table (in case a view or materialised view is read, it must be specified with the
#' parameter \code{pkey}).
#'
#' @param conn a \code{\linkS4class{DBIConnection}} object, as produced by \code{\link[DBI]{dbConnect}}.
#' @param name \code{character} the name of the spatial table / view / materialised view.
#' If the table is not in the "public" schema, use a two-length character vector in the form
#' \code{c("schema_name","table_name")}.
#' @param pkey \code{character} (optional) the name of the primary key. This is needed only in the case of
#' views or materialised views (the primary key of a table is automatically retrieved).
#' @param ...
#'
#' @return An object of class \code{\linkS4class{SpatialPixelsDataFrame}},
#' \code{\linkS4class{SpatialLinesDataFrame}} or \code{\linkS4class{SpatialPolygonsDataFrame}},
#' depending on the specific geometry which is read.
#'
#' @author Luigi Ranghetti, phD (2016) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @export
#' @import sp
#' @import RPostgreSQL
#'
#' @examples
#'
dbReadSpatial <- function(conn, name, pkey=NULL, ...) {

  require(rgeos); require(sp)

  # Checks
  if (length(name) == 2) {
    table_schema <- name[1]
    table_name <- name[2]
  } else if (length(name) == 1) {
    table_schema <- "public"
    table_name <- name[1]
  } else stop("\"name\" must be a 1 or 2 character length variable.")

  # Retrieve geometry column
  meta_geom <- dbGetQuery(conn, paste0("SELECT * FROM geometry_columns ",
                                       "WHERE f_table_schema='",table_schema,"' ",
                                       "AND f_table_name = '",table_name,"';"))
  geom <- meta_geom$f_geometry_column
  data_crs <- CRS(paste0("+init=epsg:",meta_geom$srid))

  # Retrieve column names and pkey
  table_type <- dbGetQuery(conn, paste0("SELECT table_type FROM information_schema.tables ",
                                        "WHERE table_schema='",table_schema,"' ",
                                        "AND table_name = '",paste(name,collapse="."),"';"))$table_type
  if (is.null(table_type)) {table_type <- "MATERIALIZED VIEW OR MISSING"}
  if (table_type=="BASE TABLE") {
    meta_columns <- dbGetQuery(conn, paste0("SELECT a.attname, i.indisprimary FROM pg_index i ",
                                            "JOIN pg_attribute a ON a.attrelid = i.indrelid ",
                                            "AND a.attnum = ANY(i.indkey) ",
                                            "WHERE i.indrelid = '",paste(name,collapse="."),"'::regclass;"))
    column_names <- meta_columns$attname[-match(geom,meta_columns$attname)]
    pkey <- meta_columns[meta_columns$indisprimary,"attname"]
  } else {
    column_names <- suppressWarnings(names(dbGetQuery(conn, paste0("SELECT * FROM ",paste(name,collapse=".")," LIMIT 1"))))
    column_names <- column_names[-match(geom,column_names)]
  }
  if (is.null(pkey)) stop("\"pkey\" value was not provided and no primary keys was found.")

  # retrieve data
  data_str <- dbGetQuery(conn, paste0("SELECT ",paste(column_names,collapse=", "),", ST_AsText(",geom,") AS geom_t ",
                                      "FROM ",paste(name,collapse="."),";"))

  # Reshape data
  data_list = list()
  for ( i in 1:nrow(data_str) )  {
    data_list[[i]] <- readWKT(data_str$geom_t[i], data_str[[pkey]][i], data_crs)
  }
  data_sp <- do.call(rbind, data_list)
  rownames(data_str) <- data_str[[pkey]]
  data_sp <- SpatialPolygonsDataFrame(data_sp, data_str[,column_names])

  return(data_sp)

}
