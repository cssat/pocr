#' Dummy function for kml help
#' 
#' Reading and Writing KML files:
#' \code{readOGR(dsn = "C:/Users/gregorp.NEBULA2/Downloads/1433", layer = "wa_counties", encoding="KML")}
#' Notice, for reading: no file extension, layer taken from inside the \code{<name>} tags in the KML file,
#' encoding specified.
#' \code{writeOGR(obj = obName, dsn = "directory/file.kml", driver = "KML", layer = "layerName")}
#' Writing KML is basically the same.
#' 
#' Reading SHP files
#' \code{readOGR(dsn = "C:/Users/gregorp.NEBULA2/Downloads/1433", layer = "wa_counties")}
#' \code{dsn} points to directory, layer is name of particular shape file (sans file extension).
#' @export
kml <- function() {
    print("See ?kml for instructions on reading and writing kml files.")
    NULL
}