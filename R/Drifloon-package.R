#' Drifloon: Canadian Weather Data Download Toolkit
#'
#' @section Main Functions:
#' \itemize{
#'   \item \code{\link{download_station_month()}}: Download one month of hourly data for a station.
#'   \item \code{\link{download_station_by_name()}}: Primary user entry for station downloads.
#'   \item \code{\link{download_station_province()}}: Download all stations in a province.
#'   \item \code{\link{download_all_station()}}: Download all stations in metadata.
#'   \item \code{\link{load_metadata()}}: Load packaged station metadata.
#'   \item \code{\link{sync_metadata()}}: Sync metadata from official station inventory.
#'   \item \code{\link{download_metadata()}}: Copy packaged metadata \code{.rds} file to disk.
#' }
#'
#' @author
#' Maintainer: Joey Frost-Xenopoulos
#' \email{joeyfrostxenopoulos@trentu.ca}
#'
#' @seealso
#' Useful links:
#' \itemize{
#'   \item Environment and Climate Change Canada climate data portal:
#'   \url{https://climate.weather.gc.ca/}
#' }
#'
#' @docType package
#' @name Drifloon
#' @keywords package
"_PACKAGE"
