#' Drifloon: Canadian Weather Data Download Toolkit
#'
#' @description
#' Drifloon helps you discover Canadian climate stations and download hourly
#' weather data from Environment and Climate Change Canada.
#'
#' @details
#' To learn more about Drifloon, start with:
#' \code{?download_station_by_name}, \code{?download_station_province},
#' and \code{?sync_metadata}.
#'
#' Common workflow:
#' \enumerate{
#'   \item Load metadata with \code{load_metadata()}.
#'   \item (Optional) refresh packaged metadata with \code{sync_metadata()}.
#'   \item Download station data by name, province, or month.
#' }
#'
#' @section Main Functions:
#' \itemize{
#'   \item \code{download_station_month()}: Download one month of hourly data for a station.
#'   \item \code{download_station_by_name()}: Primary user entry for station downloads.
#'   \item \code{download_station_province()}: Download all stations in a province.
#'   \item \code{download_all_station()}: Download all stations in metadata.
#'   \item \code{load_metadata()}: Load packaged station metadata.
#'   \item \code{sync_metadata()}: Sync metadata from official station inventory.
#'   \item \code{download_metadata()}: Copy packaged metadata \code{.rds} file to disk.
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
