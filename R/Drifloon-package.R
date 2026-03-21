#' Drifloon: Canadian weather station download and management package
#'
#' The `Drifloon` allows users to pick and choose Canadian weather stations and
#' download weather data from the years 1980 to 2020.
#'
#' @section Function index:
#' - [download_station_month()] Download one month of hourly data for a station.
#'
#' - [download_station_by_name()] Primary user entry for downloading station data
#'   by station name or station ID.
#'
#' - [download_station_province()] Download all stations in a province.
#'
#' - [download_all_station()] Downloads all weather station data available in
#' the metadata file.
#'
#' - [load_metadata()] Load packaged station metadata as a data frame.
#'
#' - [sync_metadata()] Sync metadata from the official station inventory CSV.
#'
#' - [download_metadata()] Copy packaged metadata \code{.rds} file to disk.
#'
#' @docType package
#' @name Drifloon
#' @aliases Drifloon-package
#' @keywords package
"_PACKAGE"
