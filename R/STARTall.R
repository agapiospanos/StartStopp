#' Evaluates the imported patients' data for the all the START criteria at once.
#'
#' @author
#' Agapios Panos <panosagapios@gmail.com>
#'
#' @param path (Character) the path that the excel file can be read from.
#' @param export_data_path (Character) (optional) (default: working directory) the path for excel file output.
#'
#' @export


STARTall <- function(path, export_data_path) {
  START_A1(path = path, export_data_path = export_data_path)
  START_E2(path = path, export_data_path = export_data_path)
}
