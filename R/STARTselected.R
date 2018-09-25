#' Evaluates the imported patients' data for the selected START criteria at once.
#'
#' @author
#' Agapios Panos <panosagapios@gmail.com>
#'
#' @param selected (Character vector) the selected criteria to export given in the form of c("A1", "E2")
#' @param path (Character) the path that the excel file can be read from.
#' @param export_data_path (Character) (optional) (default: working directory) the path for excel file output.
#'
#' @export

STARTselected <- function(selected, path, export_data_path) {
  if(any(selected == "A1")) START_A1(path = path, export_data_path = export_data_path)
  if(any(selected == "E2")) START_E2(path = path, export_data_path = export_data_path)
}
