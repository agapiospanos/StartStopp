#' Evaluates the imported patients' data for all the STOPP criteria at once.
#'
#' @author
#' Agapios Panos <panosagapios@gmail.com>
#'
#' @param path (Character) the path that the excel file can be read from.
#' @param export_data_path (Character) (optional) (default: working directory) the path for excel file output.
#'
#' @export


STOPPall <- function(path, export_data_path) {
  STOPP_B1(path = path, export_data_path = export_data_path)
  STOPP_B2(path = path, export_data_path = export_data_path)
  STOPP_B3(path = path, export_data_path = export_data_path)
  STOPP_B4(path = path, export_data_path = export_data_path)
  STOPP_B5(path = path, export_data_path = export_data_path)
  STOPP_B6(path = path, export_data_path = export_data_path)
  STOPP_B7(path = path, export_data_path = export_data_path)
  STOPP_B8(path = path, export_data_path = export_data_path)
  STOPP_B9(path = path, export_data_path = export_data_path)
  STOPP_B10(path = path, export_data_path = export_data_path)
  STOPP_B13(path = path, export_data_path = export_data_path)
  STOPP_C1(path = path, export_data_path = export_data_path)
  STOPP_C2(path = path, export_data_path = export_data_path)
  STOPP_C3(path = path, export_data_path = export_data_path)
  STOPP_C7(path = path, export_data_path = export_data_path)
  STOPP_C10(path = path, export_data_path = export_data_path)
  STOPP_C11(path = path, export_data_path = export_data_path)
  STOPP_D1(path = path, export_data_path = export_data_path)
  STOPP_D2(path = path, export_data_path = export_data_path)
  STOPP_D3(path = path, export_data_path = export_data_path)
  STOPP_D5(path = path, export_data_path = export_data_path)
  STOPP_D6(path = path, export_data_path = export_data_path)
  STOPP_D7(path = path, export_data_path = export_data_path)
  STOPP_D9(path = path, export_data_path = export_data_path)
  STOPP_D10(path = path, export_data_path = export_data_path)
  STOPP_D11(path = path, export_data_path = export_data_path)
  STOPP_D14(path = path, export_data_path = export_data_path)
  STOPP_E1(path = path, export_data_path = export_data_path)
  STOPP_E2(path = path, export_data_path = export_data_path)
  STOPP_E3(path = path, export_data_path = export_data_path)
  STOPP_E4(path = path, export_data_path = export_data_path)
  STOPP_E5(path = path, export_data_path = export_data_path)
  STOPP_E6(path = path, export_data_path = export_data_path)
  STOPP_F1(path = path, export_data_path = export_data_path)
  STOPP_F4(path = path, export_data_path = export_data_path)
  STOPP_G1(path = path, export_data_path = export_data_path)
  STOPP_G3(path = path, export_data_path = export_data_path)
  # STOPP_G4(path = path, export_data_path = export_data_path)
  STOPP_H1(path = path, export_data_path = export_data_path)
  STOPP_H2(path = path, export_data_path = export_data_path)
  STOPP_H3(path = path, export_data_path = export_data_path)
  STOPP_H4(path = path, export_data_path = export_data_path)
  STOPP_H5(path = path, export_data_path = export_data_path)
  STOPP_H6(path = path, export_data_path = export_data_path)
  STOPP_H7(path = path, export_data_path = export_data_path)
  STOPP_H9(path = path, export_data_path = export_data_path)
  STOPP_I1(path = path, export_data_path = export_data_path)
  STOPP_J2(path = path, export_data_path = export_data_path)
  STOPP_J4(path = path, export_data_path = export_data_path)
  STOPP_J6(path = path, export_data_path = export_data_path)
  STOPP_K1(path = path, export_data_path = export_data_path)
  STOPP_K2(path = path, export_data_path = export_data_path)
  STOPP_K3(path = path, export_data_path = export_data_path)
  STOPP_K4(path = path, export_data_path = export_data_path)
  STOPP_M1(path = path, export_data_path = export_data_path)
}
