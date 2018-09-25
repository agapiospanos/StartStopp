#' Evaluates the imported patients' data for the selected STOPP criteria at once.
#'
#' @author
#' Agapios Panos <panosagapios@gmail.com>
#'
#' @param selected (Character vector) the selected criteria to export given in the form of c("B1", "D5", "H3")
#' @param path (Character) the path that the excel file can be read from.
#' @param export_data_path (Character) (optional) (default: working directory) the path for excel file output.
#'
#' @export

STOPPselected <- function(selected, path, export_data_path) {

  if(any(selected == "B1")) STOPP_B1(path = path, export_data_path = export_data_path)

  if(any(selected == "B2")) STOPP_B2(path = path, export_data_path = export_data_path)

  if(any(selected == "B3")) STOPP_B3(path = path, export_data_path = export_data_path)

  if(any(selected == "B4")) STOPP_B4(path = path, export_data_path = export_data_path)

  if(any(selected == "B5")) STOPP_B5(path = path, export_data_path = export_data_path)

  if(any(selected == "B6")) STOPP_B6(path = path, export_data_path = export_data_path)

  if(any(selected == "B7")) STOPP_B7(path = path, export_data_path = export_data_path)

  if(any(selected == "B8")) STOPP_B8(path = path, export_data_path = export_data_path)

  if(any(selected == "B9")) STOPP_B9(path = path, export_data_path = export_data_path)

  if(any(selected == "B10")) STOPP_B10(path = path, export_data_path = export_data_path)

  if(any(selected == "B13")) STOPP_B13(path = path, export_data_path = export_data_path)

  if(any(selected == "C1")) STOPP_C1(path = path, export_data_path = export_data_path)

  if(any(selected == "C2")) STOPP_C2(path = path, export_data_path = export_data_path)

  if(any(selected == "C3")) STOPP_C3(path = path, export_data_path = export_data_path)

  if(any(selected == "C7")) STOPP_C7(path = path, export_data_path = export_data_path)

  if(any(selected == "C10")) STOPP_C10(path = path, export_data_path = export_data_path)

  if(any(selected == "C11")) STOPP_C11(path = path, export_data_path = export_data_path)

  if(any(selected == "D1")) STOPP_D1(path = path, export_data_path = export_data_path)

  if(any(selected == "D2")) STOPP_D2(path = path, export_data_path = export_data_path)

  if(any(selected == "D3")) STOPP_D3(path = path, export_data_path = export_data_path)

  if(any(selected == "D5")) STOPP_D5(path = path, export_data_path = export_data_path)

  if(any(selected == "D6")) STOPP_D6(path = path, export_data_path = export_data_path)

  if(any(selected == "D7")) STOPP_D7(path = path, export_data_path = export_data_path)

  if(any(selected == "D9")) STOPP_D9(path = path, export_data_path = export_data_path)

  if(any(selected == "D10")) STOPP_D10(path = path, export_data_path = export_data_path)

  if(any(selected == "D11")) STOPP_D11(path = path, export_data_path = export_data_path)

  if(any(selected == "D14")) STOPP_D14(path = path, export_data_path = export_data_path)

  if(any(selected == "E1")) STOPP_E1(path = path, export_data_path = export_data_path)

  if(any(selected == "E2")) STOPP_E2(path = path, export_data_path = export_data_path)

  if(any(selected == "E3")) STOPP_E3(path = path, export_data_path = export_data_path)

  if(any(selected == "E4")) STOPP_E4(path = path, export_data_path = export_data_path)

  if(any(selected == "E5")) STOPP_E5(path = path, export_data_path = export_data_path)

  if(any(selected == "E6")) STOPP_E6(path = path, export_data_path = export_data_path)

  if(any(selected == "F1")) STOPP_F1(path = path, export_data_path = export_data_path)

  if(any(selected == "F4")) STOPP_F4(path = path, export_data_path = export_data_path)

  if(any(selected == "G1")) STOPP_G1(path = path, export_data_path = export_data_path)

  if(any(selected == "G3")) STOPP_G3(path = path, export_data_path = export_data_path)

  if(any(selected == "G4")) STOPP_G4(path = path, export_data_path = export_data_path)

  if(any(selected == "H1")) STOPP_H1(path = path, export_data_path = export_data_path)

  if(any(selected == "H2")) STOPP_H2(path = path, export_data_path = export_data_path)

  if(any(selected == "H3")) STOPP_H3(path = path, export_data_path = export_data_path)

  if(any(selected == "H4")) STOPP_H4(path = path, export_data_path = export_data_path)

  if(any(selected == "H5")) STOPP_H5(path = path, export_data_path = export_data_path)

  if(any(selected == "H6")) STOPP_H6(path = path, export_data_path = export_data_path)

  if(any(selected == "H7")) STOPP_H7(path = path, export_data_path = export_data_path)

  if(any(selected == "H9")) STOPP_H9(path = path, export_data_path = export_data_path)

  if(any(selected == "I1")) STOPP_I1(path = path, export_data_path = export_data_path)

  if(any(selected == "J2")) STOPP_J2(path = path, export_data_path = export_data_path)

  if(any(selected == "J4")) STOPP_J4(path = path, export_data_path = export_data_path)

  if(any(selected == "J6")) STOPP_J6(path = path, export_data_path = export_data_path)

  if(any(selected == "K1")) STOPP_K1(path = path, export_data_path = export_data_path)

  if(any(selected == "K2")) STOPP_K2(path = path, export_data_path = export_data_path)

  if(any(selected == "K3")) STOPP_K3(path = path, export_data_path = export_data_path)

  if(any(selected == "K4")) STOPP_K4(path = path, export_data_path = export_data_path)

  if(any(selected == "M1")) STOPP_M1(path = path, export_data_path = export_data_path)
}
