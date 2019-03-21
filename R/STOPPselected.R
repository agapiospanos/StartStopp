#' Evaluates the imported patients' data for the selected STOPP criteria at once.
#'
#' @author
#' Agapios Panos <panosagapios@gmail.com>
#'
#' @param selected (Character vector) the selected criteria to export given in the form of c("A1", "E2")
#' @param exclude (Character) (optional) (default: NULL) a vector of criteria that you want to exclude. Example: c("C6", "D2").
#' @param path (Character) the path that the excel file can be read from.
#' @param excel_out (Boolean) (optional) (default: TRUE) output excel file with the evaluated data.
#' @param single_excel (Boolean) (optional) (default: TRUE) if true outputs only 1 excel file with multiple columns instead of multiple files (one for each criterion)
#' @param export_data_path (Character) (optional) (default: NULL (a popup message to choose dir will be displayed)) the path for excel file output.
#' @param suppressNA (Boolean) (optional) (default: TRUE) set this to FALSE if you want to know for which patients have NAs and for which variable. By default all NAs will be ignored so that the algorithm can distinguish between patients who meet the criterion and those who do not.
#'
#' @importFrom writexl write_xlsx
#' @importFrom stats setNames
#'
#' @export

STOPPselected <- function(selected = NULL, exclude = NULL, path = NULL, excel_out = TRUE, single_excel = TRUE, export_data_path = NULL, suppressNA = TRUE) {

  # check the imported file for its extension and display file choose window in case the path variable is NA
  path<-chk_file(path)

  # choose path for the exported files
  if (excel_out) {
    export_data_path <- choose_export_path(export_data_path)
  }

  final_data_colnames <- c('patient')

  has_build_data_frame <- FALSE

  if (any(any(selected == "B1") | selected == "all") & !any(exclude == "B1")) {
    b1 <- STOPP_B1(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(b1[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP B1')
    final_data <- cbind(final_data, as.data.frame(b1)$status)
  }

  if (any(any(selected == "B2") | selected == "all") & !any(exclude == "B2")) {
    b2 <- STOPP_B2(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(b2[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP B2')
    final_data <- cbind(final_data, as.data.frame(b2)$status)
  }

  if (any(any(selected == "B3") | selected == "all") & !any(exclude == "B3")) {
    b3 <- STOPP_B3(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(b3[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP B3')
    final_data <- cbind(final_data, as.data.frame(b3)$status)
  }

  if (any(any(selected == "B4") | selected == "all") & !any(exclude == "B4")) {
    b4 <- STOPP_B4(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(b4[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP B4')
    final_data <- cbind(final_data, as.data.frame(b4)$status)
  }

  if (any(any(selected == "B5") | selected == "all") & !any(exclude == "B5")) {
    b5 <- STOPP_B5(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(b5[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP B5')
    final_data <- cbind(final_data, as.data.frame(b5)$status)
  }

  if (any(any(selected == "B6") | selected == "all") & !any(exclude == "B6")) {
    b6 <- STOPP_B6(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(b6[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP B6')
    final_data <- cbind(final_data, as.data.frame(b6)$status)
  }

  if (any(any(selected == "B7") | selected == "all") & !any(exclude == "B7")) {
    b7 <- STOPP_B7(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(b7[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP B7')
    final_data <- cbind(final_data, as.data.frame(b7)$status)
  }

  if (any(any(selected == "B8") | selected == "all") & !any(exclude == "B8")) {
    b8 <- STOPP_B8(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(b8[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP B8')
    final_data <- cbind(final_data, as.data.frame(b8)$status)
  }

  if (any(any(selected == "B9") | selected == "all") & !any(exclude == "B9")) {
    b9 <- STOPP_B9(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(b9[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP B9')
    final_data <- cbind(final_data, as.data.frame(b9)$status)
  }

  if (any(any(selected == "B10") | selected == "all") & !any(exclude == "B10")) {
    b10 <- STOPP_B10(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(b10[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP B10')
    final_data <- cbind(final_data, as.data.frame(b10)$status)
  }

  if (any(any(selected == "B11") | selected == "all") & !any(exclude == "B11")) {
    b11 <- STOPP_B11(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(b11[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP B11')
    final_data <- cbind(final_data, as.data.frame(b11)$status)
  }

  if (any(any(selected == "B12") | selected == "all") & !any(exclude == "B12")) {
    b12 <- STOPP_B12(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(b12[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP B12')
    final_data <- cbind(final_data, as.data.frame(b12)$status)
  }

  if (any(any(selected == "B13") | selected == "all") & !any(exclude == "B13")) {
    b13 <- STOPP_B13(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(b13[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP B13')
    final_data <- cbind(final_data, as.data.frame(b13)$status)
  }

  if (any(any(selected == "C1") | selected == "all") & !any(exclude == "C1")) {
    c1 <- STOPP_C1(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(c1[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP C1')
    final_data <- cbind(final_data, as.data.frame(c1)$status)
  }

  if (any(any(selected == "C2") | selected == "all") & !any(exclude == "C2")) {
    c2 <- STOPP_C2(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(c2[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP C2')
    final_data <- cbind(final_data, as.data.frame(c2)$status)
  }

  if (any(any(selected == "C3") | selected == "all") & !any(exclude == "C3")) {
    c3 <- STOPP_C3(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(c3[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP C3')
    final_data <- cbind(final_data, as.data.frame(c3)$status)
  }

  if (any(any(selected == "C4") | selected == "all") & !any(exclude == "C4")) {
    c4 <- STOPP_C4(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(c4[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP C4')
    final_data <- cbind(final_data, as.data.frame(c4)$status)
  }

  if (any(any(selected == "C5") | selected == "all") & !any(exclude == "C5")) {
    c5 <- STOPP_C5(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(c5[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP C5')
    final_data <- cbind(final_data, as.data.frame(c5)$status)
  }

  if (any(any(selected == "C6") | selected == "all") & !any(exclude == "C6")) {
    c6 <- STOPP_C6(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(c6[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP C6')
    final_data <- cbind(final_data, as.data.frame(c6)$status)
  }

  if (any(any(selected == "C7") | selected == "all") & !any(exclude == "C7")) {
    c7 <- STOPP_C7(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(c7[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP C7')
    final_data <- cbind(final_data, as.data.frame(c7)$status)
  }

  if (any(any(selected == "C8") | selected == "all") & !any(exclude == "C8")) {
    c8 <- STOPP_C8(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(c8[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP C8')
    final_data <- cbind(final_data, as.data.frame(c8)$status)
  }

  if (any(any(selected == "C9") | selected == "all") & !any(exclude == "C9")) {
    c9 <- STOPP_C9(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(c9[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP C9')
    final_data <- cbind(final_data, as.data.frame(c9)$status)
  }

  if (any(any(selected == "C10") | selected == "all") & !any(exclude == "C10")) {
    c10 <- STOPP_C10(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(c10[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP C10')
    final_data <- cbind(final_data, as.data.frame(c10)$status)
  }

  if (any(any(selected == "C11") | selected == "all") & !any(exclude == "C11")) {
    c11 <- STOPP_C11(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(c11[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP C11')
    final_data <- cbind(final_data, as.data.frame(c11)$status)
  }

  if (any(any(selected == "D1") | selected == "all") & !any(exclude == "D1")) {
    d1 <- STOPP_D1(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(d1[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP D1')
    final_data <- cbind(final_data, as.data.frame(d1)$status)
  }

  if (any(any(selected == "D2") | selected == "all") & !any(exclude == "D2")) {
    d2 <- STOPP_D2(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(d2[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP D2')
    final_data <- cbind(final_data, as.data.frame(d2)$status)
  }

  if (any(any(selected == "D3") | selected == "all") & !any(exclude == "D3")) {
    d3 <- STOPP_D3(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(d3[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP D3')
    final_data <- cbind(final_data, as.data.frame(d3)$status)
  }

  if (any(any(selected == "D4") | selected == "all") & !any(exclude == "D4")) {
    d4 <- STOPP_D4(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(d4[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP D4')
    final_data <- cbind(final_data, as.data.frame(d4)$status)
  }

  if (any(any(selected == "D5") | selected == "all") & !any(exclude == "D5")) {
    d5 <- STOPP_D5(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(d5[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP D5')
    final_data <- cbind(final_data, as.data.frame(d5)$status)
  }

  if (any(any(selected == "D6") | selected == "all") & !any(exclude == "D6")) {
    d6 <- STOPP_D6(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(d6[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP D6')
    final_data <- cbind(final_data, as.data.frame(d6)$status)
  }

  if (any(any(selected == "D7") | selected == "all") & !any(exclude == "D7")) {
    d7 <- STOPP_D7(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(d7[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP D7')
    final_data <- cbind(final_data, as.data.frame(d7)$status)
  }

  if (any(any(selected == "D8") | selected == "all") & !any(exclude == "D8")) {
    d8 <- STOPP_D8(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(d8[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP D8')
    final_data <- cbind(final_data, as.data.frame(d8)$status)
  }

  if (any(any(selected == "D9") | selected == "all") & !any(exclude == "D9")) {
    d9 <- STOPP_D9(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(d9[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP D9')
    final_data <- cbind(final_data, as.data.frame(d9)$status)
  }

  if (any(any(selected == "D10") | selected == "all") & !any(exclude == "D10")) {
    d10 <- STOPP_D10(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(d10[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP D10')
    final_data <- cbind(final_data, as.data.frame(d10)$status)
  }

  if (any(any(selected == "D11") | selected == "all") & !any(exclude == "D11")) {
    d11 <- STOPP_D11(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(d11[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP D11')
    final_data <- cbind(final_data, as.data.frame(d11)$status)
  }

  if (any(any(selected == "D12") | selected == "all") & !any(exclude == "D12")) {
    d12 <- STOPP_D12(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(d12[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP D12')
    final_data <- cbind(final_data, as.data.frame(d12)$status)
  }

  if (any(any(selected == "D13") | selected == "all") & !any(exclude == "D13")) {
    d13 <- STOPP_D13(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(d13[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP D13')
    final_data <- cbind(final_data, as.data.frame(d13)$status)
  }

  if (any(any(selected == "D14") | selected == "all") & !any(exclude == "D14")) {
    d14 <- STOPP_D14(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(d14[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP D14')
    final_data <- cbind(final_data, as.data.frame(d14)$status)
  }

  if (any(any(selected == "E1") | selected == "all") & !any(exclude == "E1")) {
    e1 <- STOPP_E1(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(e1[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP E1')
    final_data <- cbind(final_data, as.data.frame(e1)$status)
  }

  if (any(any(selected == "E2") | selected == "all") & !any(exclude == "E2")) {
    e2 <- STOPP_E2(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(e2[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP E2')
    final_data <- cbind(final_data, as.data.frame(e2)$status)
  }

  if (any(any(selected == "E3") | selected == "all") & !any(exclude == "E3")) {
    e3 <- STOPP_E3(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(e3[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP E3')
    final_data <- cbind(final_data, as.data.frame(e3)$status)
  }

  if (any(any(selected == "E4") | selected == "all") & !any(exclude == "E4")) {
    e4 <- STOPP_E4(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(e4[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP E4')
    final_data <- cbind(final_data, as.data.frame(e4)$status)
  }

  if (any(any(selected == "E5") | selected == "all") & !any(exclude == "E5")) {
    e5 <- STOPP_E5(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(e5[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP E5')
    final_data <- cbind(final_data, as.data.frame(e5)$status)
  }

  if (any(any(selected == "E6") | selected == "all") & !any(exclude == "E6")) {
    e6 <- STOPP_E6(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(e6[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP E6')
    final_data <- cbind(final_data, as.data.frame(e6)$status)
  }

  if (any(any(selected == "F1") | selected == "all") & !any(exclude == "F1")) {
    f1 <- STOPP_F1(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(f1[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP F1')
    final_data <- cbind(final_data, as.data.frame(f1)$status)
  }

  if (any(any(selected == "F2") | selected == "all") & !any(exclude == "F2")) {
    f2 <- STOPP_F2(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(f2[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP F2')
    final_data <- cbind(final_data, as.data.frame(f2)$status)
  }

  if (any(any(selected == "F3") | selected == "all") & !any(exclude == "F3")) {
    f3 <- STOPP_F3(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(f3[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP F3')
    final_data <- cbind(final_data, as.data.frame(f3)$status)
  }

  if (any(any(selected == "F4") | selected == "all") & !any(exclude == "F4")) {
    f4 <- STOPP_F4(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(f4[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP F4')
    final_data <- cbind(final_data, as.data.frame(f4)$status)
  }

  if (any(any(selected == "G1") | selected == "all") & !any(exclude == "G1")) {
    g1 <- STOPP_G1(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(g1[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP G1')
    final_data <- cbind(final_data, as.data.frame(g1)$status)
  }

  if (any(any(selected == "G2") | selected == "all") & !any(exclude == "G2")) {
    g2 <- STOPP_G2(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(g2[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP G2')
    final_data <- cbind(final_data, as.data.frame(g2)$status)
  }

  if (any(any(selected == "G3") | selected == "all") & !any(exclude == "G3")) {
    g3 <- STOPP_G3(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(g3[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP G3')
    final_data <- cbind(final_data, as.data.frame(g3)$status)
  }

  if (any(any(selected == "G4") | selected == "all") & !any(exclude == "G4")) {
    g4 <- STOPP_G4(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(g4[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP G4')
    final_data <- cbind(final_data, as.data.frame(g4)$status)
  }

  if (any(any(selected == "H1") | selected == "all") & !any(exclude == "H1")) {
    h1 <- STOPP_H1(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(h1[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP H1')
    final_data <- cbind(final_data, as.data.frame(h1)$status)
  }

  if (any(any(selected == "H2") | selected == "all") & !any(exclude == "H2")) {
    h2 <- STOPP_H2(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(h2[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP H2')
    final_data <- cbind(final_data, as.data.frame(h2)$status)
  }

  if (any(any(selected == "H3") | selected == "all") & !any(exclude == "H3")) {
    h3 <- STOPP_H3(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(h3[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP H3')
    final_data <- cbind(final_data, as.data.frame(h3)$status)
  }

  if (any(any(selected == "H4") | selected == "all") & !any(exclude == "H4")) {
    h4 <- STOPP_H4(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(h4[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP H4')
    final_data <- cbind(final_data, as.data.frame(h4)$status)
  }

  if (any(any(selected == "H5") | selected == "all") & !any(exclude == "H5")) {
    h5 <- STOPP_H5(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(h5[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP H5')
    final_data <- cbind(final_data, as.data.frame(h5)$status)
  }

  if (any(any(selected == "H6") | selected == "all") & !any(exclude == "H6")) {
    h6 <- STOPP_H6(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(h6[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP H6')
    final_data <- cbind(final_data, as.data.frame(h6)$status)
  }

  if (any(any(selected == "H7") | selected == "all") & !any(exclude == "H7")) {
    h7 <- STOPP_H7(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(h7[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP H7')
    final_data <- cbind(final_data, as.data.frame(h7)$status)
  }

  if (any(any(selected == "H8") | selected == "all") & !any(exclude == "H8")) {
    h8 <- STOPP_H8(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(h8[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP H8')
    final_data <- cbind(final_data, as.data.frame(h8)$status)
  }

  if (any(any(selected == "H9") | selected == "all") & !any(exclude == "H9")) {
    h9 <- STOPP_H9(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(h9[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP H9')
    final_data <- cbind(final_data, as.data.frame(h9)$status)
  }

  if (any(any(selected == "I1") | selected == "all") & !any(exclude == "I1")) {
    i1 <- STOPP_I1(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(i1[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP I1')
    final_data <- cbind(final_data, as.data.frame(i1)$status)
  }

  if (any(any(selected == "I2") | selected == "all") & !any(exclude == "I2")) {
    i2 <- STOPP_I2(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(i2[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP I2')
    final_data <- cbind(final_data, as.data.frame(i2)$status)
  }

  if (any(any(selected == "J1") | selected == "all") & !any(exclude == "J1")) {
    j1 <- STOPP_J1(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(j1[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP J1')
    final_data <- cbind(final_data, as.data.frame(j1)$status)
  }

  if (any(any(selected == "J2") | selected == "all") & !any(exclude == "J2")) {
    j2 <- STOPP_J2(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(j2[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP J2')
    final_data <- cbind(final_data, as.data.frame(j2)$status)
  }

  if (any(any(selected == "J3") | selected == "all") & !any(exclude == "J3")) {
    j3 <- STOPP_J3(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(j3[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP J3')
    final_data <- cbind(final_data, as.data.frame(j3)$status)
  }

  if (any(any(selected == "J4") | selected == "all") & !any(exclude == "J4")) {
    j4 <- STOPP_J4(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(j4[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP J4')
    final_data <- cbind(final_data, as.data.frame(j4)$status)
  }

  if (any(any(selected == "J5") | selected == "all") & !any(exclude == "J5")) {
    j5 <- STOPP_J5(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(j5[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP J5')
    final_data <- cbind(final_data, as.data.frame(j5)$status)
  }

  if (any(any(selected == "J6") | selected == "all") & !any(exclude == "J6")) {
    j6 <- STOPP_J6(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(j6[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP J6')
    final_data <- cbind(final_data, as.data.frame(j6)$status)
  }

  if (any(any(selected == "K1") | selected == "all") & !any(exclude == "K1")) {
    k1 <- STOPP_K1(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(k1[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP K1')
    final_data <- cbind(final_data, as.data.frame(k1)$status)
  }

  if (any(any(selected == "K2") | selected == "all") & !any(exclude == "K2")) {
    k2 <- STOPP_K2(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(k2[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP K2')
    final_data <- cbind(final_data, as.data.frame(k2)$status)
  }

  if (any(any(selected == "K3") | selected == "all") & !any(exclude == "K3")) {
    k3 <- STOPP_K3(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(k3[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP K3')
    final_data <- cbind(final_data, as.data.frame(k3)$status)
  }

  if (any(any(selected == "K4") | selected == "all") & !any(exclude == "K4")) {
    k4 <- STOPP_K4(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(k4[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP K4')
    final_data <- cbind(final_data, as.data.frame(k4)$status)
  }

  if (any(any(selected == "L1") | selected == "all") & !any(exclude == "L1")) {
    l1 <- STOPP_L1(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(l1[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP L1')
    final_data <- cbind(final_data, as.data.frame(l1)$status)
  }

  if (any(any(selected == "L2") | selected == "all") & !any(exclude == "L2")) {
    l2 <- STOPP_L2(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(l2[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP L2')
    final_data <- cbind(final_data, as.data.frame(l2)$status)
  }

  if (any(any(selected == "M1") | selected == "all") & !any(exclude == "M1")) {
    m1 <- STOPP_M1(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(m1[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'STOPP M1')
    final_data <- cbind(final_data, as.data.frame(m1)$status)
  }

  if (single_excel){
    # generate the single excel file containing all criteria
    final_data <- setNames(final_data, final_data_colnames)
    write_xlsx(final_data, path = paste0( export_data_path, '/STOPP_critetia.xlsx'), col_names = TRUE)
  }
}
