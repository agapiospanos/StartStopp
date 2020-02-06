#' Evaluates the imported patients' data for the selected START criteria at once.
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
#' @param returnData (Boolean) (optional) (default: FALSE) set this to TRUE if you want to have data returned in a data frame format.
#'
#' @importFrom writexl write_xlsx
#' @importFrom stats setNames
#'
#' @return NULL or data.frame. Returns the evaluated data for each criterion if you set the returnData argument to TRUE. By default it does not return anything.
#'
#' @export

STARTselected <- function(selected = NULL, exclude = NULL, path = NULL, excel_out = TRUE, single_excel = TRUE, export_data_path = NULL, suppressNA = TRUE, returnData = FALSE) {

  # check the imported file for its extension and display file choose window in case the path variable is NA
  path<-chk_file(path)

  # choose path for the exported files
  if (excel_out) {
    export_data_path <- choose_export_path(export_data_path)
  }

  final_data_colnames <- c('patient')

  has_build_data_frame <- FALSE

  if (any(any(selected == "A1") | selected == "all") & !any(exclude == "A1")) {
    a1 <- START_A1(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(a1[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'START A1')
    final_data <- cbind(final_data, as.data.frame(a1)$status)
  }

  if (any(any(selected == "A2") | selected == "all") & !any(exclude == "A2")) {
    a2 <- START_A2(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(a2[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'START A2')
    final_data <- cbind(final_data, as.data.frame(a2)$status)
  }

  if (any(any(selected == "A3") | selected == "all") & !any(exclude == "A3")) {
    a3 <- START_A3(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(a3[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'START A3')
    final_data <- cbind(final_data, as.data.frame(a3)$status)
  }

  if (any(any(selected == "A4") | selected == "all") & !any(exclude == "A4")) {
    a4 <- START_A4(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(a4[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'START A4')
    final_data <- cbind(final_data, as.data.frame(a4)$status)
  }

  if (any(any(selected == "A5") | selected == "all") & !any(exclude == "A5")) {
    a5 <- START_A5(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(a5[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'START A5')
    final_data <- cbind(final_data, as.data.frame(a5)$status)
  }

  if (any(any(selected == "A6") | selected == "all") & !any(exclude == "A6")) {
    a6 <- START_A6(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(a6[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'START A6')
    final_data <- cbind(final_data, as.data.frame(a6)$status)
  }

  if (any(any(selected == "A7") | selected == "all") & !any(exclude == "A7")) {
    a7 <- START_A7(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(a7[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'START A7')
    final_data <- cbind(final_data, as.data.frame(a7)$status)
  }

  if (any(any(selected == "A8") | selected == "all") & !any(exclude == "A8")) {
    a8 <- START_A8(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(a8[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'START A8')
    final_data <- cbind(final_data, as.data.frame(a8)$status)
  }

  if (any(any(selected == "B1") | selected == "all") & !any(exclude == "B1")) {
    b1 <- START_B1(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(b1[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'START B1')
    final_data <- cbind(final_data, as.data.frame(b1)$status)
  }

  if (any(any(selected == "B2") | selected == "all") & !any(exclude == "B2")) {
    b2 <- START_B2(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(b2[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'START B2')
    final_data <- cbind(final_data, as.data.frame(b2)$status)
  }

  if (any(any(selected == "C1") | selected == "all") & !any(exclude == "C1")) {
    c1 <- START_C1(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(c1[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'START C1')
    final_data <- cbind(final_data, as.data.frame(c1)$status)
  }

  if (any(any(selected == "C2") | selected == "all") & !any(exclude == "C2")) {
    c2 <- START_C2(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(c2[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'START C2')
    final_data <- cbind(final_data, as.data.frame(c2)$status)
  }

  if (any(any(selected == "C3") | selected == "all") & !any(exclude == "C3")) {
    c3 <- START_C3(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(c3[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'START C3')
    final_data <- cbind(final_data, as.data.frame(c3)$status)
  }

  if (any(any(selected == "C4") | selected == "all") & !any(exclude == "C4")) {
    c4 <- START_C4(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(c4[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'START C4')
    final_data <- cbind(final_data, as.data.frame(c4)$status)
  }

  if (any(any(selected == "C5") | selected == "all") & !any(exclude == "C5")) {
    c5 <- START_C5(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(c5[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'START C5')
    final_data <- cbind(final_data, as.data.frame(c5)$status)
  }

  if (any(any(selected == "C6") | selected == "all") & !any(exclude == "C6")) {
    c6 <- START_C6(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(c6[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'START C6')
    final_data <- cbind(final_data, as.data.frame(c6)$status)
  }

  if (any(any(selected == "D1") | selected == "all") & !any(exclude == "D1")) {
    d1 <- START_D1(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(d1[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'START D1')
    final_data <- cbind(final_data, as.data.frame(d1)$status)
  }

  if (any(any(selected == "D2") | selected == "all") & !any(exclude == "D2")) {
    d2 <- START_D2(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(d2[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'START D2')
    final_data <- cbind(final_data, as.data.frame(d2)$status)
  }

  if (any(any(selected == "E1") | selected == "all") & !any(exclude == "E1")) {
    e1 <- START_E1(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(e1[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'START E1')
    final_data <- cbind(final_data, as.data.frame(e1)$status)
  }

  if (any(any(selected == "E2") | selected == "all") & !any(exclude == "E2")) {
    e2 <- START_E2(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(e2[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'START E2')
    final_data <- cbind(final_data, as.data.frame(e2)$status)
  }

  if (any(any(selected == "E3") | selected == "all") & !any(exclude == "E3")) {
    e3 <- START_E3(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(e3[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'START E3')
    final_data <- cbind(final_data, as.data.frame(e3)$status)
  }

  if (any(any(selected == "E4") | selected == "all") & !any(exclude == "E4")) {
    e4 <- START_E4(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(e4[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'START E4')
    final_data <- cbind(final_data, as.data.frame(e4)$status)
  }

  if (any(any(selected == "E5") | selected == "all") & !any(exclude == "E5")) {
    e5 <- START_E5(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(e5[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'START E5')
    final_data <- cbind(final_data, as.data.frame(e5)$status)
  }

  if (any(any(selected == "E6") | selected == "all") & !any(exclude == "E6")) {
    e6 <- START_E6(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(e6[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'START E6')
    final_data <- cbind(final_data, as.data.frame(e6)$status)
  }

  if (any(any(selected == "E7") | selected == "all") & !any(exclude == "E7")) {
    e7 <- START_E7(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(e7[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'START E7')
    final_data <- cbind(final_data, as.data.frame(e7)$status)
  }

  if (any(any(selected == "F1") | selected == "all") & !any(exclude == "F1")) {
    f1 <- START_F1(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(f1[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'START F1')
    final_data <- cbind(final_data, as.data.frame(f1)$status)
  }

  if (any(any(selected == "G1_2") | selected == "all") & !any(exclude == "G1_2")) {
    g1_2 <- START_G1_2(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(g1_2[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'START G1_2')
    final_data <- cbind(final_data, as.data.frame(g1_2)$status)
  }


  if (any(any(selected == "G3") | selected == "all") & !any(exclude == "G3")) {
    g3 <- START_G3(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(g3[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'START G3')
    final_data <- cbind(final_data, as.data.frame(g3)$status)
  }

  if (any(any(selected == "H1") | selected == "all") & !any(exclude == "H1")) {
    h1 <- START_H1(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(h1[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'START H1')
    final_data <- cbind(final_data, as.data.frame(h1)$status)
  }

  if (any(any(selected == "H2") | selected == "all") & !any(exclude == "H2")) {
    h2 <- START_H2(path = path, excel_out = !single_excel, export_data_path = export_data_path, suppressNA = suppressNA)
    if (!has_build_data_frame) {
      final_data <- data.frame(h2[[1]]$patients)
      has_build_data_frame <- TRUE
    }
    final_data_colnames <- cbind(final_data_colnames, 'START H2')
    final_data <- cbind(final_data, as.data.frame(h2)$status)
  }

  final_data <- setNames(final_data, final_data_colnames)

  if (single_excel) {
    # generate the single excel file containing all criteria
    write_xlsx(final_data, path = paste0( export_data_path, '/START_critetia.xlsx'), col_names = TRUE)
  }

  if (returnData) {
    return (final_data)
  }
}
