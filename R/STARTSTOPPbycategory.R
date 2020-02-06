#' Evaluates the imported patients' data for all the START and STOPP criteria at once and returns the results by category (overuse, misuse, underuse) and not by each criterion (e.g. STOPP B1, STOPP B2, etc.).
#'
#' @author
#' Agapios Panos <panosagapios@gmail.com>
#'
#' @param path (Character) the path that the excel file can be read from.
#' @param STARTselected (Character) (optional) (default: c('all')) a vector of START criteria that you want to include. Example: c("B3", "C1").
#' @param STARTexclude (Character) (optional) (default: NULL) a vector of START criteria that you want to exclude. Example: c("B3", "C1").
#' @param STOPPselected (Character) (optional) (default: c('all')) a vector of STOPP criteria that you want to include Example: c("C6", "D2").
#' @param STOPPexclude (Character) (optional) (default: NULL) a vector of STOPP criteria that you want to exclude. Example: c("C6", "D2").
#' @param excel_out (Boolean) (optional) (default: TRUE) output excel file with the evaluated data.
#' @param single_excel (Boolean) (optional) (default: TRUE) if true outputs only 1 excel file with multiple columns instead of multiple files (one for each criterion)
#' @param export_data_path (Character) (optional) (default: NULL (a popup message to choose dir will be displayed)) the path for excel file output.
#' @param suppressNA (Boolean) (optional) (default: TRUE) set this to FALSE if you want to know for which patients have NAs and for which variable. By default all NAs will be ignored so that the algorithm can distinguish between patients who meet the criterion and those who do not.
#'
#' @export

STARTSTOPPbycategory <- function (path = NULL, STARTselected = "all", STARTexclude = NULL, STOPPselected = "all", STOPPexclude = NULL, excel_out = TRUE, single_excel = TRUE, export_data_path = NULL, suppressNA = TRUE) {

  # check the imported file for its extension and display file choose window in case the path variable is NA
  path <- chk_file(path)

  # choose path for the exported files
  if (excel_out) {
    export_data_path <- choose_export_path(export_data_path)
  }

  overuse <- c( 'STOPP B1', 'STOPP B7', 'STOPP C4', 'STOPP C5', 'STOPP C6', 'STOPP C8', 'STOPP C9', 'STOPP D5', 'STOPP D7', 'STOPP D9', 'STOPP D10', 'STOPP D13', 'STOPP F2', 'STOPP H5', 'STOPP J6', 'STOPP K1', 'STOPP K2', 'STOPP K4' )
  misuse  <- c( 'STOPP B2', 'STOPP B3', 'STOPP B4', 'STOPP B5', 'STOPP B6', 'STOPP B9', 'STOPP B10', 'STOPP B13', 'STOPP C1', 'STOPP C2', 'STOPP C3',
                'STOPP C7', 'STOPP C10', 'STOPP C11', 'STOPP D1', 'STOPP D2', 'STOPP D3', 'STOPP D6', 'STOPP D8', 'STOPP D11', 'STOPP D12', 'STOPP D14',
                'STOPP F1', 'STOPP F3', 'STOPP F4', 'STOPP G1', 'STOPP G2', 'STOPP G3', 'STOPP H1', 'STOPP H2', 'STOPP H3', 'STOPP H4', 'STOPP H6',
                'STOPP H7', 'STOPP H8', 'STOPP H9', 'STOPP I1', 'STOPP I2', 'STOPP J1', 'STOPP J2', 'STOPP J3', 'STOPP J4', 'STOPP J5', 'STOPP K3', 'STOPP L1', 'STOPP L2', 'STOPP M1' )

  underuse <- c( 'START A1', 'START A2', 'START A3', 'START A4', 'START A5', 'START A6', 'START A7', 'START A8', 'START B1', 'START B2', 'START C1', 'START C2', 'START C3', 'START C4',
                 'START C5', 'START C6', 'START D1', 'START D2', 'START E1', 'START E2', 'START E3', 'START E4', 'START E5', 'START E6', 'START E7', 'START F1', 'START G1_2', 'START G3',
                 'START H1', 'START H2' )

  allSTOPPcriteria <- c("B1", "B10", "B11", "B12", "B13", "B2", "B3", "B4", "B5", "B6", "B7", "B9", "C1", "C10", "C11", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "D1", "D10", "D11", "D12", "D13", "D14", "D2", "D3", "D5", "D6", "D7", "D8", "D9", "F1", "F2", "F3", "F4", "G1", "G2", "G3", "H1", "H2", "H3", "H4", "H5", "H6", "H7", "H8", "H9", "I1", "I2", "J1", "J2", "J3", "J4", "J5", "J6", "K1", "K2", "K3", "K4", "L1", "L2", "M1")
  allSTARTcriteria <- c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "B1", "B2", "C1", "C2", "C3", "C4", "C5", "C6", "D1", "D2", "E1", "E2", "E3", "E4", "E5", "E6", "E7", "F1", "G1_2", "G2", "G3", "H1", "H2")

  if (tolower(STOPPselected) != c('all') & tolower(STOPPselected) != 'all') {
    excludedSTOPP <- setdiff(allSTOPPcriteria, STOPPselected)
    STOPPexclude <- unique(c(excludedSTOPP, STOPPexclude))
  } else {
    # apply only the selected
    excludedSTOPP <- setdiff(allSTOPPcriteria, STOPPselected)

  }

  if (tolower(STARTselected) != c('all') & tolower(STARTselected) != 'all') {
    excludedSTART <- setdiff(allSTARTcriteria, STARTselected)
    STARTexclude <- unique(c(excludedSTART, STARTexclude))
  }

  STOPP <- STOPPall(path, STOPPexclude, excel_out, single_excel, export_data_path, suppressNA, TRUE)
  START <- STARTall(path, STARTexclude, excel_out, single_excel, export_data_path, suppressNA, TRUE)

  if (length(STOPPexclude) > 0) {
    for (i in 1:length(STOPPexclude)) {
      # removing excluded stopp criteria
      misuse.location <- grep(paste0('STOPP ', STOPPexclude[i]), misuse, ignore.case = T)
      if (length(misuse.location) > 0) {
        misuse <- misuse[-(misuse.location)]
      }
      overuse.location <- grep(paste0('STOPP ', STOPPexclude[i]), overuse, ignore.case = T)
      if (length(overuse.location) > 0) {
        overuse <- overuse[-(overuse.location)]
      }
    }
  }

  if (length(STARTexclude) > 0) {
    for (i in 1:length(STARTexclude)) {
      # removing excluded start criteria
      underuse.location <- grep(paste0('START ', STARTexclude[i]), underuse, ignore.case = T)
      if (length(underuse.location) > 0) {
        underuse <- underuse[-(underuse.location)]
      }
    }
  }

  overuse.count <- overuse.found <- underuse.count <- underuse.found <- misuse.count <- misuse.found <- c()

  for (i in 1:nrow(STOPP)) {
    overuse.count[i] <- sum(STOPP[unlist(overuse)][i,] == 1)
    misuse.count[i] <- sum(STOPP[unlist(misuse)][i,] == 1)
    underuse.count[i] <- sum(START[unlist(underuse)][i,] == 1)

    if (overuse.count[i] > 0) {
      overuse.found[i] <- 1
    } else {
      overuse.found[i] <- 0
    }

    if (misuse.count[i] > 0) {
      misuse.found[i] <- 1
    } else {
      misuse.found[i] <- 0
    }

    if (underuse.count[i] > 0) {
      underuse.found[i] <- 1
    } else {
      underuse.found[i] <- 0
    }
  }

  output <- data.frame(STOPP[,1], misuse.count, misuse.found, overuse.count, overuse.found, underuse.count, underuse.found)
  names(output)[1] <- 'patient'

  if (excel_out) {
    write_xlsx(output, path = paste0( export_data_path, '/STARTSTOPP_critetia_by_category.xlsx'), col_names = TRUE)
  }

  # return evaluated data
  invisible (list(start = START, stopp = STOPP, sumdata = output)) # instead of return as we do not want to be printed

}
