#' Evaluates the imported patients' data for all the START criteria at once.
#'
#' @author
#' Agapios Panos <panosagapios@gmail.com>
#'
#' @param path (Character) the path that the excel file can be read from.
#' @param exclude (Character) (optional) (default: NULL) a vector of criteria that you want to exclude. Example: c("C6", "D2").
#' @param excel_out (Boolean) (optional) (default: TRUE) output excel file with the evaluated data.
#' @param single_excel (Boolean) (optional) (default: TRUE) if true outputs only 1 excel file with multiple columns instead of multiple files (one for each criterion)
#' @param export_data_path (Character) (optional) (default: NULL (a popup message to choose dir will be displayed)) the path for excel file output.
#' @param suppressNA (Boolean) (optional) (default: TRUE) set this to FALSE if you want to know for which patients have NAs and for which variable. By default all NAs will be ignored so that the algorithm can distinguish between patients who meet the criterion and those who do not.
#' @param returnData (Boolean) (optional) (default: FALSE) set this to TRUE if you want to have data returned in a data frame format.
#'
#' @return NULL or data.frame. Returns the evaluated data for each criterion if you set the returnData argument to TRUE. By default it does not return anything.
#'
#' @export


STARTall <- function(path = NULL, exclude = NULL, excel_out = TRUE, single_excel = TRUE, export_data_path = NULL, suppressNA = TRUE, returnData = FALSE) {

  output <- STARTselected(selected = "all", exclude, path, excel_out, single_excel, export_data_path, suppressNA)

  if (returnData) {
    return(output)
  }
}
