#' Helper function for choosing the export path for the excel files
#'
#' @param export_data_path (Character) (optional) (default: NULL (a popup message to choose dir will be displayed)) the path for excel file output.
#' @param suppressNA (Boolean) (optional) (default: TRUE) set this to FALSE if you want to know for which patients have NAs and for which variable. By default all NAs will be ignored so that the algorithm can distinguish between patients who meet the criterion and those who do not.
#' @return a valid export path for the excel files.
#'
#' @author
#' Agapios Panos <panosagapios@gmail.com>
#'
#' @importFrom easycsv choose_dir

choose_export_path <- function(export_data_path = NULL, suppressNA = TRUE){
  # checking if the user specified an export.path argument. If not, a prompt window will be displayed to ask for a path.
  if (is.null(export_data_path)) {
    cat('Please choose a folder to export the Word Document...', '\n')
    export_data_path <- choose_dir()
  }

  # checking if the export_data_path is specified
  if (length(export_data_path) == 0) {
    cat('You did not specify an export path. The working directory will be used for the export: ', getwd(), '\n')
    export_data_path <- getwd()
  }

  # checking if the specified directory is valid
  if (!dir.exists(export_data_path)){
    stop('You did not specify a valid export_data_path argument')
  } else {
    cat('You selected the folder ', export_data_path, ' for the export. \n')
  }

  return(export_data_path)
}
