#' Evaluates the imported patients' data for the STOPP D8 criterion.
#'
#' @param path (Character) (optional) (default: NULL) the path that the excel file can be read from. If not specified a file choose window will be displayed.
#' @param excel_out (Boolean) (optional) (default: TRUE) output excel file with the evaluated data.
#' @param export_data_path (Character) (optional) (default: NULL (a popup message to choose dir will be displayed)) the path for excel file output.
#' @param suppressNA (Boolean) (optional) (default: TRUE) set this to FALSE if you want to know for which patients have NAs and for which variable. By default all NAs will be ignored so that the algorithm can distinguish between patients who meet the criterion and those who do not.
#' @return list of lists of evaluated patient ids categorized in 1) the ids that fulfill the criterion, 2) the ids that do not fulfill the criterion and 3) the ids that has missing data
#'
#' @author
#' Agapios Panos <panosagapios@gmail.com>
#'
#' @importFrom writexl write_xlsx
#' @export


STOPP_D8 <- function(path = NULL, excel_out = TRUE, export_data_path = NULL, suppressNA = TRUE) {

  # check the imported file for its extension and display file choose window in case the path variable is NA
  path<-chk_file(path)

  # choose path for the exported files
  if (excel_out) {
    export_data_path <- choose_export_path(export_data_path)
  }

  missing_data_patients <- list()

  # the variable to keep the final data frame of patients:
  # 0 marks the patient that does not fulfill the criterion,
  # 1 marks the patient that fulfills the criterion and
  # 2 marks the patient with missing data.
  evaluated_patients <- data.frame(patients = character(0), status = numeric(0), missing_variables = character(0))

  # Importing the data
  data <- import_excel_data(path = path, worksheet = 1, var_col = 'med_gen__decod', include_missing = suppressNA, ignore_na = suppressNA )
  data <- import_excel_data(current_data = data, path = path, worksheet = 2, var_col = 'ih_icd10__decod', include_missing = suppressNA, ignore_na = suppressNA )
  data <- import_excel_data(current_data = data, path = path, worksheet = 3, var_col = 'h_icd10__decod', include_missing = suppressNA, ignore_na = TRUE ) # in the third sheet we ignore the n/a as they refer to a patient that visited the hospital but nothing was recorded.
  pdata <- data[[1]]
  missing_data_patients <- data[[2]]

  # iterration over all patients
  for ( i in 1: length(pdata)){
    # checking if the patient id is in the list of missing data
    pid <- names(sapply(pdata[i], names))

    if (is.na(match( pid, names(sapply(missing_data_patients, names))))){ # checking if missing_data_patients contain pid

      # checking if fulfills at least one primary condition
      if ( any(grepl('R06AA02|R06AA04|R06AA52|R06AA54|R06AB02|R06AB03|R06AB04|R06AB52|R06AB54|R06AC04|R06AD01|R06AD02|R06AD52|R06AD08|R06AE03|R06AE53|R06AE04|R06AE05|R06AE55|R06AE06|R06AX02|R06AX15|R06AX17|N05BB01|N05BB51|N07CA02|N07CA52|G04BD04|G04BD07|G04BD08|G04BD10|G04BD11|G04BD02|A03BB01|^N06AA|N04BB01|N06AA09|A03BA01|M03BX01|N04AA01|N04AC01|N04AA02|N03AF01|N05AA01|N06AA04|C01BA03|N06AA16|N06AA12|N05AD01|N06AA02|A07DA03|N06AA10|N05AH03|N05AX13|N06AB05|N05AC01|N02CX01|N05AB04|A03AB05|N05AH04|N05AX08|N05AB06|N05AE04', unlist(pdata[[i]][1]), ignore.case=T)) & # checking at least one condition in the antichol list
           (
             any(grepl('^F05|^F00|^F01|^F02|F03|^G30|G31.0|G31.1|G31.8', unlist(pdata[[i]][2]), ignore.case=T)) | # checking F05* OR F00* OR F01* OR F02* OR F03 OR G30* OR G31.0 OR G31.1 OR G31.8 in the ih_icd10_decod list
             any(grepl('^F05|^F00|^F01|^F02|F03|^G30|G31.0|G31.1|G31.8', unlist(pdata[[i]][3]), ignore.case=T))   # checking F05* OR F00* OR F01* OR F02* OR F03 OR G30* OR G31.0 OR G31.1 OR G31.8 in the h_icd10_decod list
           )
         )
      {
        # inserting the record to the data.frame evaluated_patients with status 1
        evaluated_patients <- rbind(evaluated_patients, data.frame(patients = pid, status = 1, missing_variables = ''))
      } else {

        # inserting the record to the data.frame evaluated_patients with status 0
        evaluated_patients <- rbind(evaluated_patients, data.frame(patients = pid, status = 0, missing_variables = ''))
      }
    } else { # patient has missing data

      # inserting the record to the data.frame evaluated_patients with status 2
      evaluated_patients <- rbind(evaluated_patients, data.frame(patients = pid, status = 2, missing_variables = paste(missing_data_patients[[pid]], collapse = ', ')))
    }
  }

  # storing the results
  fulfill_count <- length(which(evaluated_patients$status == 1))
  total_count <- fulfill_count + length(which(evaluated_patients$status == 0))
  missing_count <- length(which(evaluated_patients$status == 2))

  # printing results to the console
  if (suppressNA) {
    cat('STOPP D8: ', fulfill_count, 'patients out of', total_count + missing_count, 'patients meet the criterion.\n')
  } else {
    cat('STOPP D8: ', fulfill_count, 'patients out of', total_count, 'patients meet the criterion.', missing_count, 'patients have missing data. \n')
  }

  if (excel_out) {
    # export the evaluated list of patients to excel file
    write_xlsx(evaluated_patients, path = paste0( export_data_path, '/STOPP-D8.xlsx'), col_names = TRUE)
  }

  invisible (list(evaluated_patients)) # instead of return as we do not want to be printed
}
