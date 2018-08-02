#' Evaluates the imported patients' data for the STOPP C2 criterion.
#'
#' @param path (Character) the path that the excel file can be read from.
#' @param excel_out (Boolean) (optional) (default: TRUE) output excel file with the evaluated data.
#' @param export_data_path (Character) (optional) (default: working directory) the path for excel file output.
#' @return list of lists of evaluated patient ids categorized in 1) the ids that fulfill the criterion, 2) the ids that do not fulfill the criterion and 3) the ids that has missing data
#'
#' @author
#' Agapios Panos <panosagapios@gmail.com>
#'
#' @importFrom writexl write_xlsx
#' @export


STOPP_C2 <- function(path, excel_out = TRUE, export_data_path=getwd()) {

  missing_data_patients <- list()

  # the variable to keep the final data frame of patients:
  # 0 marks the patient that does not fulfill the criterion,
  # 1 marks the patient that fulfills the criterion and
  # 2 marks the patient with missing data.
  evaluated_patients <- data.frame(patients = character(0), status = numeric(0), missing_variables = character(0))

  # Importing the data
  data <- import_excel_data(path = path, worksheet = 1, var_col = 'med_gen__decod')
  data <- import_excel_data(current_data = data, path = path, worksheet = 2, var_col = 'ih_icd10__decod')
  pdata <- data[[1]]
  missing_data_patients <- data[[2]]

  # iterration over all patients
  for ( i in 1: length(pdata)){
    # checking if the patient id is in the list of missing data
    pid <- names(sapply(pdata[i], names))

    if (is.na(match( pid, names(sapply(missing_data_patients, names))))){
      # checking for the unless condition
      if ( any(grepl('^A02BC', unlist(pdata[[i]][1]), ignore.case=T)) # checking unless condition A02BC** in the med_gen__decod list
      ){
        # inserting the record to the data.frame evaluated_patients
        evaluated_patients <- rbind(evaluated_patients, data.frame(patients = pid, status = 0, missing_variables = ''))
      } else {
        #checking if fulfills at least one primary condition AND at least one secondary condition
        if ( any(grepl('B01AC06|B01AC08|N02BA01|N02BA51|M01BA03|C10BX01|C10BX02|C10BX04|C10BX05|C10BX06|C10BX08|N02BA15', unlist(pdata[[i]][1]), ignore.case=T)) & # checking primary condition B01AC06 OR B01AC08 OR N02BA01 OR N02BA51 OR M01BA03 OR C10BX01 OR C10BX02 OR C10BX04 OR C10BX05 OR C10BX06 OR C10BX08 OR N02BA15 in the med_gen_decod list
             any(grepl('K22.1|K25|K26|K27|K28', unlist(pdata[[i]][2]), ignore.case=T)) # checking secondary condition K22.1 OR K25 OR K26 OR K27 OR K28 in the ih_icd_10_decod list
        ) {
          # inserting the record to the data.frame evaluated_patients
          evaluated_patients <- rbind(evaluated_patients, data.frame(patients = pid, status = 1, missing_variables = ''))
        } else {
          # inserting the record to the data.frame evaluated_patients
          evaluated_patients <- rbind(evaluated_patients, data.frame(patients = pid, status = 0, missing_variables = ''))
        }
      }
    } else { # patient has missing data
     # inserting the record to the data.frame evaluated_patients
      evaluated_patients <- rbind(evaluated_patients, data.frame(patients = pid, status = 2, missing_variables = paste(missing_data_patients[[pid]], collapse = ', ')))
    }
  }

  # storing the results
  fulfill_count <- length(which(evaluated_patients$status == 1))
  total_count <- fulfill_count + length(which(evaluated_patients$status == 0))
  missing_count <- length(which(evaluated_patients$status == 2))

  # printing results to the console
  cat ('STOPP C2: ', fulfill_count, 'patients out of', total_count, 'patients fulfill the criterion.', missing_count, 'patients have missing data. \n')

  if (excel_out) {
    # export the evaluated list of patients to excel file
    write_xlsx(evaluated_patients, path = paste0( export_data_path, '/STOPP-C2.xlsx'), col_names = TRUE)
  }

  invisible (list(evaluated_patients)) # instead of return as we do not want to be printed
}
