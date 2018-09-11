#' Evaluates the imported patients' data for the STOPP D11 criterion.
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


STOPP_D11 <- function(path, excel_out = TRUE, export_data_path=getwd()) {

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
      #checking if fulfills at least one primary condition AND at least one secondary condition
      if ( any(grepl('^N06DA', unlist(pdata[[i]][1]), ignore.case=T)) & # checking primary condition N06DA* in the med_gen_decod list
           ( any(grepl('^C07|C01AA05|C01AA08|C08DB01|C08DA01|C09BB10|C08DA51', unlist(pdata[[i]][1]), ignore.case=T)) | # checking secondary conditions C07* OR C01AA05 OR C01AA08 OR C08DB01 OR C08DA01 OR C09BB10 OR C08DA51 in the med_gen_decod list
             any(grepl('I49.5|R00.1|I44.1|I44.2|I44.3|I45.5|I45.9|Q24.9|R55', unlist(pdata[[i]][2]), ignore.case=T)) ) # checking secondary conditions I49.5 OR R00.1 OR I44.1 OR I44.2 OR I44.3 OR I45.5 OR I45.9 OR Q24.9 OR R55 in the ih_icd10__decod list
      ) {
        # inserting the record to the data.frame evaluated_patients
        evaluated_patients <- rbind(evaluated_patients, data.frame(patients = pid, status = 1, missing_variables = ''))
      } else {
        # inserting the record to the data.frame evaluated_patients
        evaluated_patients <- rbind(evaluated_patients, data.frame(patients = pid, status = 0, missing_variables = ''))
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
  cat ('STOPP D11: ', fulfill_count, 'patients out of', total_count, 'patients fulfill the criterion.', missing_count, 'patients have missing data. \n')

  if (excel_out) {
    # export the evaluated list of patients to excel file
    write_xlsx(evaluated_patients, path = paste0( export_data_path, '/STOPP-D11.xlsx'), col_names = TRUE)
  }

  invisible (list(evaluated_patients)) # instead of return as we do not want to be printed
}
