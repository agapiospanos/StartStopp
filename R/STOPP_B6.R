#' Evaluates the imported patients' data for the STOPP B6 criterion.
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


STOPP_B6 <- function(path, excel_out = TRUE, export_data_path=getwd()) {

  missing_data_patients <- list()

  # the variable to keep the final data frame of patients:
  # 0 marks the patient that does not fulfill the criterion,
  # 1 marks the patient that fulfills the criterion and
  # 2 marks the patient with missing data.
  evaluated_patients <- data.frame(patients = character(0), status = numeric(0), missing_variables = character(0))

  # Importing the data
  data <- import_excel_data(path = path, worksheet = 1, var_col = 'med_gen__decod')
  data <- import_excel_data(current_data = data, path = path, worksheet = 2, var_col = 'ih_icd10__decod')
  data <- import_excel_data(current_data = data, path = path, worksheet = 3, var_col = 'h_icd10__decod', ignore_na = TRUE ) # in the third sheet we ignore the n/a as they refer to a patient that visited the hospital but nothing was recorded.
  pdata <- data[[1]]
  missing_data_patients <- data[[2]]

  # iterration over all patients
  for ( i in 1: length(pdata)){
    # checking if the patient id is in the list of missing data
    pid <- names(sapply(pdata[i], names))

    if (is.na(match( pid, names(sapply(missing_data_patients, names))))){
      # checking for the conditions
      if ( any(grepl('^I50|I11.0|I13.0|I13.2', unlist(pdata[[i]][2]), ignore.case=T)) | # checking unless condition I50* OR I11.0 OR I13.0 OR I13.2 in the ih_icd_10_decod list
           any(grepl('^I50|I11.0|I13.0|I13.2', unlist(pdata[[i]][3]), ignore.case=T))   # checking unless condition I50* OR I11.0 OR I13.0 OR I13.2 in the h_icd_10_decod list
      ){
        # inserting the record to the data.frame evaluated_patients
        evaluated_patients <- rbind(evaluated_patients, data.frame(patients = pid, status = 0, missing_variables = ''))
      } else {
        #checking if fulfills at least one primary condition AND at least one secondary condition
        if ( ( any(grepl('^C03C|^C03EB', unlist(pdata[[i]][1]), ignore.case=T)) # checking primary condition C03C* OR C03EB*  in the med_gen_decod list
              ) & ( # AND between the primary and secondary conditions
                any(grepl('I10|^I15', unlist(pdata[[i]][2]), ignore.case=T)) | # checking secondary condition I10 OR I15* in the ih_icd_10_decod list
                any(grepl('I10|^I15', unlist(pdata[[i]][3]), ignore.case=T))   # checking secondary condition I10 in the h_icd_10_decod list
              )
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
  fullfil_count <- length(which(evaluated_patients$status == 1))
  total_count <- fullfil_count + length(which(evaluated_patients$status == 0))
  missing_count <- length(which(evaluated_patients$status == 2))

  # printing results to the console
  cat ('STOPP B6: ', fullfil_count, 'patients out of', total_count, 'patients fulfill the criterion.', missing_count, 'patients have missing data. \n')

  if (excel_out) {
    # export the evaluated list of patients to excel file
    write_xlsx(evaluated_patients, path = paste0( export_data_path, '/STOPP-B6.xlsx'), col_names = TRUE)
  }

  invisible (list(evaluated_patients)) # instead of return as we do not want to be printed
}
