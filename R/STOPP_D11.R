#' Evaluates the imported patients' data for the STOPP D11 criterion.
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


STOPP_D11 <- function(path = NULL, excel_out = TRUE, export_data_path = NULL, suppressNA = TRUE) {

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
  data <- import_excel_data(current_data = data, path = path, worksheet = 4, var_col = 'cp_hr', include_missing = TRUE, ignore_na = suppressNA )

  pdata <- data[[1]]
  missing_data_patients <- data[[2]]

  # iterration over all patients
  for ( i in 1: length(pdata)){
    # checking if the patient id is in the list of missing data
    pid <- names(sapply(pdata[i], names))

    if (is.na(match( pid, names(sapply(missing_data_patients, names))))){

      cp_hr_cond <- FALSE
      cp_hr <- as.numeric(unlist(pdata[[i]][4]))
      cp_hr <- cp_hr[!is.na(cp_hr)]

      if (length(cp_hr) > 0){
        cp_hr_cond <- any(cp_hr < 50) # checking if heart rate is bellow 50
      }

      #checking if fulfills at least one primary condition AND at least one secondary condition
      if ( any(grepl('^N06DA', unlist(pdata[[i]][1]), ignore.case=T)) & # checking primary condition N06DA* in the med_gen_decod list
           ( cp_hr_cond |
             any(grepl('^C07|C01AA05|C01AA08|C08DB01|C08DA01|C09BB10|C08DA51', unlist(pdata[[i]][1]), ignore.case=T)) | # checking secondary conditions C07* OR C01AA05 OR C01AA08 OR C08DB01 OR C08DA01 OR C09BB10 OR C08DA51 in the med_gen_decod list
             ( any(grepl('I49.5|R00.1|I44.1|I44.2|I44.3|I45.5|I45.9|Q24.9|R55', unlist(pdata[[i]][2]), ignore.case=T)) | # checking secondary conditions I49.5 OR R00.1 OR I44.1 OR I44.2 OR I44.3 OR I45.5 OR I45.9 OR Q24.9 OR R55 in the ih_icd10__decod list
               any(grepl('I49.5|R00.1|I44.1|I44.2|I44.3|I45.5|I45.9|Q24.9|R55', unlist(pdata[[i]][3]), ignore.case=T))   # checking secondary conditions I49.5 OR R00.1 OR I44.1 OR I44.2 OR I44.3 OR I45.5 OR I45.9 OR Q24.9 OR R55 in the h_icd10__decod list
             )
           )
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
  if (suppressNA) {
    cat('STOPP D11: ', fulfill_count, 'patients out of', total_count + missing_count, 'patients meet the criterion.\n')
  } else {
    cat('STOPP D11: ', fulfill_count, 'patients out of', total_count, 'patients meet the criterion.', missing_count, 'patients have missing data. \n')
  }

  if (excel_out) {
    # export the evaluated list of patients to excel file
    write_xlsx(evaluated_patients, path = paste0( export_data_path, '/STOPP-D11.xlsx'), col_names = TRUE)
  }

  invisible (list(evaluated_patients)) # instead of return as we do not want to be printed
}
