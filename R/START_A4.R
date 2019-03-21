#' Evaluates the imported patients' data for the START A4 criterion.
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


START_A4 <- function(path = NULL, excel_out = TRUE, export_data_path = NULL, suppressNA = TRUE) {

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
  data <- import_excel_data(current_data = data, path = path, worksheet = 4, var_col = 'cp_syst', include_missing = TRUE, ignore_na = suppressNA )
  data <- import_excel_data(current_data = data, path = path, worksheet = 4, var_col = 'cp_diast', include_missing = TRUE, ignore_na = suppressNA )

  pdata <- data[[1]]
  missing_data_patients <- data[[2]]

  # iterration over all patients
  for ( i in 1: length(pdata)){

    # checking if the patient id is in the list of missing data
    pid <- names(sapply(pdata[i], names))

    missing_syst <- is.na(as.numeric(unlist(pdata[[i]][4])))
    missing_diast <- is.na(as.numeric(unlist(pdata[[i]][5])))

    if (is.na(match( pid, names(sapply(missing_data_patients, names))))){

      # checking if both the cp_syst AND cp_diast are NAs OR if there are without conditions. If this is the case, we mark the patient with 0.
      if((missing_syst & missing_diast) | any(grepl('^C07|^C08|^C09|^C03A|^C03EA|^C02', unlist(pdata[[i]][1]), ignore.case=T))){

        # inserting the record to the data.frame evaluated_patients
        evaluated_patients <- rbind(evaluated_patients, data.frame(patients = pid, status = 0, missing_variables = ''))

      } else {

        diast_over_limit <- syst_over_low_limit <- syst_over_high_limit <- FALSE # setting the boolean vars to false by default so that if one of the cp_diast or cp_syst is NA it does not affect the result.

        if (missing_syst) { # if cp_syst is NA then we check only the
            diast_over_limit <- (as.numeric(unlist(pdata[[i]][5]))) > 90 # checking if cp_diast > 90
        }

        if (missing_diast) {
            syst_over_low_limit <- (as.numeric(unlist(pdata[[i]][4]))) > 140  # checking if cp_syst > 140
            syst_over_high_limit <- (as.numeric(unlist(pdata[[i]][4]))) > 160 # checking if cp_syst > 160
        }

        if (
             (
               ( any(grepl('^E10|^E11|^E12|^E13|^E14', unlist(pdata[[i]][2]), ignore.case=T)) | # checking for primary conditions E10* OR E11* OR E12* OR E13* OR E14* in the ih_icd10__decod list
                 any(grepl('^E10|^E11|^E12|^E13|^E14', unlist(pdata[[i]][3]), ignore.case=T))   # checking for primary conditions E10* OR E11* OR E12* OR E13* OR E14* in the h_icd10__decod list
               ) & (
                 syst_over_low_limit | # checking if cp_syst > 140
                 diast_over_limit # checking if cp_diast > 90
               )
             ) | (
               syst_over_high_limit | # checking if cp_syst > 160
               diast_over_limit # checking if cp_diast > 90
             )
           )
        {
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
  if (suppressNA) {
    cat('START A4: ', fulfill_count, 'patients out of', total_count + missing_count, 'patients meet the criterion.\n')
  } else {
    cat('START A4: ', fulfill_count, 'patients out of', total_count, 'patients meet the criterion.', missing_count, 'patients have missing data. \n')
  }

  if (excel_out) {
    # export the evaluated list of patients to excel file
    write_xlsx(evaluated_patients, path = paste0( export_data_path, '/START-A4.xlsx'), col_names = TRUE)
  }

  invisible (list(evaluated_patients)) # instead of return as we do not want to be printed
}
