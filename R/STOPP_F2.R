#' Evaluates the imported patients' data for the STOPP F2 criterion.
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


STOPP_F2 <- function(path = NULL, excel_out = TRUE, export_data_path = NULL, suppressNA = TRUE) {

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
  data <- import_excel_data(current_data = data, path = path, worksheet = 1, var_col = 'daily_dosage', include_missing = suppressNA, ignore_na = suppressNA )
  data <- import_excel_data(current_data = data, path = path, worksheet = 1, var_col = 'med_long_term', include_missing = suppressNA, ignore_na = suppressNA )


  pdata <- data[[1]]
  missing_data_patients <- data[[2]]

  # iterration over all patients
  for ( i in 1: length(pdata)){
    # checking if the patient id is in the list of missing data
    pid <- names(sapply(pdata[i], names))

    if (is.na(match( pid, names(sapply(missing_data_patients, names))))){

      # get vectors of atc codes and medicine strength for the current patient
      patient_atc_codes <- unlist(pdata[[i]][1])
      daily_dosage <- unlist(pdata[[i]][2])
      med_long_term <- unlist(pdata[[i]][3])

      cond1 <- cond2 <- cond3 <- cond4 <- cond5 <- cond6 <- FALSE

      index1 <- grep('A02BC01', patient_atc_codes, ignore.case = T)
      if (length(index1) > 0) { # we get length of index because the grep returns an empty integer vector if this atc code is not found.
        cond1 <- eval_cond(index1, med_long_term, daily_dosage, 39)
      }

      index2 <- grep('A02BC02', patient_atc_codes, ignore.case = T)
      if (length(index2) > 0) { # we get length of index because the grep returns an empty integer vector if this atc code is not found.
        cond2 <- eval_cond(index2, med_long_term, daily_dosage, 79)
      }

      index3 <- grep('A02BC03', patient_atc_codes, ignore.case = T)
      if (length(index3) > 0) { # we get length of index because the grep returns an empty integer vector if this atc code is not found.
        cond3 <- eval_cond(index3, med_long_term, daily_dosage, 59)
      }

      index4 <- grep('A02BC04', patient_atc_codes, ignore.case = T)
      if (length(index4) > 0) { # we get length of index because the grep returns an empty integer vector if this atc code is not found.
        cond4 <- eval_cond(index4, med_long_term, daily_dosage, 19)
      }

      index5 <- grep('A02BC05', patient_atc_codes, ignore.case = T)
      if (length(index5) > 0) { # we get length of index because the grep returns an empty integer vector if this atc code is not found.
        cond5 <- eval_cond(index5, med_long_term, daily_dosage, 39)
      }

      index6 <- grep('A02BC06', patient_atc_codes, ignore.case = T)
      if (length(index6) > 0) { # we get length of index because the grep returns an empty integer vector if this atc code is not found.
        cond6 <- eval_cond(index6, med_long_term, daily_dosage, 29)
      }

      # checking if fulfills at least one set of primary condition AND secondary condition
      if ( cond1 | cond2 | cond3 | cond4 | cond5 | cond6 ) {
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
    cat('STOPP F2: ', fulfill_count, 'patients out of', total_count + missing_count, 'patients meet the criterion.\n')
  } else {
    cat('STOPP F2: ', fulfill_count, 'patients out of', total_count, 'patients meet the criterion.', missing_count, 'patients have missing data. \n')
  }

  if (excel_out) {
    # export the evaluated list of patients to excel file
    write_xlsx(evaluated_patients, path = paste0( export_data_path, '/STOPP-F2.xlsx'), col_names = TRUE)
  }

  invisible (list(evaluated_patients)) # instead of return as we do not want to be printed
}

# helper function to evaluate condition and return TRUE or FALSE
eval_cond <- function(index, med_long_term, daily_dosage, dosage_limit) {

  cond1 <- FALSE

  daily_dosage1 <- as.numeric(unlist(daily_dosage[index]))
  daily_dosage1 <- daily_dosage1[!is.na(daily_dosage1)]

  med_long_term1 <- as.numeric(unlist(med_long_term[index]))
  med_long_term1 <- med_long_term1[!is.na(med_long_term1)]

  if (length(daily_dosage1) > 0 & length(med_long_term1) > 0) {
    cond1 <- any(daily_dosage1 > dosage_limit) & any(med_long_term1 == 1) # checking daily dosage AND med long term for this atc code
  }

  return(cond1)
}
