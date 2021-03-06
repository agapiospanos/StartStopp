#' Evaluates the imported patients' data for the STOPP H6 criterion.
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


STOPP_H6 <- function(path = NULL, excel_out = TRUE, export_data_path = NULL, suppressNA = TRUE) {

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
  data <- import_excel_data(current_data = data, path = path, worksheet = 1, var_col = 'med_long_term', include_missing = suppressNA, ignore_na = suppressNA )
  data <- import_excel_data(current_data = data, path = path, worksheet = 2, var_col = 'ih_icd10__decod', include_missing = suppressNA, ignore_na = suppressNA )
  data <- import_excel_data(current_data = data, path = path, worksheet = 3, var_col = 'h_icd10__decod', include_missing = suppressNA, ignore_na = TRUE ) # in the third sheet we ignore the n/a as they refer to a patient that visited the hospital but nothing was recorded.

  pdata <- data[[1]]
  missing_data_patients <- data[[2]]

  # iterration over all patients
  for ( i in 1: length(pdata)){
    # checking if the patient id is in the list of missing data
    pid <- names(sapply(pdata[i], names))

    if (is.na(match( pid, names(sapply(missing_data_patients, names))))){
      # get vectors of atc codes and long-term medicine variable of patient
      patient_atc_codes <- unlist(pdata[[i]][1])
      long_term <- unlist(pdata[[i]][2])

      cond1 <- cond2 <- cond3 <- cond4 <- FALSE

      index1 <- grep('^M01A', patient_atc_codes, ignore.case = T)
      if (length(index1) > 0) { # we get length of index because the grep returns an empty integer vector if the M01A* is not found.
        med_long_term1 <- as.numeric(unlist(long_term[index1]))
        med_long_term1 <- med_long_term1[!is.na(med_long_term1)]
        if (length(med_long_term1) > 0) {
          cond1 <- any(med_long_term1 == 1)
        }
      }

      index2 <- grep('^N02BA', patient_atc_codes, ignore.case = T)
      if (length(index2) > 0) { # we get length of index because the grep returns an empty integer vector if the N02BA* is not found.
        med_long_term2 <- as.numeric(unlist(long_term[index2]))
        med_long_term2 <- med_long_term2[!is.na(med_long_term2)]
        if (length(med_long_term2) > 0) {
          cond2 <- any(med_long_term2 == 1)
        }
      }

      index3 <- grep('M04AC01', patient_atc_codes, ignore.case = T)
      if (length(index3) > 0) { # we get length of index because the grep returns an empty integer vector if the M04AC01 is not found.
        med_long_term3 <- as.numeric(long_term[index3])
        med_long_term3 <- med_long_term3[!is.na(med_long_term3)]
        if (length(med_long_term3) > 0) {
          cond3 <- any(med_long_term3 == 1)
        }
      }

      index4 <- grep('^M01BA', patient_atc_codes, ignore.case = T)
      if (length(index4) > 0) { # we get length of index because the grep returns an empty integer vector if the M01BA* is not found.
        med_long_term4 <- as.numeric(long_term[index4])
        med_long_term4 <- med_long_term4[!is.na(med_long_term4)]
        if (length(med_long_term4) > 0) {
          cond4 <- any(med_long_term4 == 1)
        }
      }

      #checking if fulfills at least one primary condition AND at least one secondary condition
      if (
        ( cond1 | cond2 | cond3 | cond4 ) & # AND between the primary and secondary conditions
        ( any(grepl('^M10', unlist(pdata[[i]][3]), ignore.case=T)) | # checking secondary condition M10* in the ih_icd_10_decod list
          any(grepl('^M10', unlist(pdata[[i]][4]), ignore.case=T))   # checking secondary condition M10* in the h_icd_10_decod list
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
    cat('STOPP H6: ', fulfill_count, 'patients out of', total_count + missing_count, 'patients meet the criterion.\n')
  } else {
    cat('STOPP H6: ', fulfill_count, 'patients out of', total_count, 'patients meet the criterion.', missing_count, 'patients have missing data. \n')
  }

  if (excel_out) {
    # export the evaluated list of patients to excel file
    write_xlsx(evaluated_patients, path = paste0( export_data_path, '/STOPP-H6.xlsx'), col_names = TRUE)
  }

  invisible (list(evaluated_patients)) # instead of return as we do not want to be printed
}
