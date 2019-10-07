#' Evaluates the imported patients' data for the START H1 criterion.
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


START_H1 <- function(path = NULL, excel_out = TRUE, export_data_path = NULL, suppressNA = TRUE) {

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
  data <- import_excel_data(current_data = data, path = path, worksheet = 1, var_col = 'daily_dosage', include_missing = TRUE, ignore_na = suppressNA )
  data <- import_excel_data(current_data = data, path = path, worksheet = 2, var_col = 'ih_icd10__decod', include_missing = suppressNA, ignore_na = suppressNA )
  data <- import_excel_data(current_data = data, path = path, worksheet = 3, var_col = 'h_icd10__decod', include_missing = suppressNA, ignore_na = TRUE ) # in the third sheet we ignore the n/a as they refer to a patient that visited the hospital but nothing was recorded.

  pdata <- data[[1]]
  missing_data_patients <- data[[2]]

  # iterration over all patients
  for ( i in 1: length(pdata)){
    # checking if the patient id is in the list of missing data
    pid <- names(sapply(pdata[i], names))

    if (is.na(match( pid, names(sapply(missing_data_patients, names))))){

      if ( any(grepl('N02AA01|N02AA03|N02AA04|N02AA05|N02AA51|N02AB02|N02AB03|N02AB52|N02AB72|N05AC52|N02AD01|N02AD02|N02AE01|N02AG01|N02AG03|N02AG04|N07BC02|N07BC05', unlist(pdata[[i]][1]), ignore.case=T)) ) # checking for without condition in the med_gen__decod list
      {
        # inserting the record to the data.frame evaluated_patients
        evaluated_patients <- rbind(evaluated_patients, data.frame(patients = pid, status = 0, missing_variables = ''))

      } else {

        # get vectors of atc codes and medicine strength for the current patient
        patient_atc_codes <- unlist(pdata[[i]][1])
        daily_dosage <- unlist(pdata[[i]][2])

        daily_dosage_cond <- FALSE

        index <- grep('N02BE01', patient_atc_codes, ignore.case = T)
        if (length(index) > 0) { # we get length of index because the grep returns an empty integer vector if the B01AC06 is not found.
          daily_dosage_single <- as.numeric(unlist(daily_dosage[index]))
          daily_dosage_single <- daily_dosage_single[!is.na(daily_dosage_single)]
          if (length(daily_dosage_single) > 0) {
            daily_dosage_cond <- any(daily_dosage_single > 2.9) # checking if daily_dosage for this atc code is greater than 2.9
          }
        }

        # checking if fulfills at least one set of primary condition AND secondary condition
        if (
            (
              any(grepl('^R52', unlist(pdata[[i]][3]), ignore.case=T)) | # checking primary condition R52* in the ih_icd10__decod list
              any(grepl('^R52', unlist(pdata[[i]][4]), ignore.case=T))   # checking primary condition R52* in the h_icd10__decod list
            ) & (
              (
                daily_dosage_cond & # checking if daily dosage for the N02BE01 atc code is greater than 2.9
                any(grepl('N02BE01', unlist(pdata[[i]][1]), ignore.case=T)) # checking the conditions N02BE01 in the med_gen__decod list
              ) |
              any(grepl('N02AA08|N02AA58|N05AA59|N02AA79|N02AX01|N02AX02|N02BA01|N02BA15|N05BA51|N02BA65|N02BA71|^M01A', unlist(pdata[[i]][1]), ignore.case=T)) # checking the conditions N02AA08 OR N02AA58 OR N05AA59 OR N02AA79 OR N02AX01 OR N02AX02 OR N02BA01 OR N02BA15 OR N05BA51 OR N02BA65 OR N02BA71 OR M01A* in the med_gen__decod list
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
    cat('START H1: ', fulfill_count, 'patients out of', total_count + missing_count, 'patients meet the criterion.\n')
  } else {
    cat('START H1: ', fulfill_count, 'patients out of', total_count, 'patients meet the criterion.', missing_count, 'patients have missing data. \n')
  }

  if (excel_out) {
    # export the evaluated list of patients to excel file
    write_xlsx(evaluated_patients, path = paste0( export_data_path, '/START-H1.xlsx'), col_names = TRUE)
  }

  invisible (list(evaluated_patients)) # instead of return as we do not want to be printed
}
