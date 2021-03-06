#' Evaluates the imported patients' data for the START A5 criterion.
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


START_A5 <- function(path = NULL, excel_out = TRUE, export_data_path = NULL, suppressNA = TRUE) {

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
  data <- import_excel_data(current_data = data, path = path, worksheet = 5, var_col = 'fup2m_date', include_missing = TRUE, ignore_na = suppressNA )
  data <- import_excel_data(current_data = data, path = path, worksheet = 5, var_col = 'dm_dob', include_missing = TRUE, ignore_na = suppressNA )

  pdata <- data[[1]]
  missing_data_patients <- data[[2]]

  # iterration over all patients
  for ( i in 1: length(pdata)){

    # checking if the patient id is in the list of missing data
    pid <- names(sapply(pdata[i], names))

    fup2m_date_full <- unlist(pdata[[i]][4])
    fup2m_date_full <- fup2m_date_full[!is.na(fup2m_date_full)]

    if (length(fup2m_date_full) > 1) {
      warning(paste('fup2m_date for patient', pid, 'has >1 values. Please note that code expects only 1 value to work as expected'))
    }

    fup2m_date <- as.numeric(substr(fup2m_date_full, nchar(fup2m_date_full) - 3, nchar(fup2m_date_full))) # getting only the year from fup2m_date (e.g. from 27/01/2017 to 2017 )


    dob_year <- unlist(pdata[[i]][5]) # date of birth year
    dob_year <- as.numeric(dob_year[!is.na(dob_year)])

    if (length(dob_year) > 1) {
      warning(paste('dob_year for patient', pid, 'has >1 values. Please note that code expects only 1 value to work as expected'))
    }

    year_diff <- FALSE

    if (length(fup2m_date) > 0 & length(dob_year) > 0) {
      year_diff <- any((fup2m_date - dob_year) < 85)
    }

    if (is.na(match( pid, names(sapply(missing_data_patients, names))))){

      # checking if there are without conditions. If this is the case, we mark the patient with 0.
      if(any(grepl('^C10', unlist(pdata[[i]][1]), ignore.case=T))){

        # inserting the record to the data.frame evaluated_patients
        evaluated_patients <- rbind(evaluated_patients, data.frame(patients = pid, status = 0, missing_variables = ''))

      } else {

        primary_conditions <-
          ( any(grepl('^I20|^I21|^I22|^I24|^I25|^I63|^I64|^I65|^I66|I73.9|^I74|^G45|Z95.1|Z95.5|Z95.8', unlist(pdata[[i]][2]), ignore.case=T)) | # checking for primary conditions I20* OR I21* OR I22* OR I24* OR I25* OR I63* OR I64 OR I65* OR I66* OR I73.9 OR I74* OR G45* OR Z95.1 OR Z95.5 OR Z95.8 in the ih_icd10__decod list
            any(grepl('^I20|^I21|^I22|^I24|^I25|^I63|^I64|^I65|^I66|I73.9|^I74|^G45|Z95.1|Z95.5|Z95.8', unlist(pdata[[i]][3]), ignore.case=T))   # checking for primary conditions I20* OR I21* OR I22* OR I24* OR I25* OR I63* OR I64 OR I65* OR I66* OR I73.9 OR I74* OR G45* OR Z95.1 OR Z95.5 OR Z95.8 in the h_icd10__decod list
          )

        if (
             primary_conditions |
             (
               primary_conditions &
               year_diff
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
    cat('START A5: ', fulfill_count, 'patients out of', total_count + missing_count, 'patients meet the criterion.\n')
  } else {
    cat('START A5: ', fulfill_count, 'patients out of', total_count, 'patients meet the criterion.', missing_count, 'patients have missing data. \n')
  }

  if (excel_out) {
    # export the evaluated list of patients to excel file
    write_xlsx(evaluated_patients, path = paste0( export_data_path, '/START-A5.xlsx'), col_names = TRUE)
  }

  invisible (list(evaluated_patients)) # instead of return as we do not want to be printed
}
