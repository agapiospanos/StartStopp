#' Evaluates the imported patients' data for the START E2 criterion.
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


START_E2 <- function(path = NULL, excel_out = TRUE, export_data_path = NULL, suppressNA = TRUE) {

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

  pdata <- data[[1]]
  missing_data_patients <- data[[2]]

  # iterration over all patients
  for ( i in 1: length(pdata)){
    # checking if the patient id is in the list of missing data
    pid <- names(sapply(pdata[i], names))

    if (is.na(match( pid, names(sapply(missing_data_patients, names))))){

      # checking for without conditions
      if (
           any(grepl('M05BB04|M05BB05|M05BB08', unlist(pdata[[i]][1]), ignore.case=T)) |
           ( any(grepl('M05BA', unlist(pdata[[i]][1]), ignore.case=T))  &
             any(grepl('A11CC|A11CB', unlist(pdata[[i]][1]), ignore.case=T)) &
             any(grepl('A12AA', unlist(pdata[[i]][1]), ignore.case=T))
           ) |
           ( any(grepl('M05BA', unlist(pdata[[i]][1]), ignore.case=T))  &
             any(grepl('A12AX', unlist(pdata[[i]][1]), ignore.case=T))
           ) |
           ( any(grepl('A12AA|A12AX', unlist(pdata[[i]][1]), ignore.case=T)) &
             any(grepl('M05BB03|M05BB06|M05BB07', unlist(pdata[[i]][1]), ignore.case=T))
           )
         )
      {
        # inserting the record to the data.frame evaluated_patients
        evaluated_patients <- rbind(evaluated_patients, data.frame(patients = pid, status = 0, missing_variables = ''))
      } else {

        # get vectors of atc codes and med_long_term for the current patient
        patient_atc_codes <- unlist(pdata[[i]][1])
        med_long_term <- unlist(pdata[[i]][2])
        all_na <- which(is.na(med_long_term))

        if (length(all_na) > 0) {
          patient_atc_codes <- patient_atc_codes[-all_na]
          med_long_term <- med_long_term[-all_na]
        }

        cond1 <- FALSE

        index1 <- grep('^H02AB', patient_atc_codes, ignore.case = T)
        if (length(index1) > 0) { # we get length of index because the grep returns an empty integer vector if the H02AB* is not found.
          med_long_term_single <- as.numeric(unlist(med_long_term[index1]))
          med_long_term_single <- med_long_term_single[!is.na(med_long_term_single)] # removing all NAs
          if (length(med_long_term_single) > 0) {
            cond1 <- any(med_long_term_single == 1) # checking if the patient receives the H02AB* for a long term period
          }
        }

        if ( any(grepl('^H02AB', unlist(pdata[[i]][1]), ignore.case=T)) & # checking primary condition H02AB* in the med_gen__decod list
             cond1 # checking if med_long_term == 1 for the medicine H02AB* for this patient
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
    cat('START E2: ', fulfill_count, 'patients out of', total_count + missing_count, 'patients meet the criterion.\n')
  } else {
    cat('START E2: ', fulfill_count, 'patients out of', total_count, 'patients meet the criterion.', missing_count, 'patients have missing data. \n')
  }

  if (excel_out) {
    # export the evaluated list of patients to excel file
    write_xlsx(evaluated_patients, path = paste0( export_data_path, '/START-E2.xlsx'), col_names = TRUE)
  }

  invisible (list(evaluated_patients)) # instead of return as we do not want to be printed
}
