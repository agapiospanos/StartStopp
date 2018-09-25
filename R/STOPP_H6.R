#' Evaluates the imported patients' data for the STOPP H6 criterion.
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


STOPP_H6 <- function(path, excel_out = TRUE, export_data_path=getwd()) {

  missing_data_patients <- list()

  # the variable to keep the final data frame of patients:
  # 0 marks the patient that does not fulfill the criterion,
  # 1 marks the patient that fulfills the criterion and
  # 2 marks the patient with missing data.
  evaluated_patients <- data.frame(patients = character(0), status = numeric(0), missing_variables = character(0))

  # Importing the data
  data <- import_excel_data(path = path, worksheet = 1, var_col = 'med_gen__decod')
  data <- import_excel_data(path = path, worksheet = 1, var_col = 'med_long_term')
  data <- import_excel_data(current_data = data, path = path, worksheet = 2, var_col = 'ih_icd10__decod')

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
      if (length(index1)>0) { # we get length of index because the grep returns an empty integer vector if the M01A* is not found.
        if (as.numeric(long_term[index1]) == 1) { # checking if patient receives that medicine long-term
          cond1 <- TRUE
        }
      }

      index2 <- grep('^N02BA', patient_atc_codes, ignore.case = T)
      if (length(index2)>0) { # we get length of index because the grep returns an empty integer vector if the N02BA* is not found.
        if (as.numeric(long_term[index2]) == 1) { # checking if patient receives that medicine long-term
          cond2 <- TRUE
        }
      }

      index3 <- grep('M04AC01', patient_atc_codes, ignore.case = T)
      if (length(index3)>0) { # we get length of index because the grep returns an empty integer vector if the M04AC01 is not found.
        if (as.numeric(long_term[index3]) == 1) { # checking if patient receives that medicine long-term
          cond3 <- TRUE
        }
      }

      index4 <- grep('^M01BA', patient_atc_codes, ignore.case = T)
      if (length(index4)>0) { # we get length of index because the grep returns an empty integer vector if the M01BA* is not found.
        if (as.numeric(long_term[index4]) == 1) { # checking if patient receives that medicine long-term
          cond4 <- TRUE
        }
      }

      #checking if fulfills at least one primary condition AND at least one secondary condition
      if (
        ( cond1 | cond2 | cond3 | cond4 ) & # AND between the primary and secondary conditions
        any(grepl('^M10', unlist(pdata[[i]][3]), ignore.case=T)) # checking secondary condition M10* in the ih_icd_10_decod list
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
  cat ('STOPP H6: ', fulfill_count, 'patients out of', total_count, 'patients fulfill the criterion.', missing_count, 'patients have missing data. \n')

  if (excel_out) {
    # export the evaluated list of patients to excel file
    write_xlsx(evaluated_patients, path = paste0( export_data_path, '/STOPP-H6.xlsx'), col_names = TRUE)
  }

  invisible (list(evaluated_patients)) # instead of return as we do not want to be printed
}
