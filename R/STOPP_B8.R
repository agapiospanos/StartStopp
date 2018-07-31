#' Evaluates the imported patients' data for the STOPP B8 criterion.
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


STOPP_B8 <- function(path, excel_out = TRUE, export_data_path=getwd()) {

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
  data <- import_excel_data(current_data = data, path = path, worksheet = 4, var_col = 'cp_pot')
  data <- import_excel_data(current_data = data, path = path, worksheet = 4, var_col = 'cp_sod')
  data <- import_excel_data(current_data = data, path = path, worksheet = 4, var_col = 'cp_cal')

  pdata <- data[[1]]
  missing_data_patients <- data[[2]]

  # iterration over all patients
  for ( i in 1: length(pdata)){
    # checking if the patient id is in the list of missing data
    pid <- names(sapply(pdata[i], names))

    if (is.na(match( pid, names(sapply(missing_data_patients, names))))){ # checking if missing_data_patients contain pid

      # checking if fulfills at least one primary condition
      if ( ( any(grepl('^C07B|^C03BA|^C03A|C09XA52|C09XA54|C03EA01|C03EA02|C03EA07|C03EA13|C09DX01|C09DX03|^C07D', unlist(pdata[[i]][1]), ignore.case=T)) # checking primary condition C07B* OR C03BA* OR C03A* OR C09XA52 OR C09XA54 OR C03EA01 OR C03EA02 OR C03EA07 OR C03EA13 OR C09DX01 OR C07D* in the med_gen_decod list
            ) & ( # AND between the primary and secondary conditions
              any(grepl('E87.6|E87.1|E83.5|^M10', unlist(pdata[[i]][2]), ignore.case=T)) | # checking secondary condition E87.5 OR E87.1 OR E83.5 OR M10* in the ih_icd_10_decod list
              any(grepl('E87.6|E87.1|E83.5|^M10', unlist(pdata[[i]][3]), ignore.case=T)) | # checking secondary condition E87.5 OR E87.1 OR E83.5 OR M10* in the h_icd_10_decod list
              any(as.numeric(unlist(pdata[[i]][4])) < 3)  | # checking if cp_pot (K+) < 3
              any(as.numeric(unlist(pdata[[i]][5])) < 130) | # checking if cp_sod (Na+) < 130
              any(as.numeric(unlist(pdata[[i]][6])) > 2.65)  # checking if cp_cal (Ca++) > 2.65
            )
      ) {
        # inserting the record to the data.frame evaluated_patients with status 1
        evaluated_patients <- rbind(evaluated_patients, data.frame(patients = pid, status = 1, missing_variables = ''))
      } else {

        # inserting the record to the data.frame evaluated_patients with status 0
        evaluated_patients <- rbind(evaluated_patients, data.frame(patients = pid, status = 0, missing_variables = ''))
      }
    } else { # patient has missing data

      # inserting the record to the data.frame evaluated_patients with status 2
      evaluated_patients <- rbind(evaluated_patients, data.frame(patients = pid, status = 2, missing_variables = paste(missing_data_patients[[pid]], collapse = ', ')))
    }
  }

  # storing the results
  fulfill_count <- length(which(evaluated_patients$status == 1))
  total_count <- fulfill_count + length(which(evaluated_patients$status == 0))
  missing_count <- length(which(evaluated_patients$status == 2))

  # printing results to the console
  cat ('STOPP B8: ', fulfill_count, 'patients out of', total_count, 'patients fulfill the criterion.', missing_count, 'patients have missing data. \n')

  if (excel_out) {
    # export the evaluated list of patients to excel file
    write_xlsx(evaluated_patients, path = paste0( export_data_path, '/STOPP-B8.xlsx'), col_names = TRUE)
  }

  invisible (list(evaluated_patients)) # instead of return as we do not want to be printed
}
