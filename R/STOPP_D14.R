#' Evaluates the imported patients' data for the STOPP D14 criterion.
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


STOPP_D14 <- function(path, excel_out = TRUE, export_data_path=getwd()) {

  missing_data_patients <- list()

  # the variable to keep the final data frame of patients:
  # 0 marks the patient that does not fulfill the criterion,
  # 1 marks the patient that fulfills the criterion and
  # 2 marks the patient with missing data.
  evaluated_patients <- data.frame(patients = character(0), status = numeric(0), missing_variables = character(0))

  # Importing the data
  data <- import_excel_data(path = path, worksheet = 1, var_col = 'med_gen__decod')

  pdata <- data[[1]]
  missing_data_patients <- data[[2]]

  # iterration over all patients
  for ( i in 1: length(pdata)){
    # checking if the patient id is in the list of missing data
    pid <- names(sapply(pdata[i], names))

    if (is.na(match( pid, names(sapply(missing_data_patients, names))))){
      #checking if fulfills at least one primary condition AND at least one secondary condition
      if ( any(grepl('R06AD01|R06AB03|R06AB04|R06AB54|R06AB02|R06AB52|R06AA02|R06AA52|N05BB01|N05BB51|R06AE05|R06AE55|R06AD02|R06AD52|R06AA59|R06AX02|R06AA04|R06AA54|R06AC04|R06AD08|R06AE03|R06AE53|R06AE04|R06AE06|R06AX15|R06AX17|N07CA02|N07CA52', unlist(pdata[[i]][1]), ignore.case=T))
           # checking primary condition R06AD01 OR R06AB03 OR R06AB04 OR R06AB54 OR R06AB02 OR R06AB52 OR R06AA02 OR R06AA52
           # OR N05BB01 OR N05BB51 OR R06AE05 OR R06AE55 OR R06AD02 OR R06AD52 OR R06AA59 OR R06AX02 OR R06AA04 OR R06AA54
           # OR R06AC04 OR R06AD08 OR R06AE03 OR R06AE53 OR R06AE04 OR R06AE06 OR R06AX15 OR R06AX17 OR N07CA02 OR N07CA52 in the med_gen_decod list
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
  cat ('STOPP D14: ', fulfill_count, 'patients out of', total_count, 'patients fulfill the criterion.', missing_count, 'patients have missing data. \n')

  if (excel_out) {
    # export the evaluated list of patients to excel file
    write_xlsx(evaluated_patients, path = paste0( export_data_path, '/STOPP-D14.xlsx'), col_names = TRUE)
  }

  invisible (list(evaluated_patients)) # instead of return as we do not want to be printed
}
