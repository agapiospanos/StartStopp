#' Imports patient data from multiple sheets of excel file.
#'
#' @param current_data (List) the data that was imported by another call of import_excel_data so that this call will append the new data to the old ones.
#' @param path (Character) the path that the excel file can be read from.
#' @param worksheet (Character) the worksheet that the desired variable is stored in.
#' @param var_col (Character) the column that stores the desired variable
#' @param ignore_na (Boolean) (optional) (default: FALSE) a boolean variable that indicates whether to ignore any N/As found in excel (if set to FALSE) or to handle them as missing data (if set to TRUE).
#' @param include_missing (Boolean) (optional) (default: FALSE) a boolean that indicated whether to include the missing values as NAs in the data to achieve one to one correspondance in the data if necessary.
#' @return list of [[1]] patient data for the specified variable. [[2]] missing patient id and variable name as specified in excel that we used to import data.
#'
#' @author
#' Agapios Panos <panosagapios@gmail.com>
#'
#' @importFrom readxl read_excel

import_excel_data <- function(current_data, path, worksheet, var_col, ignore_na=FALSE, include_missing=FALSE) {

  # specify the excel file location and worksheet
  sheet <- read_excel(path, worksheet)

  # ordering the sheet in ascending order by the specified column. Necessary for the proper parsing of the patient data as the algorithm expects the sheet data to be ordered and parses it in a serial way.
  sheet <- sheet[order(unlist(sheet['usubjid'])),]

  # storing the patients' ids
  pids <- unlist(sheet['usubjid'])

  # initializing the patient id for sheet using the first patient's id.
  pid <- pids[1]

  # initialize patient data
  pdata <- list()

  # initialize missing data for patients
  missing_data_patients <- list()

  # if the user specified current_data set them in pdata
  if(!missing(current_data)) {
    pdata <- current_data[[1]]
    missing_data_patients <- current_data[[2]]
  }

  # initializing the variable for the signle patient data.
  conditions <- c()

  # variable to check if this patient has missing data. Initialized to FALSE.
  has_missing <- FALSE

  for (i in 1:length(pids)) {
    if (pid == pids[i]) { # still the same patient id - collecting data

      #checking if it is NA - missing data in case we chose to not ignore them
      if (is.na(sheet[[var_col]][i])) {

        # if we will ignore the missing we will not store the data, neither we will set has_missing to TRUE
        if (!ignore_na){
          # update the logical var
          has_missing <- TRUE
        }

        # if we must include the missing to achieve 1-1 correspondace we insert an NA
        if(include_missing){
          conditions <- c(conditions, NA)
        }

      } else { # is not NA data
        # storing the data to the list
        conditions <- c(conditions, sheet[[var_col]][i])
      }

    } else { # new patient id

      # keep the current data to the patient data var
      pdata[[pids[i-1]]] <- c(pdata[[pids[i-1]]], list(conditions)) # i-1 because we are refering to the previous patient's data

      # we also record the id if it has missing data.
      if (has_missing){
        missing_data_patients[[pids[i-1]]] <- c(missing_data_patients[[pids[i-1]]], var_col) # i-1 because we are refering to the previous patient's data
      }

      # reseting the logical var for the next patient
      has_missing <- FALSE

      # assign the new id to the patient var so that we can compare to it in the next loop
      pid <- pids[i]

      # keep only the new data deleting the data from the old patient
      # checking if it is NA - missing data
      if (is.na(sheet[[var_col]][i])) {

        # if we will ignore the missing we will not store the data, neither we will set has_missing to TRUE and empty the list
        if (!ignore_na){
          # update the logical var
          has_missing <- TRUE
          # set conditions as an empty list
          conditions <- c()
        }

        # if we must include the missing to achieve 1-1 correspondace we insert an NA
        if(include_missing){
          conditions <- c(NA)
        }

      } else {
        # storing the data to the list
        conditions <- c(sheet[[var_col]][i])
      }
    }
  }

  # keep the data for the last patient. Till now it kept the data for the n-1 patients
  pdata[[pids[i]]] <- c(pdata[[pids[i]]], list(conditions))

  # we also record the id if she/he has missing data.
  if (has_missing){
    missing_data_patients[[pids[i]]] <- c(missing_data_patients[[pids[i]]], var_col)
  }

  # ret <- list(pdata, missing_data_patients)

  invisible (list(pdata, missing_data_patients))  # instead of return as we do not want to be printed
}
