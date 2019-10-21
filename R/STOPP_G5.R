#' Evaluates the imported patients' data for the STOPP G5 criterion.
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


STOPP_G5 <- function(path = NULL, excel_out = TRUE, export_data_path = NULL, suppressNA = TRUE) {

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
  data <- import_excel_data(current_data = data, path = path, worksheet = 4, var_col = 'cp_po', include_missing = suppressNA, ignore_na = suppressNA )
  data <- import_excel_data(current_data = data, path = path, worksheet = 4, var_col = 'cp_po_unit', include_missing = suppressNA, ignore_na = suppressNA )
  data <- import_excel_data(current_data = data, path = path, worksheet = 4, var_col = 'cp_pco', include_missing = suppressNA, ignore_na = suppressNA )
  data <- import_excel_data(current_data = data, path = path, worksheet = 4, var_col = 'cp_pco_unit', include_missing = suppressNA, ignore_na = suppressNA )

  pdata <- data[[1]]
  missing_data_patients <- data[[2]]

  # iterration over all patients
  for (i in 1:length(pdata)) {
    # checking if the patient id is in the list of missing data
    pid <- names(sapply(pdata[i], names))

    if (is.na(match( pid, names(sapply(missing_data_patients, names))))) {

      cp_po <- as.numeric(unlist(pdata[[i]][2]))
      cp_po <- cp_po[!is.na(cp_po)]

      cp_po_unit <- unlist(pdata[[i]][3])

      cp_po_cond <- FALSE

      if (length(cp_po) > 0 & length(cp_po_unit) > 0){

        if (length(unique(cp_po_unit)) > 1) warning(paste('More than 1 cp_po_units found', unique(cp_po_unit), 'for patient', pid, '. The algorithm may not produce the expected results. Please remove one of the units for this patient.'))

        if (grepl('kPA', cp_po_unit, ignore.case = T)) { # using grepl so that we can evaluate the unit in a case insensitive way
          cp_po_cond <- any(cp_po < 8)
        } else if (grepl('mmHg', cp_po_unit, ignore.case = T)) {
          cp_po_cond <- any(cp_po < 60)
        } else {
          warning(paste('the cp_po_unit for patient ', pid, ' is not valid. It must be either kPA or mmHg. Please correct this and rerun the criterion'))
        }
      }

      cp_po_cond <- FALSE

      cp_pco <- as.numeric(unlist(pdata[[i]][4]))
      cp_pco <- cp_pco[!is.na(cp_pco)]

      cp_pco_unit <- unlist(pdata[[i]][5])

      if (length(cp_pco) > 0 & length(cp_pco_unit) > 0){

        if (length(unique(cp_pco_unit)) > 1) warning(paste('More than 1 cp_pco_units found:', unique(cp_po_unit), 'for patient', pid, '. The algorithm may not produce the expected results. Please remove one of the units for this patient.'))

        if (grepl('kPA', cp_pco_unit, ignore.case = T)) { # using grepl so that we can evaluate the unit in a case insensitive way
          cp_pco_cond <- any(cp_pco > 6.5)
        } else if (grepl('mmHg', cp_pco_unit, ignore.case = T)) {
          cp_pco_cond <- any(cp_pco > 48.75)
        } else {
          warning(paste('the cp_po_unit for patient ', pid, ' is not valid. It must be either kPA or mmHg. Please correct this and rerun the criterion'))
        }
      }

      # checking if fulfills at least one set of primary condition AND secondary condition
      if ( any(grepl('^N05BA|^N05CD|^N05CF|^N03AE', unlist(pdata[[i]][1]), ignore.case=T)) & # checking for primary conditions N05BA* OR N05CD* OR N05CF* OR N03AE* in the med_gen_decod list
           ( cp_po_cond | cp_pco_cond )
      )
      {
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
    cat('STOPP G5: ', fulfill_count, 'patients out of', total_count + missing_count, 'patients meet the criterion.\n')
  } else {
    cat('STOPP G5: ', fulfill_count, 'patients out of', total_count, 'patients meet the criterion.', missing_count, 'patients have missing data. \n')
  }

  if (excel_out) {
    # export the evaluated list of patients to excel file
    write_xlsx(evaluated_patients, path = paste0( export_data_path, '/STOPP-G5.xlsx'), col_names = TRUE)
  }

  invisible (list(evaluated_patients)) # instead of return as we do not want to be printed
}
