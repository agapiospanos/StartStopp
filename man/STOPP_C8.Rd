% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/STOPP_C8.R
\name{STOPP_C8}
\alias{STOPP_C8}
\title{Evaluates the imported patients' data for the STOPP C8 criterion.}
\usage{
STOPP_C8(path = NULL, excel_out = TRUE, export_data_path = NULL,
  suppressNA = TRUE)
}
\arguments{
\item{path}{(Character) (optional) (default: NULL) the path that the excel file can be read from. If not specified a file choose window will be displayed.}

\item{excel_out}{(Boolean) (optional) (default: TRUE) output excel file with the evaluated data.}

\item{export_data_path}{(Character) (optional) (default: NULL (a popup message to choose dir will be displayed)) the path for excel file output.}

\item{suppressNA}{(Boolean) (optional) (default: TRUE) set this to FALSE if you want to know for which patients have NAs and for which variable. By default all NAs will be ignored so that the algorithm can distinguish between patients who meet the criterion and those who do not.}
}
\value{
list of lists of evaluated patient ids categorized in 1) the ids that fulfill the criterion, 2) the ids that do not fulfill the criterion and 3) the ids that has missing data
}
\description{
Evaluates the imported patients' data for the STOPP C8 criterion.
}
\author{
Agapios Panos <panosagapios@gmail.com>
}
