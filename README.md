StartStopp R Package
================

[![Build Status](https://travis-ci.com/agapiospanos/StartStopp.svg?branch=master)](https://travis-ci.com/agapiospanos/StartStopp) <img src="man/figures/start-stopp-logo.png" width=400 align="right" style="margin-left:20px; margin-right: 20px;"/>

Description
-----------

A package that provides patient data evaluation algorithm for the START and STOPP criteria. The patient data are parsed and their ids are classified based on whether the meet the conditions or not for each START and STOPP criterion.

The package exports excel files that contains the patient ids and their status (1: if they fulfill the conditions for the criterion, 0: if they do not fulfill the conditions for the criterion and 2: if they have missing data). Furthermore if there are missing data on the input data the excel file specifies the column name of the input excel file that contains missing data.

Till now the following START and STOPP criteria have been incorporated in the package.

START

-   START A1
-   START E2

STOPP

-   STOPP B1
-   STOPP B2
-   STOPP B3
-   STOPP B4
-   STOPP B5
-   STOPP B6
-   STOPP B7
-   STOPP B8
-   STOPP B9
-   STOPP B10
-   STOPP B13
-   STOPP C1
-   STOPP C2
-   STOPP C3
-   STOPP C7
-   STOPP C10
-   STOPP C11
-   STOPP D1
-   STOPP D2
-   STOPP D3
-   STOPP D5
-   STOPP D6
-   STOPP D7
-   STOPP D9
-   STOPP D10
-   STOPP D11
-   STOPP D14
-   STOPP E1
-   STOPP E2
-   STOPP E3
-   STOPP E4
-   STOPP E5
-   STOPP E6
-   STOPP F1
-   STOPP F4
-   STOPP G1
-   STOPP G3
-   STOPP G4
-   STOPP H1
-   STOPP H2
-   STOPP H3
-   STOPP H4
-   STOPP H5
-   STOPP H6
-   STOPP H7
-   STOPP H9
-   STOPP I1
-   STOPP J2
-   STOPP J4
-   STOPP J6
-   STOPP K1
-   STOPP K2
-   STOPP K3
-   STOPP K4
-   STOPP M1

Installation
------------

The package is still under active development and has not been submitted to the CRAN yet.

You can install the **StartStopp** package from GitHub repository as follows:

Installation using R package **[devtools](https://cran.r-project.org/package=devtools)** (with [Rtools](https://cran.r-project.org/bin/windows/Rtools/) for Windows):

``` r
install.packages("devtools")
devtools::install_github("agapiospanos/StartStopp")
```

Input data format
-----------------

I will soon provide an excel file as a template for the input data format.

Usage example
-------------

Provided that you have the input data in the format specifed above you can use the package by calling any of the criteria functions. For example to evaluate the data for the STOPP B6 criterion you can call the following function.

``` r
# this function will import the patient data from the file specified in path argument 
# and then export an excel file that contains the evaluated data at the working directory
STOPP_B6(path = "~/patient_data.xlsx")

# you can also specify the export data path by including the argument export_data_path
STOPP_B6(path = "~/patient_data.xlsx", export_data_path="~/Desktop")

# if you do not wish to have an excel file exported and you prefer to have the data assigned 
# to a variable you can also omit the export by using the excel_out = FALSE
output <- STOPP_B6(path = "~/patient_data.xlsx", excel_out = FALSE)

# you can also run all the STOPP criteria at once. Note that this will generate many excel files in
# the specified destination path
STOPPall(path = "~/patient_data.xlsx", export_data_path="~/Desktop")

# similarly you can also run all the START criteria at once
STARTall(path = "~/patient_data.xlsx", export_data_path="~/Desktop")
```
