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
-   START A2
-   START A3
-   START A4
-   START A5
-   START A6
-   START A7
-   START A8
-   START B1
-   START B2
-   START B3
-   START C1
-   START C2
-   START C3
-   START C4
-   START C5
-   START C6
-   START D1
-   START D2
-   START E1
-   START E2
-   START E3
-   START E4
-   START E5
-   START E6
-   START E7
-   START F1
-   START G1
-   START G2
-   START G3
-   START H1
-   START H2
-   START I2

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
-   STOPP B11
-   STOPP B12
-   STOPP B13
-   STOPP C1
-   STOPP C2
-   STOPP C3
-   STOPP C4
-   STOPP C5
-   STOPP C6
-   STOPP C7
-   STOPP C8
-   STOPP C9
-   STOPP C10
-   STOPP C11
-   STOPP D1
-   STOPP D2
-   STOPP D3
-   STOPP D4
-   STOPP D5
-   STOPP D6
-   STOPP D7
-   STOPP D8
-   STOPP D9
-   STOPP D10
-   STOPP D11
-   STOPP D12
-   STOPP D13
-   STOPP D14
-   STOPP E1
-   STOPP E2
-   STOPP E3
-   STOPP E4
-   STOPP E5
-   STOPP E6
-   STOPP F1
-   STOPP F2
-   STOPP F3
-   STOPP F4
-   STOPP G1
-   STOPP G2
-   STOPP G3
-   STOPP G4
-   STOPP H1
-   STOPP H2
-   STOPP H3
-   STOPP H4
-   STOPP H5
-   STOPP H6
-   STOPP H7
-   STOPP H8
-   STOPP H9
-   STOPP I1
-   STOPP I2
-   STOPP J1
-   STOPP J2
-   STOPP J3
-   STOPP J4
-   STOPP J5
-   STOPP J6
-   STOPP K1
-   STOPP K2
-   STOPP K3
-   STOPP K4
-   STOPP L1
-   STOPP L2
-   STOPP M1

Installation
------------

The package is still under active development and has not been submitted to the CRAN yet.

You can install the **StartStopp** package from GitHub repository as follows:

Installation using R package **[devtools](https://cran.r-project.org/package=devtools)**:

``` r
install.packages("devtools")
devtools::install_github("agapiospanos/StartStopp")
```

Input data format
-----------------

I will soon provide an excel file as a template for the input data format.

Basic usage examples
--------------------

Provided that you have the input data in the format specifed above you can use the package by calling any of the criteria functions. For example to evaluate the data for the STOPP B6 criterion you can call the following function. Calling this function, a popup window will appear to choose the excel file that contains the patient data. Then another popup window will be displayed to choose a folder that the excel file will be exported.

``` r
STOPP_B6()
```

You can also get a single excel file for all the STOPP or START criteria using the commands below:

``` r
STARTall()
STOPPall()
```

Advanced usage examples
-----------------------

You can exclude some criteria from running when calling STARTall() and STOPPall() functions by using the exclude argument. Use a vector in the form shown below to specify the criteria you want to exclude. NOTE: You have to use capital letters in the vector for the exclude argument as shown below.

``` r
STOPPall(exclude = c("B12", "G4"))
```

You can also make the STARTall() and STOPPall() functions export single excel files for each criterion. Just add the single\_excel = FALSE argument as shown below:

``` r
STOPPall(single_excel = FALSE)
```

You can avoid all the popup windows for choosing the export path and the excel file to read data from. Just specify them using the below arguments. This arguments work for a single criterion as well as for the STARTall() and STOPPall() functions.

``` r
STOPP_B6(path = "~/patient_data.xlsx", export_data_path="~/Desktop")
STOPPall(path = "~/patient_data.xlsx", export_data_path="~/Desktop")
STARTall(path = "~/patient_data.xlsx", export_data_path="~/Desktop")
```

You can evaluate multiple START or STOPP criteria at once using the STARTselected and STOPPselected criteria. All the arguments of STARTall and STOPPall function also apply to the STARTselected and STOPPselected. You must provide the selected START and STOPP criteria in a vector form using the selected argument. NOTE: You have to use capital letters in the vector for the selected argument as shown below.

``` r
STOPPselected(selected=c("B13", "B1", "M1"))
STARTselected(selected=c("A2", "E5", "E7"))
```
