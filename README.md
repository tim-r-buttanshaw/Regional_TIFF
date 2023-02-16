# Regional_TIFF
Code used to create the publication Total Income from Farming in the Regions of England 

## Layout of this repo
There are 2 folders in this repo:

* main_page

* individual_regions

These are there to distinguish between the code which is for the summary landing page for the regional TIFF account and the code which is for creating the individual regional code. The files which aren't in either foler represent the files which are needed for both folders to run: 

* imorting and cleaning the data
* creating the numerous specific functions used in order to try and reduce repetition and errors in the code

To create the regional accounts if the data is in the fomrat you expect then all you need to do it to run the .Rmd files in each of the folders (making sure that the source references for the scripts are correct). I will outline below what is happening in each of the scripts and where they fit in the process of running the regional accounts.

The approximate data flow is like this:

1. import the data from Access with the _prep_data_regional.R_ script
* this is where the majority of the changes will need to be made if the raw data format is changed
* this will not run without a pre-set _working_year_ or _previous_year_ variable 
2. run the _functions_regional.R_ script to create the functions used elsewhere in the regional accounts
* note that some of these functions become relyant on other functions in this file so be sure to understand what each of them is doing if producing the accounts for the first time
