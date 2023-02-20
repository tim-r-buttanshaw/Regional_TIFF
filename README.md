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

## Rmd files
### TIFF in the regions of England.Rmd
#### YAML header lines 1 - 22
**title** gives the title in the preview - this has to be specifiecd when sending .txt file to publishing <br>
**date** shows the date of publication in the preview, just set for the system date currently <br>
**output** controls the output format <br>
**params** here you can set what paramteres you want to provide before *kntting the document* (runnning the script and making an output) <br>
**working_year** this is the account year that is being worked on, usualy *current_year - 1* but it means you can run the account for any year providing you have data 

* label: what you see when you choose the **working_year** before knitting
* value: value by default if no option is chosen, set at 2021 because that is the accounts year that this script was made for initially
* input: *slider* but this can be changed to *select* or a manual input
* min/max/step: minimum/maximum/step size value for the slider

**pubdate** this is the publication date of the document, used for appending file saves on final runs

* label: what you see when you choose the **pubdate** before knitting
* input: *date* this brings up a calendar interface to select a date before knitting
* value: this is system date by default, should be set to the scheduled publication date when running to produce final outputs

**run** this chooses the type of run of the document you are doing, generally this will be a development run except to produce the files to send to publishing

* label: what you see when you choose the **run** before knitting
* input: *select* this gives a dropdown list to choose options from
* value: set to development by default as this doesn't produce all the extra files needed for publication 
* choices: this gives the choices for the dropdown menu when using select

## Scripts
### prep_data_regional.R
#### lines 9-28
import any packages I will need as this is the first script the Rmd runs

#### lines 30 - 44
import the raw data and make a new clean set of the data with:

* column names snake_cased and renamed column names
* itl(formaerly nuts) codes snake_cased
* the relevant columns selected
* data in long format sorted by year

#### lines 46 - 75
make a lookup table to be used in a function to lookup the account item as it appears in the final data set and stats release to convert the raw data names

#### lines 76 - 81
make a variable of the latest colours to mach gov.uk and meet accessibility requirements

#### lines 83 - 122
create the data sets filteres by itl1 region i.e. north east, north west etc.

#### lines 124 - 136
import the raw data and make a new clean set of the data same as above but for the data per hectare

### functions_regional.R
#### lines 9 - 18
create a table for both itl1 and itl2 codes with the code in the code column and the publication name in the other column

#### lines 20 - 65
create a set of lookup functions to be run on data sets which have a column of itl1, itl2 or account item codes to be swapped to the pubication name
(generic lookup and replace function lines 67 - 77 but it's not really relevant just potentially useful to create other specific ones and too many variables)

#### lines 80 - 104
**80 - 88** a function to find the 3 itl1 regions which had the highest value for a particular account item (can be input or output) in the account year <br>
**90 - 104** using the top three itl1 regions found above, create a table with them across the top and years going down for the last 6 years (including account year) i.e. figures 2.2 and 2.3 [here](https://www.gov.uk/government/statistics/total-income-from-farming-for-the-regions-of-england/total-income-from-farming-in-the-regions-of-england-in-2021)

#### lines 106 - 118
a function to create a tibble for a specific account item over the past 2 years for each itl1 region. This can then be converted using kable into a table which gov.uk can make into an interactive barchart i.e. figures 1.1 and 1.2 [here](https://www.gov.uk/government/statistics/total-income-from-farming-for-the-regions-of-england/total-income-from-farming-in-the-regions-of-england-in-2021)

#### lines 121 - 131
a function which pivots a tibble so years are across the top, the items are arranged from largest to smallest, and the values are all rounded to 0dp

#### lines 134 - 144
a collection of functions which are always performed on a dataset when the resolution of the data in question is itl1 to produce tables such as figures 1.1 and 1.2 [here](https://www.gov.uk/government/statistics/total-income-from-farming-for-the-regions-of-england/total-income-from-farming-in-the-regions-of-england-in-2021)

#### lines 146 - 156
a collection of functions which are always performed on a dataset when the resolution of the data is itl2 to produce tables such as figures 2.4 and 2.6 [here](https://www.gov.uk/government/statistics/total-income-from-farming-for-the-regions-of-england/total-income-from-farming-in-the-north-east-of-england)

#### lines 158 - 178
a function which can be used for formatting of kables to strikethrough specified rows or make them bold or italics
