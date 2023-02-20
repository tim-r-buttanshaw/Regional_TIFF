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
**80 - 88** a function to find the 3 itl1 regions which had the highest value for a particular account item (can be input or output) in the account year
**90 - 104** using the top three itl1 regions found above, create a table with them across the top and years going down for the last 6 years (including account year)

#### lines 106 - 118
a function to create a tibble for a specific account item over the past 2 years for each itl1 region. This can then be converted using kable into a table which gov.uk can make into an interactive barchart i.e. figures 1.1 and 1.2 here: https://www.gov.uk/government/statistics/total-income-from-farming-for-the-regions-of-england/total-income-from-farming-in-the-regions-of-england-in-2021
