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
### prep_data_regional
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
