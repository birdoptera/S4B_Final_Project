We created a script in R as part of our final project for the S4B.
This script will allow you to download the occurence data for a taxa
and use climactic models to estimate current niche and predict how these will change in the future as a result of climate change. 

Please note: in its current state, this script will only work for species distributions within the continental US.

# Installation

To install the latest version directly from GitHub, go to:
<https://github.com/birdoptera/S4B_Final_Project>

## Additional Downloads and Instructions

Before running this script, create a temporary working folder and set it as the working directory .i.e. 

`mkdir /scratch/USER_temp` 

This script will require the manual download of a data files before running the script.

Go to <https://nrcs.app.box.com/v/soils/folder/53525984812> and download the file soil_color_CONUS_v2.zip. 

Then, move the file "CONUS_brigh.tif" into your working directory.


## Dependencies
This script has several R-package dependencies that need to be installed. Please install them before running the scrip, using:

```
install.packages("ridigbio")

install.packages("rgbif")

install.packages("raster")

install.packages("rgdal")

install.packages("dismo")
```


The file "CONUS_brigh.tif" should be in the GitHub files and download with the code. This is a color map of soil in the US. 
Therefore, this script is only applicable in its current state to species (and their niches) within the US.

## Default Models and Climate Data

We're sourcing our climate projection data from CMIP5, the Coupled Model Intercomparison Project
This specifically uses the GFDL data set from NOAA, with the representative concentration pathway (rcp) (how
severe the climate change is expected to be based on how much greenhouse gases are emitted.
85 is the most extreme projection), with the year set to 50. There are other models, rcp, and year available,
The options are given in the documentation for 'raster' and can be altered within the script to suit your purposes.


# Testing

Before beginning testing, install the above dependencies in R 3.5.1. Then run the script in R. There will be a prompt
asking for the genus and species of the species you wish to examine. For this test, use Myzus persicae, a species with 
good collection data.

After each step and package there should have readout indicating which step has completed to allow for easier alterations 
to the script to help customization.

The first step in this script is gathering species occurence data from iDigBio and GBIF. We recommend testing that this data
is sufficient for your purposes before running the full script.

Next niche information (such as soil type and landuse) is downloaded for the US. Again, this script is specifically for the US 
since much of this data is segmented by country. This information is then converted to a raster files (which are functionally maps
that are saved by coded pixels) for further use.

The script then sets specifications and downloads data for these climate variables before modifying them to be consistent
with each other:

solar radiation data (source: National Renewable Energy Laboratory)

soil type raster from NRCS, STATSTOGO.

Climate predictions are being used through WorldClim for present data and CMIP5 for future predictions,
since they use the same variables and can be used to make comparisons.

## Usage

Run the script in R:

`./S4B_life_new_distrubution_due_to_climatic_change.R`

Write in the prompt the genus and species you are interested in:

`Myzuz persicae` #Species with a good collection data

