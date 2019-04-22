We created a script in R as part of our final project for the Auburn University course Scripting for Biologists. This script will allow you to download the occurrence data for a taxon and use climactic models to estimate current niche and predict how these will change in the future as a result of climate change. 

# Updates
For this project, you will run the main script: `Predicting_fundamental_niche_locations.R`


Please note: in its current state, this script will only work for species distributions within the continental US. It is currently configuered to prompt the user for a binomial species name, but this can be altered to encompass other taxonomic levels, such as genus.

# Installation

To install the latest version directly from GitHub, go to:
<https://github.com/birdoptera/S4B_Final_Project>

## Additional Downloads and Instructions

Before running this script, create a temporary working folder and set it as the working directory, i.e. 

`mkdir /scratch/USER_temp` 

This script will require the manual download of a data files before running the script.

Go to <https://nrcs.app.box.com/v/soils/folder/53525984812> and download the file soil_color_CONUS_v2.zip. 

Then, move the file "CONUS_brigh.tif" into your working directory.


## Dependencies
This script has several R-package dependencies that need to be installed. Please install them before running the script, using:

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

We are sourcing our climate projection data from [CMIP5](https://cmip.llnl.gov/cmip5/), the Coupled Model Intercomparison Project. This specifically uses the GFDL data set from NOAA, with the representative concentration pathway (abrieviated rcp, it indicates how severe the climate change is expected to be based on the volume of greenhouse gases emitted). The most extreme projection is 85, with the year set to 50. There are other models, rcps, and years available. The options are given in the documentation for 'raster' and can be altered within the script to suit your purposes.


# Testing

In the Alabama Supercomputer system, to load R, type the command:
`module load R/3.5.1`

You can then launch R using the command:`R`

Before beginning testing, install the above dependencies in R 3.5.1. Then run the script in R `Rscript Predicting_fundamental_niche_locations.R`. There will be two prompts asking for the genus and species, respectively, of the species you wish to examine. For this test, use "Myzus" and "persicae", a species with good collection data.(See below in usage for more information)

After each step and package there should have readout indicating which step has completed to allow for easier alterations to the script to help customization.

The first step in this script is gathering species occurrence data from iDigBio and GBIF. We recommend testing that this data is sufficient for your purposes before running the full script.

Next niche information (such as soil type and landuse) is downloaded for the US. Again, this script is specifically for the US since much of this data is segmented by country. This information is then converted to a raster files (which are functionally maps that are saved by coded pixels) for further use.

The script then sets specifications and downloads data for these climate variables before modifying them to be consistent with each other:

solar radiation data (source: National Renewable Energy Laboratory)

soil type raster (source: NRCS, STATSTOGO)

Climate predictions are being used through WorldClim for present data and CMIP5 for future predictions, since they use the same variables and can be used to make comparisons.

There will be two additional prompts as the script runs, the first asks for how many years out the CMIP5 prediction should be. The second will ask you to specify an rcp (which is a measure of how extreme the climate change should be based on differing levels of greenhouse gases emitted, for further information, see the CMIP5 website).

In our examples, we select 70 years in the future and a rcp of 85, the most extreme example.

After the data is downloaded, the script then estimates current niche using distribution and current climate and uses this information to predict the future areas this species will be found. The final results are two models specifying the distribution of the target species, here _Myzus persicae_.

There will be two plots showing the present and future estimated distribution. There will be a third plot that highlights the differences between the two plots.

## Usage

Run the script in R:

`Rscript Predicting_fundamental_niche_locations.R`

Write in the prompt the genus and species you are interested in:

`Myzus persicae` (to test the code is working as expected).

