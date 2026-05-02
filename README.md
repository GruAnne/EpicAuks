# README


# The "EPIC approach" for investigating habitat loss in and around wind parks 

**** USE OF ANY OF THESE CONTENTS ON THIS GITHUB REPOSITORY SHOULD BE REFERRED TO AS: ****

Anne Grundlehner, Mardik F. Leopold, Anna Kersten,
This is EPIC: Extensive Periphery for Impact and Control to study seabird habitat loss in and around offshore wind farms combining a peripheral control area and Bayesian statistics,
Ecological Informatics,
Volume 85, 2025, 102981, ISSN 1574-9541,
https://doi.org/10.1016/j.ecoinf.2024.102981.

The data and R code in this repository was used in the statistical modelling for  "This is EPIC: Extensive Periphery for Impact and Control to study seabird habitat loss in and around offshore wind farms combining a peripheral control area and Bayesian statistics" published in Ecological Informatics in 2025 (10.1016/j.ecoinf.2024.102981)

The goal of the work is to investigate the presence habitat loss in two auk species around the offshore wind park in the Dutch North Sea. 

The data consists of bird sightings from aerial surveys in and around an offshore wind park. With the R scripts we create a Bayesian spatial temporal model based on the sightings data, which is then used for a simulation study. For all details of statistical models, interpretation and rationale of modelling procedures, please refer to our article: https://doi.org/10.1016/j.ecoinf.2024.102981 


Commonly used shortenings 

RB = Razorbills (Focal bird species)

GM = Guillemots (Focal bird species)

OWF = Offshore Wind Farm (similar to OWP, used interchangeably, Offshore Wind Park)

Gemini = Name of OWP

Buitengaats (BG): right  turbine field area of the Gemini OWP twinpark

ZeeEnergie (ZE): left turbine field area of the Gemini OWP twin park



*NOTE: GEMINI IS A TWIN PARK* 

The Gemini wind park is a so-called "Twin park", consisting of 2 seperate wind park areas (called Buitengaats and ZeeEnergie) of similar size (32-33 km2) and a similar space that seperates the two areas. In the scripts, some parts of the analyses is done for both parks seperately as well as for the parks combined.  When applying the EPIC approach to a singular wind park, this can be adjusted by removing repetitive code applied to "Area1" to "Area4" or "ZeeEnergie" /"Buitengaats", etc. This is indicated in the code too.

# List of Scripts
File: "INLA_ZAG_GM_FullPipeline_clean.R"

Description: This script contains the computation of the Spatial-Temporal ZAG-GAM in R-INLA, for eight surveys, as well as the simulations where we sample random areas around the OWP to compare bird densities in and around the OWP, in order to quantify the habitat loss effect (for details, see Methods).

*NOTE*

Only the ZAG-GAM script for RB is supplied in this repository. Scripts for GM and AUK are identical, to run the models for these other bird groups, the only required adjustment is selection of a different column (GM; AUK) from the imported dataset at the top of the script and replacement of "RB" with "GM" or "AUK" when saving files.


File: "Visualize_OWPEffect.R"

Description: Here, the output of the simulations is used to visualize bird densities in and around the OWP, using the model output and simulations.

File: "OWP_Effect_BoundaryAreas_sep.R"

Description: This script has code to study the effect of the boundary areas, using the model output and simulations.



# List of data files

File: "GeminiDataForModelling_densities.csv"

Description: Survey data with auk abundances. Each row represents one observation. One observation is one photograph taken during one focal survey. One focal survey is performed during one continuous airplane flight. 

code	= Survey code

Transect	= Numeric Survey ID (Here, we had eight surveys, it ranges 1 to 8)

YEAR	= Calender year of survey

MONTH	= Calendar month of survey

Lat	= Latitude (WSG84) of observation (centre of photo)

Long	= Longitude (WSG84) of observation (centre of photo)

Area = surface area (km2) of photo analyzed, used for conversion to bird densities

Xkm	= Position on x axis in km

Ykm	= Position on y axis in km

RB	= Razorbill densities observed (n/100km)

GM	= Guillemot densities observed (n/100km)

AUK	= All auks (Guillemots + Razorbills + unidentified auks) densities observed (n/100km)

Guillemot.n	= Raw count of guillemots

Razorbill.n	= Raw count of razorbills

Auks.n = Raw count of all auks (Guillemots + Razorbills + unidentified auks)


File: "MyStudyShape.shp"

Description: Shapefile with outline of the study area


File: "windfarmspolyPolygon.shp"

Description: Shapefile with polygon of windpark outline


File: "Shipping lane north_region.shp"

Description: Shapefile with Northern shipping lane near the study area (see Figure 1 manuscript)


File:"Shipping lane south_region.shp"

Description: Shapefile with Southern shipping lane near the study area (see Figure 1 manuscript)


