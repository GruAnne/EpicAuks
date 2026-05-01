# EpicAuks

# The "EPIC approach" for investigating habitat loss in and around wind parks 

The data and R code in this repository was used in the statistical modelling for  "This is EPIC: Extensive Periphery for Impact and Control to study seabird habitat loss in and around offshore wind farms combining a peripheral control area and Bayesian statistics" published in Ecological Informatics in 2025 (10.1016/j.ecoinf.2024.102981)

**** USE OF ANY OF THESE CONTENTS ON THIS GITHUB REPOSITORY SHOULD BE REFERRED TO AS: ****
Anne Grundlehner, Mardik F. Leopold, Anna Kersten,
This is EPIC: Extensive Periphery for Impact and Control to study seabird habitat loss in and around offshore wind farms combining a peripheral control area and Bayesian statistics,
Ecological Informatics,
Volume 85, 2025, 102981, ISSN 1574-9541,
https://doi.org/10.1016/j.ecoinf.2024.102981.

The goal of the work is to investigate the presence habitat loss in two auk species around the offshore wind park in the Dutch North Sea. 

The data consists of bird sightings from aerial surveys in and around an offshore wind park. With the R scripts we create a Bayesian spatial temporal model based on the sightings data, which is then used for a simulation study. For all details of statistical models, interpretation and rationale of modelling procedures, please refer to our article: https://doi.org/10.1016/j.ecoinf.2024.102981 


# GEMINI IS A TWIN PARK
The Gemini wind park is a so-called "Twin park", consisting of 2 seperate wind park areas (called Buitengaats and ZeeEnergie) of similar size (32-33 km2) and a similar space that seperates the two areas. In the scripts, some parts of the analyses is done for both parks seperately as well as for the parks combined.  When applying the EPIC approach to a singular wind park, this can be adjusted by removing repetitive code applied to "Area1" to "Area4" or "ZeeEnergie" /"Buitengaats", etc. This is indicated in the code too.


# REPOSITORY
Commonly used shortenings in the code:
RB = Razorbill
GM = Guillemot
OWF = Offshore Wind Farm (similar to OWP, Offshore Wind Park)
Gemini = Name of OWP

# List of data files
File:
Description:


# List of Scripts
File:
Description:
*NOTE*
Only the ZAG-GAM script for RB is supplied in this repository. Scripts for GM and AUK are identical, to run the models for these other bird groups, the only required adjustment is selection of a different column (GM; AUK) from the imported dataset at the top of the script and replacement of "RB" with "GM" or "AUK" when saving files.

File: 
Description:

File:
Description:



