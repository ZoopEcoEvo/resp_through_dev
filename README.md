# Naupliar exposure to acute warming does not affect ontogenetic patterns in respiration, size, or development time in the cosmopolitan copepod *Acartia tonsa* 

Mathew Holmes-Hackerd<sup>1</sup>, M.C. Sasaki<sup>1,2</sup>, Hans G. Dam<sup>1</sup> 

1. University of Connecticut, Department of Marine Sciences  
2. University of Vermont, Department of Biology  

This project tested the effects of naupliar heat stress on respiration through development in the copepod *Acartia tonsa*. Copepods were collected from Eastern Long Island Sound during the summer of 2020. 

Data archived: [![DOI](https://zenodo.org/badge/563165816.svg)](https://zenodo.org/badge/latestdoi/563165816)
Published manuscript: https://doi.org/10.1371/journal.pone.0282380 

## Directory Structure 
The root directory contains the README and Licensing files, along with a .Rproj file and five sub-directories: Data, Manuscript, Output, Randomized_Setup, and Scripts.  

-   `Data/` contains the raw data used in this analysis.  

-   `Manuscript/` contains the R Markdown file, templates, and bibliography used to produce the manuscript draft. 

-   `Output/` contains the various products of the project (processed data, figures, knit reports, and a PDF copy of the manuscript. Note, the `Reports/` directory contains the R Markdown file used to generate the figures used in the manuscript.  

-   `Randomized_Setup` contains the script resulting files describing the randomized experimental set ups.  

-   `Scripts/` contains two R scripts. 
    -   `01_Data_analysis.R` is used to process the raw data. The main component of this processing is converting raw oxygen concentration readings into respiration rates, standardized against control measurements.  
    -   `02_make_report.R` is use to control the project workflow. Through this script, you can choose to run the process the data, make the figures, or knit the manuscript. This script should be used rather than running isolated fragments individually. 


## Data Description 

The `Data/` directory contains four directories, one for each type of data analyzed in the project:  

-   `Body_Size` contains the prosome measurements from each replicate experiment. Each file name includes the experimental replicate number (R1, R2, or R3) and the date of the measurements in YYYY_MM_DD format. Data files include the following columns:  
    -   *id* - The individual's unique ID. Allows body sizes to be followed through development.
    -   *vial* - The vessel ID from the respiration experiment. Allows body sizes to be matched to respiration rates.
    -   *stage*	- The individual's stage at the time of measurement. Recorded as c1-c6 (copepodite 1, etc.)
    -   *body_length * - The prosome length of the individual, measured in mm.  

-   `Molts` contains the daily observations of individual copepod stages. There is one file per experimental replicate. Data files include the following columns:   
    -   *id* - The individual's unique ID. 	  
    -   *treatment*	- The treatment individuals were exposed to as nauplii - either 'control' or 'heatstress'.    
    -   *group* - Whether the individual was initially assigned to the actively observed group, or the back-up/handling control group.   
    -   *source_cup* - Which of the initial cups (n = 6, three per treatment) were the individuals picked from.   
    -   *sex* - The sex of the individual. This was identified when an individual reached the final copepodite stage (C6).   
    -   *M_D_YYYY* - After the initial columns, there are a series of columns containing the observed stage on each of the experimental days. Transition between stages was tracked primarily by the presence of molts. Mortality is indicated by an 'x'.    

-   `Nauplii` contains the information for preliminary experiments on naupliar thermal performance. Contains three sub-directories:  
    -   `Body_Size` - As described above, contains data on body sizes of nauplii used in preliminary respiration experiments. Three nauplii were measured per vial.
    -   `Respiration` - Contains the raw oxygen readings from the Presens sensor dish reader for the naupliar respiration rate experiments. See the next section for additional details on these file types. 
    -   `Survivorship` - Contains the data on naupliar survivorship across a range of temeperatures. This is used to estimate thermal tolerance (as LD50). Individual survivorship was recorded for nauplii after a 24-hour exposure to a range of stress temperatures. 
    
-   `Respiration` contains the data used to calculate copepodite respiration rates. Files are essentially unaltered output from the Presens sensor dish reader, with two important exceptions. For ease of use in R, header rows were deleted, leaving just the columns of data. Further, each vial column used in the calculations was modified to include either 'Control' or 'Test' in the column name before the vial id. This allowed the code to correctly identify which vials should be used to estimate background respiration rates. Each file name includes the experimental replicate number (R1, R2, or R3) and the date of the measurements in YYYY_MM_DD format. Data files include the following columns:   
    -   *Date/Time* - The date and time information recorded by the sensor dish reader. Not used in the analyses. 	  
    -   *Time/Min.*	- Records the time points of each oxygen measurement in minutes.      
    -   *Vial Columns* - Oxygen readings for each individual vial are recorded in their own columns. Vials used as controls (did not contain a copepod) are indicated with a 'Control' prefix before the vial ID. Vials that did contain a copepod are indicated with a 'Test' prefix.     

  
## Workflow

The workflow is operated via the 02_Make_report.R script in the Scripts directory. It is not recommended that you run analyses or knit documents from the files themselves as the file paths are internally set and may not be correct otherwise. At the top of this script, you are able to indicate whether:

1. The raw data should be processed to calculate respiration rates.  

2. The summary file (located in the Output/Reports directory) should be knit. This markdown file will generate the figures used in the manuscript, as well as an HTML and a GitHub flavored markdown document.

3. The manuscript file (located in the Manuscripts directory) should be knit. This markdown file will produce formatted PDF and word document versions of the manuscript. 


## Funding

This study was funded by a grant from the National Science Foundation (OCE-1947965).  
