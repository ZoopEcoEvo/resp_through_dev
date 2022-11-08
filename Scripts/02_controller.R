# Load in required packages
library(rmarkdown)
library(tidyverse)
library()

#Determine which scripts should be run
process_data = F #Runs data analysis 
make_report = F #Runs project summary
knit_manuscript = F #Compiles manuscript draft

############################
### Read in the RAW data ###
############################

if(process_data = T){
  source(file = "Scripts/01_data_processing.R")
}

##################################
### Read in the PROCESSED data ###
##################################

if(make_report == T){
  render(input = "Output/Reports/report.Rmd", #Input the path to your .Rmd file here
         output_file = "report.html", #Name your file here; as it is, this line will create reports named with the date
         output_format = "all")
}

##################################
### Read in the PROCESSED data ###
##################################

if(knit_manuscript == T){
  render(input = "Manuscript/manuscript_name.Rmd", #Input the path to your .Rmd file here
         output_file = paste("draft_", Sys.Date(), ".pdf", sep = ""), #Name your file here; as it is, this line will create reports named with the date
         output_dir = "Output/Drafts/", #Set the path to the desired output directory here
         output_format = "all")
}
