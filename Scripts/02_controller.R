# Load in required packages
library(rmarkdown)
library(tidyverse)
library(readxl)
library(nlme)
library(ggpubr)

#Determine which scripts should be run
process_data = F #Runs data analysis 
make_report = F #Runs project summary
knit_manuscript = T
############################
### Read in the RAW data ###
############################

if(process_data == T){
  source(file = "Scripts/01_data_processing.R")
}

##################################
### Read in the PROCESSED data ###
##################################
#Read in nauplii survival data 
mort_data = read_excel(path = "Data/Nauplii/Survivorship/July_nauplii_survivorship.xlsx")
o2_record = read.csv(file = "Output/Data/o2_record.csv")
full_dataset = read.csv(file = "Output/Data/full_dataset.csv")
naup_resp_rates = read.csv(file = "Output/Data/naup_resp_rates.csv")
molt_record = read.csv(file = "Output/Data/molt_record.csv")
#Reads in survival data 
surv_data = read_xlsx("Data/survival_test_corr.xlsx")%>%
  filter(group == "active")


if(make_report == T){
  render(input = "Output/Reports/report.Rmd", #Input the path to your .Rmd file here
         output_file = "report", #Name your file here; as it is, this line will create reports named with the date
         output_format = "all")
}

if(knit_manuscript == T){
  render(input = "Manuscript/Holmes_Hackerd_etal_2023.Rmd", #Input the path to your .Rmd file here
         output_file = paste("dev_draft_", Sys.Date(), sep = ""), #Name your file here; as it is, this line will create reports named with the date
         #NOTE: Any file with the dev_ prefix in the Drafts directory will be ignored. Remove "dev_" if you want to include draft files in the GitHub repo
         output_dir = "Output/Drafts/", #Set the path to the desired output directory here
         output_format = "all",
         clean = T)
}
