# Load in required packages
library(rmarkdown)
library(tidyverse)
library(readxl)

#Determine which scripts should be run
process_data = F #Runs data analysis 
make_report = T #Runs project summary

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
         output_file = "report.html", #Name your file here; as it is, this line will create reports named with the date
         output_format = "github_document")
}
