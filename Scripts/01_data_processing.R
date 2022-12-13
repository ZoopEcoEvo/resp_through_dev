library(readxl)
library(ggpubr)
library(ggExtra)
library(respR)
library(car)
library(survival)
library(survminer)
library(emmeans)
library(MASS)
library(tidyverse)

#### Respiration Analysis ####
#Creates list of files containing oxygen data
file_list_resp = dir("Data/Respiration")
file_list_bs = dir("Data/Body_size")
file_list_molts = dir("Data/Molts")

#Creates empty data frame to store data after processing
resp_rates = data.frame()
o2_record = data.frame()
bg_resp_rates = data.frame()
molt_record = data.frame()

#Start of loop through respiration folder
for(i in 1:length(file_list_resp)){
  
  #unlist and store meta data from file name 
  meta = unlist(str_split(file_list_resp[i], pattern = "_"))
  
  #meta data from file names
  replicate = meta[1]
  month = meta[2]
  day = meta[3]
  
  #records date of measurement
  daily_track = paste(replicate, month, day, sep = "_")
  
  #Will be used later to match data files for different traits
  file_name = unlist(str_split(file_list_resp[i], pattern = "_Oxygen"))[1]
  
  #Reads in oxygen data
  Oxygen_Data = read_excel(path = paste("Data/Respiration", file_list_resp[i], sep = "/"))
  
  #Puts Oxygen excel file into DPLYR format
  Oxygen_Data = as_tibble(Oxygen_Data)
  
  #Removes acclimation time, only looks at >300 min portion of runs
  Oxygen_Data_acc = Oxygen_Data[Oxygen_Data$`Time/Min.` > 350 ,]
  
  #Gets rid of empty sensor slots
  O2_adj <- Oxygen_Data_acc %>% select_if(is.numeric)
  
  #Import body size measurements
  Body_Size_Data = read_excel(path = paste("Data/Body_Size", file_list_bs[i], sep = "/"))
  
  #Set control and test columns
  Control_cols = which(str_detect(colnames(O2_adj), pattern = "Control"))
  Exp_cols = which(str_detect(colnames(O2_adj), pattern = "Test"))
  
  
  #Calculates background rates of oxygen depletion
  bg = calc_rate.bg(O2_adj, time = 1, oxygen = Control_cols, plot = F)
  
  #Creates empty object to store background resp rate values
  bg_rates = c()
  bg_rates = c(bg_rates, bg$rate.bg)
  
  #Creates empty objects to store rate values
  rates = c()
  
  #start of loop going through individual vials (columns)
  for(j in Exp_cols){
    
    resp_rate = O2_adj %>%
      dplyr::select(`Time/Min.`, colnames(O2_adj)[j])%>%
      calc_rate(plot = F)
    
    rates = c(rates, resp_rate$rate)
    
  } #end of vial (column) loop
  
  #store vial and background vial names
  vial_names = colnames(O2_adj)[Exp_cols]
  bg_vial_names = colnames(O2_adj)[Control_cols]
  
  resp_data = data.frame("vial" = vial_names, 
                         "file" = i, #from the `for loop` used to cycle through all available oxygen data files
                         "replicate" = replicate,
                         "resp_rate" =  rates)
  
  bg_resp_data = data.frame("vial" = bg_vial_names, 
                            "file" = i, #from the `for loop` used to cycle through all available oxygen data files
                            "replicate" = replicate,
                            "bg_rate" = bg_rates)
  
  #Import body lengths (in mm)
  length_data = readxl::read_excel(path = paste("Data/Body_Size/", file_name, "_bodysize.xlsx", sep = "")) 
  
  #Adds body length data to this data set
  resp_data = inner_join(resp_data, length_data, by = "vial")
  
  #Creates replicate-specific id
  resp_data = resp_data%>%
    mutate("id" = paste(replicate, id, sep = "_"))
  
  #convert length in mm to um
  resp_data$length_um = (resp_data$body_length)*1000
  
  #Length-weight conversion from Berggreen et al 1988. inputs are length (um), output is body weight (ng C)
  resp_data$weight_ngC = 1.11 * 10^-5 * (resp_data$length_um)^2.92
  
  #convert weight from ngC to kgC necesary for convert_rate function below
  resp_data$weight_kgC = (resp_data$weight_ngC) / 1000000000000
  
  #Inputs should be in % Air. Sat, minutes, kilograms, liters
  #Output is mL Oxygen per hour per mg dry weight
  mass_conv = convert_rate(resp_data$resp_rate,
                           oxy.unit = "%Oxy",
                           time.unit = "m",
                           output.unit = "mL/h/mg",
                           mass = resp_data$weight_kgC,
                           volume = 0.0028,
                           S = O2_adj$`Salinity [g/1000g]`[1],
                           t = mean(O2_adj$`Tm [°C]`),
                           P = (O2_adj$`p [mbar]`[1]/1000))
  
  raw_conv = convert_rate(resp_data$resp_rate,
                          oxy.unit = "%Oxy",
                          time.unit = "m",
                          output.unit = "mL/h",
                          mass = NULL,
                          volume = 0.0028,
                          S = O2_adj$`Salinity [g/1000g]`[1],
                          t = mean(O2_adj$`Tm [°C]`),
                          P = (O2_adj$`p [mbar]`[1]/1000))
  
  
  #storage of respiration rates in resp_data df
  resp_data$mass_spec_resp = mass_conv$rate.output
  resp_data$resp_mL_h = raw_conv$rate.output
  resp_rates = bind_rows(resp_rates, resp_data)
  bg_resp_rates = bind_rows(bg_resp_rates, bg_resp_data)
  
  #storage of replicate and day of measurement to create replicate-specific ID
  O2_adj$rep = i
  O2_adj$day_id = daily_track
  o2_record = bind_rows(o2_record, O2_adj)
  
} #end of I (individual file) loop

#transforms all respiration measurments into positive values
resp_rates$resp_rate = resp_rates$resp_rate * -1
resp_rates$mass_spec_resp = resp_rates$mass_spec_resp * -1
resp_rates$resp_mL_h = resp_rates$resp_mL_h * -1

#start of molt tracking loop
for(i in 1:length(file_list_molts)){
  
  #grabs replicate data from file name
  meta = unlist(str_split(file_list_molts[i], pattern = "_"))
  
  replicate = meta[1]  
  
  #Read in molt data
  raw_molt_data = read_xlsx(path = paste("Data/Molts", file_list_molts[i], sep = "/"))
  
  #create replicate-specific unique ID
  raw_molt_data = raw_molt_data %>%
    mutate("id" = paste(replicate, id, sep = "_"))
  
  #cleans up molt data df for simplification of joining df's
  molt_data = raw_molt_data %>%
    pivot_longer(cols = c(-id,-treatment,-sex, -group, -source_cup),   
                 names_to = "date", 
                 values_to = "stage")%>% 
    
    mutate(date = str_replace_all(string = date, pattern = "_", replacement = "/"), #replaces all _'s with /'s
           date = parse_date(date, format = "%m/%d/%Y"),
           days = as.numeric(date-first(date)))   
  
  molt_record = bind_rows(molt_record, molt_data)
}

#join molt data w/ resp_rates data
full_dataset = inner_join(resp_rates,molt_record, by = c("id", "stage"))%>%
  group_by(id, stage)%>%
  filter(date == min(date))%>%
  ungroup(stage)%>%
  mutate("stage_duration" = lead(days) - days)



#### Nauplii Respiration/Size/Molt Analysis ####
#Creates list of files containing oxygen data
file_list_resp = dir("Data/Nauplii/Respiration")
file_list_bs = dir("Data/Nauplii/Body_Size")


#Creates empty data frame to store data after processing
naup_resp_rates = data.frame()
naup_o2_record = data.frame()
naup_bg_resp_rates = data.frame()

#Start of loop through respiration folder
for(i in 1:length(file_list_resp)){
  
  #unlist and store meta data from file name 
  meta = unlist(str_split(file_list_resp[i], pattern = "_"))
  
  #meta data from file names
  replicate = meta[1]
  naup = meta[2]
  temp = meta[3]
  
  #Will be used later to match data files for different traits
  file_name = unlist(str_split(file_list_resp[i], pattern = "_Oxygen"))[1]
  
  #Reads in oxygen data
  Oxygen_Data = read_excel(path = paste("Data/Nauplii/Respiration", file_list_resp[i], sep = "/"))
  
  #Puts Oxygen excel file into DPLYR format
  Oxygen_Data = as_tibble(Oxygen_Data)
  
  #Removes acclimation time, only looks at >300 min portion of runs
  Oxygen_Data_acc = Oxygen_Data[Oxygen_Data$`Time/Min.` > 350 ,]
  
  #Gets rid of empty sensor slots
  O2_adj <- Oxygen_Data_acc %>% select_if(is.numeric)
  
  O2_adj = na.omit(O2_adj)
  
  #Set control and test columns
  Control_cols = which(str_detect(colnames(O2_adj), pattern = "Control"))
  Exp_cols = which(str_detect(colnames(O2_adj), pattern = "Test"))
  
  #Calculates background rates of oxygen depletion
  #bg_insp = inspect(O2_adj, time = 1, oxygen = Exp_cols, plot = F)
  bg = calc_rate.bg(O2_adj, time = 1, oxygen = Control_cols, plot = F)
  
  #Creates empty object to store background resp rate values
  naup_bg_rates = c()
  naup_bg_rates = c(naup_bg_rates, bg$rate.bg)
  
  #Creates empty objects to store rate values
  naup_rates = c()
  
  #removes NAs from oxygen data
  drop_na(O2_adj)
  
  for(j in Exp_cols){
    
    resp_rate = O2_adj %>%
      dplyr::select(`Time/Min.`, colnames(O2_adj)[j]) %>%
      calc_rate(plot = F)
    
    naup_rates = c(naup_rates, resp_rate$rate)
  } #End J (column) loop

  vial_names = colnames(O2_adj)[Exp_cols]
  bg_vial_names = colnames(O2_adj)[Control_cols]
  
  naup_resp_data = data.frame("vial" = vial_names, 
                              "replicate" = replicate,
                              "resp_rate" =  naup_rates,
                              "temp" = temp)
  
  bg_resp_data = data.frame("vial" = bg_vial_names, 
                            "replicate" = replicate,
                            "bg_rate" = naup_bg_rates)
  
  #Import body lengths (currently in mm)
  length_data = readxl::read_excel(path = paste("Data/Nauplii/Body_Size/", file_name, "_bodysize.xlsx", sep = "")) 
  
  #unlist and store meta data from file name 
  bs_meta = unlist(str_split(file_list_bs[i], pattern = "_"))
  
  #meta data from file names
  replicate = bs_meta[1]
  temp = bs_meta[3]
  
  #gets the average length for this replicate & temp combination
  rep_avg_length = mean(length_data$length)
  
  naup_resp_data = naup_resp_data %>%
    mutate ("avg_length_mm" = rep_avg_length) 
  
  # Below are various unit manipulations to transform length data (in mm) to um as necesary for dry weight calculation
  naup_resp_data$avg_length_um = (naup_resp_data$avg_length_mm)*1000
  
  #length - ng C conversion from berggren et al 1988 (food size spectra)
  naup_resp_data$weight_ngC_B88 = 3.18 * 10^-6 * naup_resp_data$avg_length_um^3.31

  naup_resp_data$weight_c_kg_B88 = (naup_resp_data$weight_ngC_B88) / 1000000000000
  
  #divides resp_rate for each vial by 15, which is the number of nauplii in each vial. This transforms resp measurements into individuals, instead of bulk measurements
  naup_resp_data$resp_rate_corr = (naup_resp_data$resp_rate)/ 15
  
  #Inputs should be in % Air. Sat, minutes, kilograms, liters
  #Output is mL Oxygen per hour per microgram dry weight
  mass_conv_B88 = convert_rate(naup_resp_data$resp_rate_corr,
                               oxy.unit = "%Oxy",
                               time.unit = "m",
                               output.unit = "mL/h/mg",
                               mass = naup_resp_data$weight_c_kg_B88,
                               volume = 0.0028,
                               S = O2_adj$`Salinity [g/1000g]`[1],
                               t = mean(O2_adj$`Tm [°C]`),
                               P = (O2_adj$`p [mbar]`[1]/1000))
  
  #Inputs should be in % Air. Sat, minutes, kilograms, liters
  #Output is mL Oxygen per hour per microgram dry weight
  resp_conv = convert_rate(naup_resp_data$resp_rate_corr,
                           oxy.unit = "%Oxy",
                           time.unit = "m",
                           output.unit = "mL/h",
                           mass = NULL,
                           volume = 0.0028,
                           S = O2_adj$`Salinity [g/1000g]`[1],
                           t = mean(O2_adj$`Tm [°C]`),
                           P = (O2_adj$`p [mbar]`[1]/1000))
  
  naup_resp_data$resp_mL_h = resp_conv$rate.output
  
  naup_resp_data$msr_B88 = mass_conv_B88$rate.output
  
  naup_resp_rates = bind_rows(naup_resp_rates, naup_resp_data)
  
  naup_bg_resp_rates = bind_rows(naup_bg_resp_rates, bg_resp_data)
  
  O2_adj$rep = i
  naup_o2_record = bind_rows(naup_o2_record, O2_adj)
} #end of I (individual file) loop

naup_resp_rates$resp_rate = naup_resp_rates$resp_rate * -1

naup_resp_rates$msr_B88 = naup_resp_rates$msr_B88 * -1


#### Write Data ####
write.csv(o2_record, file = "Output/Data/o2_record.csv", row.names = F)
write.csv(full_dataset, file = "Output/Data/full_dataset.csv", row.names = F)
write.csv(naup_resp_rates, file = "Output/Data/naup_resp_rates.csv", row.names = F)
write.csv(molt_record, file = "Output/Data/molt_record.csv", row.names = F)
