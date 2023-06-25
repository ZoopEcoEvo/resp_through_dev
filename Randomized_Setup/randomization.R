#Randomization 
#### Cup set up ###

#setwd("C:/users/mholm/OneDrive/Desktop/Thesis_R/Code/26C_Trial")

num_treatments = 2 #Total number of treatment groups
num_cups = 6 #Number of cups used for the initial exposure period
treatment_levels = c("control", "heatstress") #Names for the different treatment groupings
n_per_treatment = 15 #Total number of plates per treatment group
n_backups = 5 #Out of the above number, how many are initially designated as backup plates
total_n = num_treatments * n_per_treatment #Total sample size for one experimental trial

#Assigns each cup to a treatment group 
cup_treatments = data.frame("cup_id" = seq(1:num_cups),
                            "treatment" = sample(rep(treatment_levels, each = num_cups / 2), replace = F))

#### Plate set up ####
#Assigns each plate to a treatment group
plate_treatments = rep(treatment_levels, each = n_per_treatment)

#Creates a data frame to store the details about the experimental set up 
setup = data.frame("id" = seq(1:total_n),
                   "treatment" = sample(x = plate_treatments, size = total_n, replace = F),
                   "group" = "active",
                   "source_cup" = NA)

for(i in 1:num_treatments){ #For each designated treatment group
  cups = cup_treatments$cup_id[which(cup_treatments$treatment == treatment_levels[i])] #Picks out which cups were assigned to this treatment group
  plates = setup$id[which(setup$treatment == treatment_levels[i])] #Picks out which plates were assigned to this treatment group
  
  if(length(plates) == n_per_treatment){ #Double checks to make sure the correct number of plates were assigned to the treatment group
    source_cup = sample(cups, size = length(plates), replace = T) #Randomly assigns each plate to a cup
    setup$source_cup[which(setup$treatment == treatment_levels[i])] = source_cup
    
    backups = sample(plates, size = n_backups) #Randomly assigns which plates will be initially designated as back-ups
    setup$group[backups] = "backup"
  }else{
    print("Incorrect sample size in treatment")
  } 
}
setup = dplyr::arrange(setup, source_cup) #Arranges the data by source cup to make setting up the experiment easier

# use cup_treatments and setup while setting up the experiment.
cup_treatments
write.csv(cup_treatments, file ="Randomized_Setup/R1_cup_treatments.csv")

setup
write.csv(setup, file = "Randomized_Setup/R1_plate_setup.csv")

# Use blinded after the experiment has started when you need to replace individuals from one group with a backup individual from the same group. 
blinded = setup
blinded$treatment = as.numeric(as.factor(blinded$treatment)) #Replaces the treatment group names with numbers 
write.csv(blinded, file = "Randomized_Setup/R1_blinded_plates.csv")


