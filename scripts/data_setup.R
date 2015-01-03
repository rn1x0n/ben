source(file.path("..", "_setup.R"))
data <- read.csv(file.path(gv$data, 'Individual_factors_of_voice_behaviours_19DEC2014v2 2.csv'))

# read in data, remove duplicates, row name by unique ID
data <- data[1:97,]
data0 <- data[, 2:24]

# Some NAs in countries, assume these are zeros
for(i in 16:20){
  data0[is.na(data0[,i]), i] <- 0 
}

# Make labels
labels <- names(data0)
labels <- gsub("\\.", " ", labels) # replace dots
labels <- gsub("[ ]{2,}", " ", labels) # replace double spaces
labels <- gsub("[ ]$", "", labels) # replace trailing spaces
labels <- gsub("Don t", "Don't", labels)

# Rename labels
for(label in labels){
  cat('labels[labels == "', label, '"] <- "', label, '"\n', sep = "")
}

labels[labels == "Don't bypass your boss upward IVT 1"] <- "Don't bypass boss upward"
labels[labels == "Presumed Target Identification IVT 2"] <- "Presumed Target Identification"
labels[labels == "Don't embarass the boss publicly IVT 3"] <- "Don't embarass boss publicly" 
labels[labels == "Negative career consequences of voice IVT 4"] <- "Negative career\nconsequences of voice"

labels[labels == "Constructive Voice Behavior"] <- "Constructive\nVoice Behavior"
labels[labels == "Power Distance" ] <- "Power Distance" 
labels[labels == "Collectivist"] <- "Collectivist"
labels[labels == "Resistance to change"] <- "Resistance to change"
labels[labels == "Core Self Evaluation Scale"] <- "Core Self\nEvaluation Scale"
labels[labels == "Openess to change"] <- "Openess to change"

labels[labels == "Age"] <- "Age"
labels[labels == "Gender"] <- "Gender"
labels[labels == "Level of education"] <- "Level of education"
labels[labels == "No of countries you have lived in for at least more than 12 months"] <- "Num countries 12 months"
labels[labels == "Where were these countries located Asia including India"] <- "Countries Asia"
labels[labels == "Where were these countries located Europe Australia"] <- "Countries Europe/Australia"
labels[labels == "Where were these countries located Latin America"] <- "Countries Latin America"
labels[labels == "Where were these countries located North America"] <- "Countries North America"
labels[labels == "Where were these countries located Africa"] <- "Countries Africa"
labels[labels == "Please list the countries where you have lived in for at least more than 12 months"] <- "Countries list"
labels[labels == "Are you a manager do you have people reporting to you"] <- "Manager"
labels[labels == "How long have you worked for your current organisation"] <- "Duration current organisation"

# Rename data variables
names(data0) <- make.names(labels)
names(labels) <- names(data0)

# Class varaibles
data0$Age <- factor(data0$Age)                                                                    
data0$Gender <- factor(data0$Gender)                                                              
data0$Level.of.education <- factor(data0$Level.of.education)                                      
data0$Num.countries.12.months <- factor(data0$Num.countries.12.months)                           
data0$Countries.Asia <- factor(data0$Countries.Asia)                                             
data0$Countries.Europe.Australia <- factor(data0$Countries.Europe.Australia)                      
data0$Countries.Latin.America <- factor(data0$Countries.Latin.America)                            
data0$Countries.North.America <- factor(data0$Countries.North.America)                            
data0$Countries.Africa <- factor(data0$Countries.Africa)                                          
data0$Countries.list <- factor(data0$Countries.list)                                             
data0$Manager <- factor(data0$Manager)                                                           
data0$Duration.current.organisation <- factor(data0$Duration.current.organisation)  

attr(data0, "labels") <- labels

# output data
dput(data0, file.path(gv$data, 'data.dput'))