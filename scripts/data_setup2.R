data <- read.csv(file.path(gv$data, 'Individual_factors_of_voice_behaviours Raw Data 29DEC2014v5.csv'))

# read in data, remove duplicates, row name by unique ID
data0 <- data[,1:10]

# Make labels
labels <- names(data0)
labels <- gsub("\\.", " ", labels) # replace dots
labels <- gsub("[ ]{2,}", " ", labels) # replace double spaces
labels <- gsub("[ ]$", "", labels) # replace trailing spaces

# Rename labels
labels[labels == "Constructive Voice Behaviour"] <- "Constructive voice behaviour"
labels[labels == "Power distance"] <- "Power distance"
labels[labels == "Resistance to Change"] <- "Resistance to change"
labels[labels == "Core Self Evaluation"] <- "Core self evaluation"
labels[labels == "Openness to Change"] <- "Openness to change"
labels[labels == "Age"] <- "Age"
labels[labels == "Gender"] <- "Gender"
labels[labels == "Level of education"] <- "Education"
labels[labels == "Are you a manager do you have people reporting to you"] <- "Manager"

# Rename data variables
names(data0) <- make.names(labels)
names(labels) <- names(data0)

# Class varaibles
data0$Age <- factor(data0$Age)                                                                    
data0$Gender <- factor(data0$Gender)                                                              
data0$Education <- factor(data0$Education,
                                   levels = c(                                  
                                   "High School / Secondary School",
                                   "Diploma",
                                   "Bachelor Degree",                             
                                   "Postgraduate Diploma / Masters",
                                   "PhD"))                                     
data0$Manager <- factor(data0$Manager, levels = c("NO", "YES"), labels = c("No", "Yes"))                                                           

attr(data0, "labels") <- labels

# output data
dput(data0, file.path(gv$data, 'data2.dput'))