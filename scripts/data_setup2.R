data <- read.csv(file.path(gv$data, 'Individual_factors_of_voice_behaviours Raw Data 29DEC2014v10.csv'))

#data <- read.csv(file.path(gv$data, 'Individual_factors_of_voice_behaviours Raw Data 29DEC2014v5.csv'))

# read in data, remove duplicates, row name by unique ID
#data0 <- data[,1:10]

# Make labels
labels <- names(data)
labels <- gsub("\\.", " ", labels) # replace dots
labels <- gsub("[ ]{2,}", " ", labels) # replace double spaces
labels <- gsub("[ ]$", "", labels) # replace trailing spaces

# Rename labels
labels[labels == "Constructive Voice Behaviour"] <- "Constructive voice behaviour"
labels[labels == "Power distance"] <- "Power distance"
labels[labels == "Collectivist"] <- "Collectivist"
labels[labels == "Resistance to Change"] <- "Resistance to change"
labels[labels == "Core Self Evaluation Scale"] <- "Core self evaluation"
labels[labels == "Openness to Change"] <- "Openness to change"
labels[labels == "Age"] <- "Age"
labels[labels == "Gender"] <- "Gender"
labels[labels == "Level of education"] <- "Education"
labels[labels == "Manager"] <- "Manager"

# Rename data variables
names(data) <- make.names(labels)
names(labels) <- names(data)

# Class varaibles
data$Age <- factor(data$Age)                                                                    
data$Gender <- factor(data$Gender)                                                              
data$Education <- factor(data$Education,
                                   levels = c(                                  
                                   "High School / Secondary School",
                                   "Diploma",
                                   "Bachelor Degree",                             
                                   "Postgraduate Diploma / Masters",
                                   "PhD"))                                     
data$Manager <- factor(data$Manager, levels = c("NO", "YES"), labels = c("No", "Yes"))                                                           

attr(data, "labels") <- labels

# output data
dput(data, file.path(gv$data, 'data2.dput'))



#####################################################################
# Question components

dataCVB <- read.csv(file.path(gv$data, paste0('Individual_factors_of_voice_behaviours Raw Data 29DEC2014v6 ', 'CVB', '.csv')),
                    skip = 0, stringsAsFactors = FALSE)
dataCVB <- dataCVB[-1, 3:7]

dataRTC <- read.csv(file.path(gv$data, paste0('Individual_factors_of_voice_behaviours Raw Data 29DEC2014v6 ', 'RTC', '.csv')),
                    skip = 0, stringsAsFactors = FALSE)
dataRTC <- dataRTC[-1, 2:18]

dataCSE <- read.csv(file.path(gv$data, paste0('Individual_factors_of_voice_behaviours Raw Data 29DEC2014v6 ', 'CSE', '.csv')),
                    skip = 1, stringsAsFactors = FALSE)
dataCSE <- dataCSE[-1, 3:14]

dataCOL <- read.csv(file.path(gv$data, paste0('Collectivist-Table 1.csv')),
                    skip = 1, stringsAsFactors = FALSE)
dataCOL <- dataCOL[, 4:9]

dataPOW <- read.csv(file.path(gv$data, paste0('Power distance-Table 1.csv')),
                    skip = 1, stringsAsFactors = FALSE)
dataPOW <- dataPOW[, 4:8]

data.comp <- list(
  CVB = dataCVB,
  RTC = dataRTC,
  CSE = dataCSE,
  COL = dataCOL,
  POW = dataPOW
)

for(n in names(data.comp)){
  for(v in names(data.comp[[n]])){
    data.comp[[n]][,v] <- as.numeric(data.comp[[n]][,v])
  }
  
}

dput(data.comp, file.path(gv$data, 'data.comp.dput'))
