#cleaning cancFakeer 2

#goals:
#1 recode factors to descriptive levels
#2 convert NA values to NA


library(dplyr)
library(stringr)
library(lubridate)


load("data/fakeSEERData")

#race======================================================

cancFake$race <- as.character(cancFake$race)

cancFake$race[cancFake$race == "white"] <- "White"

cancFake$race[cancFake$race == "black"] <- "Black"

cancFake$race[cancFake$race == "other"] <- "Other"

cancFake$race <- 
  as.factor(cancFake$race)

cancFake$race <- 
  relevel(cancFake$race, ref = "White")

#origin====================================================

table(cancFake$origin)

cancFake$origin <- 
  factor(cancFake$origin, levels = c(0:9), 
         labels = c("Non-Spanish/Non-Hispanic", 
                    "Mexican", 
                    "Puerto Rican", 
                    "Cuban", 
                    "South/Central American",
                    "Other Spanish/Hispanic",
                    "Spanish NOS",
                    "Spanish by surname only",
                    "Dominican Republic",
                    "Unknown"))

#make hispanic origin binary...

cancFake$origin <- ifelse(cancFake$origin == "Non-Spanish/Non-Hispanic", "No", "Yes")

#sex=======================================================

cancFake$sex <- as.character(cancFake$sex)

cancFake$sex[cancFake$sex == "male"] <- "Male"

cancFake$sex[cancFake$sex == "female"] <- "Female"

cancFake$sex <- as.factor(cancFake$sex)

table(cancFake$sex)

#parse date of diagnosis===================================

mds$Date.of.Diag <- paste(mds$Month.Diagnosis, "-", mds$Year.Diagnosis)

mds$Date.of.Diag <- parse_date_time(mds$Date.of.Diag, orders = "my")

#beho3=====================================================

cancFake$beho3 <- 
  factor(cancFake$beho3, levels = c(0:3),
         labels = c("Benign",
                    "Intracranial/CNS Uncertain",
                    "Carcinoma in situ or Intraepithelial Noninfiltrating/Noninvasive",
                    "Malignant primary site invasive"))

#grade=====================================================

cancFake$grade <- 
  factor(cancFake$grade, 
         levels = c(1:9), 
         labels = c("Grade 1", 
                    "Grade 2", 
                    "Grade 3", 
                    "Grade 4", 
                    "T-cell or T-cell precursor",
                    "B-cell or B-cell precursor",
                    "Null cell, non-t non-b",
                    "NK cell",
                    "Indeterminate"))

#behavior recode for analysis==============================

cancFake$behanal <- 
  factor(cancFake$behanal, 
         levels = c(0:6),
         labels = c("Benign",
                    "Borderline Malignant",
                    "In situ",
                    "Malignant",
                    "Only malignant according to ICDO3",
                    "No longer reportable in ICDO3",
                    "Only malignant after 2010"))

#stage=========================================================

cancFake$hststga <- 
  factor(cancFake$hststga, levels = c(0, 1, 2, 4, 8, 9),
         labels = c("In Situ", "Localized", "Regional",
         "Distant", "Loc.Reg.Prost", "Unstaged"))

#first primary=============================================

cancFake$firstprm <- 
  factor(cancFake$firstprm, 
         levels = c(0,1),
         labels = c("No", "Yes"))

#COD - site recode=========================================
icd0class <- read.table(file = "data/site02vComplete.ssv", sep = ";", stringsAsFactors = FALSE, fill = NA)

icd0class <- icd0class %>% select(V1, V5) %>% filter(V5 != "")

icd0class$V5 <- str_extract_all(string = icd0class$V5, pattern = "[0-9]{5}")

names(icd0class)[2] <- "COD"

icd0class$COD <- as.numeric(icd0class$COD)

cancFake <- left_join(cancFake, icd0class[,c(1,2)], by = "COD")

names(cancFake)[38] <- "COD.Names"

rm(icd0class)

#COD - site recode KM=========================================
icd0class <- read.table(file = "data/site02vComplete.ssv", sep = ";", stringsAsFactors = FALSE, fill = NA)

icd0class <- icd0class %>% select(V1, V5) %>% filter(V5 != "")

icd0class$V5 <- str_extract_all(string = icd0class$V5, pattern = "[0-9]{5}")

names(icd0class)[2] <- "codkm"

icd0class$codkm <- as.numeric(icd0class$codkm)

cancFake <- left_join(cancFake, icd0class[,c(1,2)], by = "codkm")

names(cancFake)[39] <- "codkm.Names"

rm(icd0class)

#vital status================================================

cancFake$statrec <- 
  factor(cancFake$statrec, 
         levels = c(1, 4), 
         labels = c("Alive",
                    "Passed Away"))

#dthclass===================================================

cancFake$dthclass <-
  factor(cancFake$dthclass, 
         levels = c(0, 1, 9), 
         labels = c("Alive or Dead of other cause",
                    "Passed Away",
                    "NA - not first tumor"))

#other dth class===================================================

cancFake$odthclass <- 
  factor(cancFake$odthclass, 
         levels = c(0, 1, 9), 
         labels = c("Alive or dead due to cancer",
                    "Passed Away",
                    "NA - not first cancer"))

#intprim===================================================

cancFake$intprim <- 
  factor(cancFake$intprim, 
         levels = c(0, 1, 9),
         labels = c("No", "Yes", 
                    "Behavioral exclusion"))

#radiatn================================================

cancFake$radiatn <- 
  factor(cancFake$radiatn, 
         levels = c(0:9), 
         labels = c("None",
                    "Beam Radiation",
                    "Radioactive Implants",
                    "Radioisotops",
                    "Beam and implants or isotopes",
                    "Radiation NOS",
                    "Other Radiation",
                    "Refused Radiation",
                    "Recommended, Admin Unknown",
                    "Unknown if admin"))

#radiation to cns=======================================

cancFake$radbrn <- 
  factor(cancFake$radbrn, 
         levels = c(0,1, 7, 8, 9), 
         labels = c("None",
                    "Radiation",
                    "Refused",
                    "Recommended, Admin Unknown",
                    "Unknown"))

#nosurg=================================================

canc$nosurg <- 
  factor(ifelse(canc$nosurg == 0, "Surgery Performed", 
                "No Surgery"))

#typefu================================================

cancFake$typefup <- 
  factor(cancFake$typefup, 
         levels = c(1:4),
         labels = c("Autopsy or Death Certificate",
                    "Active FU",
                    "In situ of cervix only",
                    "Case only now in FU"))

#surv===================================================

cancFake$surv[cancFake$surv == 9999] <- NA

#srvtimemonflag=========================================

#recode survival flags
cancFake$srvtimemonflag <- 
  factor(cancFake$srvtimemonflag, levels = c(0, 1, 2, 3, 8),
         labels = c("Date Last Contact = Date Dx", 
                    "Date Last Contact > Date Dx",
                    "Incomplete dates, maybe zero FU days",
                    "Incomplete dates, not zero FU days",
                    "Unknown"))

#srvtimemonpa=================================================== #need to rerun for this command!

cancFake$srvtimemonpa[cancFake$srvtimemonpa == 9999] <- NA

#srvtimemonflagpa=======================================

cancFake$srvtimemonflagpa <- 
  factor(cancFake$srvtimemonflagpa, levels = c(0, 1, 2, 3, 8),
         labels = c("Date Last Contact = Date Dx", 
                    "Date Last Contact > Date Dx",
                    "Incomplete dates, maybe zero FU days",
                    "Incomplete dates, not zero FU days",
                    "Unknown"))

#add Names to histo3==================================================

#read names
icdO3codes <- read.csv("data/icdo3Codes.csv", stringsAsFactors = FALSE)

#figuring out what's called what
labelJoin <- 
  icdO3codes[icdO3codes$Histology.Behavior.Four %in% unique(mds$Histology.ICDO3),c(7,8)] %>% 
  unique

rm(icdO3codes)

#make names of names useful
names(labelJoin) <- c("Morphology", "Histology.ICDO3")

#add labels
mds <- left_join(mds, labelJoin, by = "Histology.ICDO3")

rm(labelJoin)

#finally, make the names look nicer

varNames <- read.csv("data/seerabombVarNames.csv", stringsAsFactors = FALSE)

varNames <- varNames[,c(4,5)]

varNames$desc <- 
        paste(
        str_replace_all(string = 
                          str_replace_all(string = varNames[,2], 
                                          pattern =  "[^A-Za-z0-9]", 
                                          replacement = "\\."), 
                        pattern = "\\.{2}",
                        replacement = ""))

#cull
varNames <- varNames[which(names(cancFake) %in% varNames$SEERaBomb),]


#find matching names
matchingNames <- match(names(cancFake), varNames$SEERaBomb)

#match them! but only match the ones that actually exist!
names(cancFake)[is.na(matchingNames) == FALSE] <- 
  varNames$desc[matchingNames[which(is.na(matchingNames) == FALSE)]]






















