###############################################################################
#
# This R script downloads select portions of the December 2020 Food Security
# Supplement from the census.gov API; cleans the data into a tidy format; and
# creates two data frames that are used by the 2020 Food Security App.
#
# Author:  John Williams
# Email:  jwili32@ncsu.edu
#
###############################################################################

# Required packages
library(tidyverse)
library(censusapi)

###############################################################################
#
# Download data from the API using a system key.
#
###############################################################################

# Add key to .Renviron
Sys.setenv(CENSUS_KEY="7ba991f2b52a05808dd84b9e20d650cc35f11a9a")
# Reload .Renviron
readRenviron("~/.Renviron")
# Check to see that the expected key is output in your R console
#Sys.getenv("CENSUS_KEY")

rawData <- getCensus(
  name = "2020/cps/foodsec/dec",
  vars = c("HRFS12MD", "PESEX", "PTDTRACE", "PEHSPNON", "PRTAGE", "PRCITSHP",
           "HRHTYPE", "HRNUMHOU", "PEMLR", "HEFAMINC", "PRMARSTA", "HEHOUSUT",
           "PEEDUCA", "HESP1")
  )

###############################################################################
#
# Clean data into a tidy format.
#
###############################################################################

sex_levels <- c("1", "2")
sex_labels <- c("Male", "Female")

race_levels <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12",
                 "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", 
                 "23", "24", "25", "26")
race_labels <- c("White", "Black", "American Indian, Alaskan Native", "Asian",
                 "Hawaiian/Pacific Islander", "White-Black", "White-AI",
                 "White-Asian", "White-HP", "Black-AI", "Black-Asian", 
                 "Black-HP", "AI-Asian", "AI-HP", "Asian-HP", "W-B-AI", "W-B-A",
                 "W-B-HP", "W-AI-A", "W-AI-HP", "W-A-HP", "B-AI-A", "W-B-AI-A", 
                 "W-AI-A-HP", "Other 3 Race Combinations", 
                 "Other 4 and 5 Race Combinations")

hispanicOrigin_levels <- c("1", "2")
hispanicOrigin_labels <- c("Hispanic", "Non-Hispanic")

citizenship_levels <- c("1", "2", "3", "4", "5")
citizenship_labels <- c("Native, Born in US", "Native, Born in US Territory", 
                        "Native, Born Abroad to US Parent(s)", 
                        "Foreign Born, Naturalized US Citizen", 
                        "Foreign Born, Not US Citizen")

typeHH_levels <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 0, -1)
typeHH_labels <- c("Married Civilian Family", "Married Non-Civilian Family",
                   "Unmarried Civ. Male-Primary Family",
                   "Unmarried Civ. Female-Primary Family",
                   "Unmarried Non-Civilian Family",
                   "Civilian Male Primary Individual",
                   "Civilian Female Primary Individual",
                   "Non-Civilian Primary Individual",
                   "Group Quarters With Family",
                   "Group Quarters Without Family",
                   "Non-Interview Household",
                   "In Universe, Met No Conditions To Assign")

employStatus_levels <- c("1", "2", "3", "4", "5", "6", "7", "-1")
employStatus_labels <- c("Employed-At Work", "Employed-Absent",
                         "Unemployed-On Layoff", "Unemployed-Looking",
                         "Retired-Not In Labor Force",
                         "Disabled-Not In Labor Force",
                         "Other-Not In Labor Force", "Not in Universe")

annualHHIncome_levels <- c("-1", "1", "2", "3", "4", "5", "6", "7", "8", "9", 
                           "10", "11", "12", "13", "14", "15", "16")
annualHHIncome_labels <- c("NA", "Less Than $5,000", "5,000 To 7,499", 
                           "7,500 To 9,999", "10,000 To 12,499", 
                           "12,500 To 14,999", "15,000 To 19,999",
                           "20,000 To 24,999", "25,000 To 29,999", 
                           "30,000 To 34,999", "35,000 To 39,999",
                           "40,000 To 49,999", "50,000 To 59,999",
                           "60,000 To 74,999", "75,000 To 99,999",
                           "100,000 To 149,999", "150,000 or More")

maritalStatus_levels <- c("1", "2", "3", "4", "5", "6", "7", "-1")
maritalStatus_labels <- c("Married, Civilian Spouse Present", 
                          "Married, Armed Forces Spouse Present",
                          "Married, Spouse Absent (exc. Separated)", 
                          "Widowed", "Divorced", "Separated", "Never married", 
                          "In Universe, Met No Conditions To Assign")


livingQuarters_levels <- c("-1", "1", "2", "3", "4", "5", "6", "7", "8", "9", 
                           "10", "11", "12", "0")
livingQuarters_labels <- c("NA", "House, Apartment, Flat", 
                           "Hu In Nontransient Hotel, Motel, Etc.",
                           "Hu Permanent In Transient Hotel, Motel",
                           "Hu In Rooming House",
                           "Mobile Home Or Trlr W/No Perm Rm Added",
                           "Mh Or Trlr W/1 Or More Perm Rms Added",
                           "Hu Not Specified Above",
                           "Quarters Not Hu In Rooming Or Brding Hs",
                           "Unit Not Perm. In Transient Hotl, Motl",
                           "Unoccupied Tent Site Or Trlr Site",
                           "Student Quarters In College Dorm",
                           "Other Unit Not Specified Above",
                           "Other Unit")

educationLevel_levels <- c("31", "32", "33", "34", "35", "36", "37", "38", "39",
                           "40", "41", "42", "43", "44", "45", "46", "-1")
educationLevel_labels <- c("Less Than 1st Grade", "1st,2nd,3rd Or 4th Grade",
                           "5th Or 6th Grade", "7th Or 8th Grade", "9th Grade",
                           "10th Grade", "11th Grade", "12th Grade No Diploma",
                           "High School Grad-Diploma Or Equiv (ged)",
                           "Some College But No Degree",
                           "Associate Degree-Occupational/Vocationl",
                           "Associate Deg.-Academic Program",
                           "Bachelor's Degree(ex:ba,ab,bs)",
                           "MASTER'S DEGREE(EX:MA,MS,MEng,MEd,MSW)",
                           "Professional School Deg(ex:md,dds,dvm)",
                           "DOCTORATE DEGREE(EX:PhD,EdD)", "Not in Universe")

receivedSNAP_levels <- c(1, 2, -2, -3, -10, -1)
receivedSNAP_labels <- c("Yes", "No", "Don't Know", "Refused to Answer", 
                         "No Response", "No Response")

foodSecurity_levels <- c(1, 2, 3, 4, -10, -9, -1)
foodSecurity_labels <- c("High", "Marginal", "Low", "Very Low", 
                         "No Response", "No Response", "No Response")

foodSecurity <- rawData %>% 
                rename(foodSecurity = HRFS12MD,
                       sex = PESEX,
                       race = PTDTRACE,
                       hispanicOrigin = PEHSPNON,
                       age = PRTAGE,
                       citizenship = PRCITSHP,
                       typeHH = HRHTYPE,
                       numHHMembers = HRNUMHOU,
                       employStatus = PEMLR,
                       annualHHIncome = HEFAMINC,
                       maritalStatus = PRMARSTA,
                       livingQuarters = HEHOUSUT,
                       educationLevel = PEEDUCA,
                       receivedSNAP = HESP1) %>%
                mutate(foodSecurity = factor(foodSecurity,
                                             levels = foodSecurity_levels,
                                             labels = foodSecurity_labels),
                       sex = factor(sex,
                                    levels = sex_levels,
                                    labels = sex_labels),
                       race = factor(race,
                                     levels = race_levels,
                                     labels = race_labels),
                       hispanicOrigin = factor(hispanicOrigin,
                                               levels = hispanicOrigin_levels,
                                               labels = hispanicOrigin_labels),
                       age = as.numeric(age),
                       age = cut(age, 
                                 breaks = c(-0.5, 4.5, 9.5, 14.5, 19.5, 24.5,
                                            29.5, 34.5, 39.5, 44.5, 49.5, 54.5,
                                            59.5, 64.5, 69.5, 74.5, 79.5, 84.5,
                                            89.5),
                                 labels=c('Under 5', '5 - 9', '10 - 14',
                                          '15 - 19', '20 - 24', '25 - 29',
                                          '30 - 34', '35 - 39', '40 - 44',
                                          '45 - 49', '50 - 54', '55 - 59',
                                          '60 - 64', '65 - 69', '70 - 74',
                                          '75 - 80', '80 - 84', '85 and Over')),
                       citizenship = factor(citizenship,
                                            levels = citizenship_levels,
                                            labels = citizenship_labels),
                       typeHH = factor(typeHH,
                                       levels = typeHH_levels,
                                       labels = typeHH_labels),
                       numHHMembers = as.numeric(numHHMembers),
                       numHHMembers = cut(numHHMembers, 
                                          breaks = c(0.5, 1.5, 2.5, 3.5, 4.5,
                                                     5.5, 6.5, 7.5, 8.5, 9.5,
                                                     19.5),
                                          labels=c('1', '2', '3', '4', '5',
                                                   '6', '7', '8', '9',
                                                   '10 or More')),
                       employStatus = factor(employStatus,
                                             levels = employStatus_levels,
                                             labels = employStatus_labels),
                       annualHHIncome = factor(annualHHIncome, 
                                               levels = annualHHIncome_levels,
                                               labels = annualHHIncome_labels),
                       maritalStatus = factor(maritalStatus,
                                              levels = maritalStatus_levels,
                                              labels = maritalStatus_labels),
                       livingQuarters = factor(livingQuarters,
                                               levels = livingQuarters_levels,
                                               labels = livingQuarters_labels),
                       educationLevel = factor(educationLevel,
                                               levels = educationLevel_levels,
                                               labels = educationLevel_labels),
                       receivedSNAP = factor(receivedSNAP,
                                             levels = receivedSNAP_levels,
                                             labels = receivedSNAP_labels))

# Reduce the number of factors of `race` by combining all 3 or more race 
# combinations together.
levels(foodSecurity$race) <- c("White", "Black",
                               "American Indian, Alaskan Native", "Asian",
                               "Hawaiian/Pacific Islander", "White-Black",
                               "White-AI", "White-Asian", "White-HP",
                               "Black-AI", "Other Two-Race Combination", 
                               "Other Two-Race Combination", 
                               "Other Two-Race Combination", 
                               "Other Two-Race Combination",
                               "Asian-HP", "Three or More Race Combinations",
                               "Three or More Race Combinations",
                               "Three or More Race Combinations",
                               "Three or More Race Combinations",
                               "Three or More Race Combinations",
                               "Three or More Race Combinations",
                               "Three or More Race Combinations",
                               "Three or More Race Combinations",
                               "Three or More Race Combinations",
                               "Three or More Race Combinations",
                               "Three or More Race Combinations")

# Reduce the number of factors of `employmentStatus` by merging "Other" and 
# "Not in Universe" categories
levels(foodSecurity$employStatus) <- c("Employed-At Work", "Employed-Absent",
                                       "Unemployed-On Layoff",
                                       "Unemployed-Looking",
                                       "Retired-Not In Labor Force",
                                       "Disabled-Not In Labor Force",
                                       "Other-Not In Labor Force",
                                       "Other-Not In Labor Force")

# Reduce the number of factors of `maritalStatus` by combining civilian and 
# non-civilian married.
levels(foodSecurity$maritalStatus) <- c("Married", "Married",
                                        "Married, Spouse Absent", "Widowed",
                                        "Divorced", "Separated", "Never Married",
                                        "Met No Conditions To Assign")

# Reduce the number of factors of `livingQuarters`
levels(foodSecurity$livingQuarters) <- c("NA", "House, Apartment, Flat", 
                                         "Hotel, Motel, Rooming House",
                                         "Hotel, Motel, Rooming House",
                                         "Hotel, Motel, Rooming House",
                                         "Mobile Home or Trailer",
                                         "Mobile Home or Trailer",
                                         "Other", "Other", "Other", "Other",
                                         "Other", "Other", "Other")

# Reduce the number of factors of `educationLevel` by combining all factors less
# than 7th grade.
levels(foodSecurity$educationLevel) <- c("Less Than 7th Grade", 
                                         "Less Than 7th Grade",
                                         "Less Than 7th Grade", 
                                         "7th Or 8th Grade", "9th Grade",
                                         "10th Grade", "11th Grade", 
                                         "12th Grade No Diploma",
                                         "High School Grad-Diploma or GED",
                                         "Some College-No Degree",
                                         "Associate Degree-Occupational/Vocational",
                                         "Associate Degree-Academic Program",
                                         "Bachelor's Degree", "Master's Degree",
                                         "Professional School Degree",
                                         "Doctorate Degree", "Not in Universe")

# Drop unused factors of variables
foodSecurity$typeHH <- droplevels(foodSecurity$typeHH)
foodSecurity$annualHHIncome <- droplevels(foodSecurity$annualHHIncome)

# predictors <- c("Sex", "Race", "Hispanic Origin", "Age", "US Citizenship",
#                 "Type of Household", "# of Household Members",
#                 "Employment Status", "Annual Household Income",
#                 "Marital Status", "Living Quarters",
#                 "Highest Education Level Attained",
#                 "Did Household Receive SNAP Benefits?")
# response <- "Food Security Level"

foodSecurityNR <- foodSecurity %>% 
                  filter(foodSecurity != "No Response")

foodSecurityNR$foodSecurity <- droplevels(foodSecurityNR$foodSecurity)
levels(foodSecurityNR$foodSecurity) <- c("Secure", "Secure", "Insecure", 
                                         "Insecure")

###############################################################################
#
# Save two data frames to the "food-security/data" folder.  These data frames
# are used by the 2020 Food Security App.
#
###############################################################################

saveRDS(foodSecurity, "./food-security/data/foodSecurity.rds")
saveRDS(foodSecurityNR, "./food-security/data/foodSecurityNR.rds")

