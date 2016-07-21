########
# Name:
# DataExplorer
#
# Description:
# Explores/validates the initially dataset.
########

##################
### Functions  ###
##################

########
# Name:
# decimalCounter
#
# Description:
# Calculates the number of decimals in a number. Also includes trailing 0's.
#
# Input:
# decimalNumber - A number from which the number of decimal should be counted.
#
# Output:
# The number of decimals the number has (including trailing 0's).
########
decimalCounter <- function(decimalNumber) {
  #Converts int to string.
  charNumber <- as.character(decimalNumber)
  # If there is a dot, calculates number of digits behind it.
  if(grepl("\\.", charNumber)) {
    return(nchar(gsub("(.*\\.)", "", charNumber)))
  }
  # If there is no dot, returns 0.
  else {
    return(0)
  }
}

########
# Name:
# dividabilityChecker.dataframe
#
# Description:
# Runs dividabilityChecker.vector over each individual column from the given dataframe where
# maxDecimals indicates there is at least 1 decimal.
#
# Input:
# values - A dataframe containing the values (in columns) that need to be checked.
# maxDecimals - The maximum number of decimals for each column.
#
# Output:
# The number of decimals the number has (including trailing 0's).
########
dividabilityChecker.dataframe <- function(values, maxDecimals) {
  return(
    sapply(names(maxDecimals[maxDecimals > 0]), function(paramName) {
      dividabilityChecker.vector(values[,paramName], maxDecimals[paramName])
    })
  )
}

########
# Name:
# dividabilityChecker.vector
#
# Description:
# Calculates the dividable percentage of a vector (using calculateDividablePercentage)
# with multiple dividers. The dividers that are always used are 1, 0.5 and 0.1, though
# depending on the maxDecimals for that paramName, more might be used. With a lower limit
# of 1 (which creates the 0.1 mentioned before) and upper limit of 12, extra dividers are
# generated using: 10^(-1:-dividerLimit)
#
# Input:
# values - A vector containing the values that need to be checked.
# maxDecimals - The maximum number of decimals used by the values.
#
# Output:
# The number of decimals the number has (including trailing 0's).
########
dividabilityChecker.vector <- function(values, maxDecimals) {
  dividerLimit <- maxDecimals
  # Makes sure there isn't a dividerLimit lower than 1 (would cause unexpected behavior).
  if(dividerLimit < 1) {dividerLimit = 1}
  # Limits the max number of used decimals as too many can cause a 
  # "probable complete loss of accuracy in modulus" error.
  if(dividerLimit > 12) {dividerLimit = 12}
  
  paramPercentages <- sapply(c(1,0.5,10^(-1:-dividerLimit)), function(divider) {
    calculateDividablePercentage(values, 10^maxDecimals, divider)
  })
  
  return(paramPercentages)
}

########
# Name:
# calculateDividablePercentage
#
# Description:
# Calculate the percentage of numbers in a vector can be divided by the given divider.
# Uses a multiplier value to first turn decimal numbers into integers, as otherwise
# the modulo might function incorrectly
# (http://stackoverflow.com/questions/13614749/modulus-bug-in-r):
# 4.60 %% 0.1 = 0.1
# (4.60*100) %% (0.1*100) = 10
# as.integer(4.60*100) %% (0.1*100) = 9
# round(4.60*100) %% (0.1*100) = 0
#
# Input:
# values - A vector containing the values that need to be checked.
# multiplier - The multiplier that should be used to turn decimal numbers into integers.
# divider - The divider which should be used to see how many values (as percentage of
#           all known values) can be divided by this number (so have a remaining value of
#           0 when using a modulo).
# showUndividable - Prints the values that were not dividable by the given divider.
#
# Output:
# The number of decimals the number has (including trailing 0's).
########
calculateDividablePercentage <- function(values, multiplier, divider, showUndividable=FALSE) {
  # Remove NAs.
  noNAs <- values[!is.na(values)]
  # Collects the positions of the values that could be divided by the divider.
  dividablePositions <- which(round(noNAs*multiplier) %% round(divider*multiplier) == 0)
  # If TRUE, the undividable values are printed.
  if(showUndividable == TRUE) {print(noNAs[-dividablePositions])}
  # Returns the percentage of dividuable values (compared to vector with NAs removed).
  return(length(dividablePositions) / length(noNAs) * 100)
}

########
# Name:
# naPercentageCalculator
#
# Description:
# Calculates the percentage of missing values (NAs) in a vector.
#
# Input:
# values - The vector that needs to be checked on missing values.
#
# Output:
# A percentage indicating how many missing values the vector has.
########
naPercentageCalculator <- function(values) {
  return(sum(is.na(values)) / length(values) * 100)
}

########
# Name:
# convertZScoreToGroup
#
# Description:
# Converts z-scores into their ordinal values as depicted by ZHEIGHT en ZWEIGHT
# for comparing whether these are correct.
#
# Input:
# zScore - The z-score that needs to be converted into an ordinal value.
#
# Output:
# An ordinal value belonging to the z-score.
########
convertZScoreToGroup <- function(zScore) {
  if(is.na(zScore)) {
    return(zScore)
  } else if(zScore >= 3.29 | zScore <= -3.29) {
    return(1)
  } else if(zScore >= 2.58 | zScore <= -2.58) {
    return(2)
  } else if(zScore >= 1.96 | zScore <= -1.96) {
    return(3)
  } else {
    return(4)
  }
}

##################
###    Code    ###
##################

# Retrieves data.
data <- read.csv("Y:\\PhenoData\\New account; graduation thesis bioinformatics\\2016-05-26 LifeLines dataset combined_diabetes.csv")

# Returns number of parameters
ncol(data) # 133

# Fix for strange name first column
newColnames <- colnames (data)
newColnames[1] <- sub(".*\\.", "", newColnames[1])
colnames(data) <-  newColnames
rm(newColnames)

# Casts the '999' used in some columns as NA to actual NAs and includes a check
# before and after the value to view the NA percentage.
medColumns <- grepl("MED", colnames(data))
apply(data[,medColumns], 2, naPercentageCalculator)
data[,medColumns][data[,grepl("MED", colnames(data))] == 999] <- NA
rm(medColumns)

# Show NA percentages
columnNaPercentage <- apply(data, 2, naPercentageCalculator)
round(columnNaPercentage, 2)

# Show NA percentages of gestational age for females only.
naPercentageCalculator(data$FEM4[data$GENDER == 0])

# Show NA percentage of several parameters when selecting only those having answered 'yes'
# on HEALTH17A1 about wether they have DM.
naPercentageCalculator(data$Diabetes_duration[data$HEALTH17A1 == 1])
naPercentageCalculator(data$HEALTH17B1[data$HEALTH17A1 == 1])
naPercentageCalculator(data$HEALTH17C[data$HEALTH17A1 == 1])
naPercentageCalculator(data$HEALTH17D1[data$HEALTH17A1 == 1])
naPercentageCalculator(data$HEALTH_17YR[data$HEALTH17A1 == 1])

# Number of samples that gave an answer on a question that should only be filled in when they have
# answered 'yes' on HEALTH17A1 about wether they have DM while they answered 'no'.
sum(data$HEALTH17A1 == 2 & !is.na(data$HEALTH17B1), na.rm=TRUE)
sum(data$HEALTH17A1 == 2 & !is.na(data$HEALTH17C), na.rm=TRUE)
sum(data$HEALTH17A1 == 2 & !is.na(data$HEALTH17D1), na.rm=TRUE)
sum(data$HEALTH17A1 == 2 & !is.na(data$HEALTH_17YR), na.rm=TRUE)

# Checks how often each options of diabetes form was selected by samples that answered having no DM.
sapply(1:4, function(x) { sum(data$HEALTH17A1 == 2 & data$HEALTH17B1 == x, na.rm=TRUE) })

# Checks how often each options of diabetes treatment was selected by samples that answered
# having no DM.
sapply(1:5, function(x) { sum(data$HEALTH17A1 == 2 & data$HEALTH17D1 == x, na.rm=TRUE) })

# Shows unique samples that have no DM, but still noted an age since DM.
unique(data[data$HEALTH17A1 == 2 & !is.na(data$HEALTH17C), c("HEALTH17B1", "HEALTH17B2")])

# Shows how many samples filled in 0 years since DM (and how many of those filled in not having DM).
sum(data$HEALTH17C == 0, na.rm=TRUE)
sum(data$HEALTH17C == 0 & data$HEALTH17A1 == 2, na.rm=TRUE)

# Unqiue DM types for samples that filled in a year since DM but also filled in not having DM.
unique(data$HEALTH_17YR[data$HEALTH17A1 == 2])

# Shows how many samples mentioned having 'ZWANGERSCHAPSDIABETES' (gestational diabetes).
sum(data$HEALTH17B2 == "ZWANGERSCHAPSDIABETES")

# Shows unique samples that have 'ZWANGERSCHAPSDIABETES' as DM type.
unique(data[data$HEALTH17B2 == "ZWANGERSCHAPSDIABETES",
                c("HEALTH17A1", "HEALTH17B1", "HEALTH17B2")])

# Counts how many 'ZWANGERSCHAPSDIABETES' there are in each 'HEALTH17A1' group.
sapply(1:2, function(x) {
  sum(data$HEALTH17B2 == "ZWANGERSCHAPSDIABETES" & data$HEALTH17A1 == x, na.rm = TRUE)
})
sum(data$HEALTH17B2 == "ZWANGERSCHAPSDIABETES" & is.na(data$HEALTH17A1))

# Unique values where no "Other" for DM type was used but the custom field was still filled in
# (includes both DM and non-DM as filled in answer whether someone has DM).
unique(data[data$HEALTH17B1 != 3 & data$HEALTH17B2 != " ",
            c("HEALTH17A1", "HEALTH17B1", "HEALTH17B2")])

# Unique values in HEALTH17B2 for individuals that filled in having DM and "Other" in HEALTH17B1.
unique(data$HEALTH17B2[data$HEALTH17A1 == 1 & data$HEALTH17B1 == 3])

# Number of samples that have a GLUCOSE value indicating DM but diabetes indicating no DM.
sum(data$IFG == 2 & data$diabetes == 0, na.rm=TRUE)

# Number of samples having no DM (including and excluding IFG) according to GLUCOSE but diabetes
# indicating they have DM (T1DM, T2DM, gestational diabetes or hypoglycemia).
sum(data$IFG < 2 & data$diabetes != 0, na.rm=TRUE) # all forms of DM
sum(data$IFG < 1 & data$diabetes != 0, na.rm=TRUE) # all forms of DM
sum(data$IFG < 2 & data$diabetes %in% c(1,2), na.rm=TRUE) # T1DM and T2DM only
sum(data$IFG < 1 & data$diabetes %in% c(1,2), na.rm=TRUE) # T1DM and T2DM only

# Below does not work, BMI returns 5 instead of 13!
#apply(data, 2, function(x) {max(sapply(x, decimalCounter))})

# Calculates the number of decimals for each value (per parameter).
nDecimals <- list()
for(x in colnames(data)) {
  if(is.numeric(data[,x])) {
    nDecimals[[x]] <- sapply(data[,x], decimalCounter)
  }
}
rm(x)

# Show decimal info per parameter
nDecimalsSummary <- sapply(nDecimals, summary)
( maxDecimals <- nDecimalsSummary["Max.",] )

# Checks how dividable each parameter is except ones that have 0 decimals or were not numeric.
dividability <- dividabilityChecker.dataframe(data, maxDecimals)
sapply(dividability, digits=2, round)

# For some parameters, plot the number of decimals the samples have.
barplot(table(nDecimals$BMI), main="BMI", ylab="n samples", xlab="n decimals")
barplot(table(nDecimals$CGGFR), main="CGGFR", ylab="n samples", xlab="n decimals")
barplot(table(nDecimals$Percentile), main="Percentile", ylab="n samples", xlab="n decimals")
barplot(table(nDecimals$Tabak_totaal), main="Tabak_totaal", ylab="n samples", xlab="n decimals")

# Shows the values that have differing waist and taille values.
data[which(data$WAIST != data$TAILLE), c('PSEUDOIDEXT', 'WAIST', 'TAILLE')]

# Check whether there are columns with values lower than 0.
# NAs are text columns
( hasValuesLowerThanZero <- apply(data, 2, function(x) {any(as.numeric(x[!is.na(x)]) < 0)}) )

# Checks whether GLUCOSE and IFG conflict with each other.
sum(data$GLUCOSE < 6.1 & data$IFG != 0, na.rm=TRUE)
sum(data$GLUCOSE >= 6.1 & data$GLUCOSE < 7.0 & data$IFG != 1, na.rm=TRUE)
sum(data$GLUCOSE >= 7.0 & data$IFG != 2, na.rm=TRUE)

# Checks whether GLUCOSE and dm_new conflict with each other.
sum(data$GLUCOSE > 6.99 & data$dm_new == 0, na.rm=TRUE)
sum(data$GLUCOSE <= 6.99 & data$dm_new == 1, na.rm=TRUE)

# Shows all posibilities for dm_new depending on GLUCOSE levels.
unique(data$dm_new[data$GLUCOSE > 6.99])
unique(data$dm_new[data$GLUCOSE <= 6.99])

# Shows the gestational ages (sorted) that have a value lower or equal to 12 weeks.
( gestationalAge <- sort(data$gestational_age[!is.na(data$gestational_age) &
                                                data$gestational_age <= 14]) )
# Shows gestational age lower or equal to 12 as percentage of all known gestational ages.
length(gestationalAge) / sum(!is.na(data$gestational_age)) * 100

# Checks how many males there are with a gestational age.
sum(!is.na(data$gestational_age[data$GENDER == 1]))

# Show all unique cases of below defined columns regarding smoking
unique(data[,c("smokingclass", "smoking_status", "smok_group", "currsmoker", "eversmoker", "exsmoker")])

# Shows a summary of the packyears.
summary(data$packyears)

# How many samples with a known tabacco usage were not classified.
sum(!is.na(data$Tabak_totaal) & is.na(data$smokingclass))
sum(data$Tabak_totaal > 0 & is.na(data$smokingclass))

# All unique possibilities where a tabak smoked per day is known but the smokingclass,
# smoking_status or smok_group is not.
unique(data[!is.na(data$Tabak_totaal) & ( is.na(data$smokingclass) | is.na(data$smoking_status) | 
                                            is.na(data$smok_group) ),
            c("Tabak_totaal","smokingclass", "smoking_status", "smok_group")])

# All unique cases where Tabak_totaal == 0.
unique(data[data$Tabak_totaal == 0,
            c("Tabak_totaal", "packyears", "smokingclass", "smoking_status", "smok_group")])

# Number of samples marked as current smoker but use 0 g/day and/or are a non smoker.
sum(data$Tabak_totaal == 0 & data$smoking_status == 2, na.rm=TRUE)
sum(data$smokingclass == 0 & data$smoking_status == 2, na.rm=TRUE)
sum(data$Tabak_totaal == 0 & data$smokingclass == 0 & data$smoking_status == 2, na.rm=TRUE)
sum(data$Tabak_totaal == 0 & data$smokingclass == 0 & data$smok_group == 2, na.rm=TRUE)

# How many samples smoke 0g/day and have a packyears of 0.
sum(data$Tabak_totaal == 0 & data$packyears == 0, na.rm=TRUE)

# Validates packyears for non-smokers.
unique(data[data$smoking_status == 0, c("smoking_status", "Tabak_totaal", "packyears")])

# Validate packyears for former smokers.
unique(data[data$smoking_status == 1, c("smoking_status", "Tabak_totaal", "packyears")])

# All unique packyears
min(data$packyears, na.rm=TRUE)

# Check if anyone that uses tabak is classified as a non or ex smoker.
sum(data$Tabak_totaal > 0 & data$smokingclass %in% c(0,1), na.rm=TRUE)

# Check how many non-"ex smokers" that use 0 tabacco are still classified something else than non smoker.
sum(data$Tabak_totaal == 0 & data$exsmoker == 0 & !data$smokingclass %in% c(NA,0), na.rm=TRUE)
# Same as above, only then for ex-smokers classified as something else.
sum(data$Tabak_totaal == 0 & data$exsmoker == 1 & !data$smokingclass %in% c(NA,1), na.rm=TRUE)

# Checks how border cases are handled.
unique(data$smokingclass[data$Tabak_totaal == 0])
unique(data$smokingclass[data$Tabak_totaal == 10])
unique(data$smokingclass[data$Tabak_totaal == 20])

# Shows the tabacco usage and smokingclass of misclassified samples
# (excluding NAs & difference between non and ex smoker). which filters NAs.
data[which(
       ( data$Tabak_totaal == 0 & data$smoking_status == 0 & data$smokingclass != 0 ) |
       ( data$Tabak_totaal == 0 & data$smoking_status == 1 & data$smokingclass != 1 ) |
       ( data$Tabak_totaal > 0 & data$Tabak_totaal <= 10 & data$smokingclass != 2 ) |
       ( data$Tabak_totaal > 10 & data$Tabak_totaal < 20 & data$smokingclass != 3 ) |
       ( data$Tabak_totaal >= 20 & data$smokingclass != 4 ) )
       ,c("Tabak_totaal", "smokingclass", "exsmoker")]

# Checks whether the number of NAs in smoking_status is equal to the total number of NAs
# divided over currsmoker, eversmoker & exsmoker.
sum(is.na(data$smoking_status)) - sum(is.na(data$currsmoker))
sum(is.na(data$smoking_status)) - sum(is.na(data$eversmoker))
sum(is.na(data$smoking_status)) - sum(is.na(data$exsmoker))

# Check if z-scoes if different number of NAs
sum(is.na(data$WEIGHT)) - sum(is.na(data$ZWEIGHT))
sum(is.na(data$HEIGHT)) - sum(is.na(data$ZHEIGHT))

# Shows the values that have differing smoking_status and smok_group values.
data[which(data$smoking_status != data$smok_group), c('smoking_status', 'smok_group')]
sum(is.na(data$smoking_status)) - sum(is.na(data$smok_group))

# Looks at how the birth weights are distributed.
plot(table(data$BIRTH_Weight_all))

# Calculate whether BMI is stored correctly.
all.equal(data$WEIGHT / (data$HEIGHT/100)^2, data$BMI)

# Validates the Z-scores.
weightZScores <- (data$WEIGHT - mean(data$WEIGHT, na.rm=TRUE)) / sd(data$WEIGHT, na.rm=TRUE)
all.equal(sapply(weightZScores, convertZScoreToGroup), data$ZWEIGHT)

heightZScores <- (data$HEIGHT - mean(data$HEIGHT, na.rm=TRUE)) / sd(data$HEIGHT, na.rm=TRUE)
all.equal(sapply(heightZScores, convertZScoreToGroup), data$ZHEIGHT)
