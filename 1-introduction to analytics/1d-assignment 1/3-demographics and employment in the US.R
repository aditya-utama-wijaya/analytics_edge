CPS = read.csv('3a-CPSData.csv')

# How many interviewees are in the dataset?
str(CPS)

# What is the most common industry of employment?
sort(table(CPS$Industry))

# Which state has the fewest interviewees?
# Which state has the largest number of interviewees?
sort(table(CPS$State))

# What proportion of interviewees are citizens of the US?
table(CPS$Citizenship)

# For which races are there at least 250 interviewees in the CPS dataset of Hispanic ethnicity?
table(CPS$Race, CPS$Hispanic)

#Which variable have at least 1 interviewee with a missing (NA) value?
summary(CPS)

table(CPS$Region, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))
table(CPS$Citizenship, is.na(CPS$Married))
# The Married variable being missing is related to the Age value for the interviewee

# How many states had all interviewees living in a non-metropolitan area?
# How many states had all interviewees living in a metropolitan area?
table(CPS$State, is.na(CPS$MetroAreaCode))

# Which region has the largest proportion of interviewees living in a non-metropolitan area?
table(CPS$Region, is.na(CPS$MetroAreaCode))

# Which state has a proportion of interviewees living in a non-metropolitan area closest to 30%?
# Which state has the largest proportion of non-metropolitan interviewees?
sort(tapply(is.na(CPS$MetroAreaCode), CPS$State, mean))

MetroAreaMap = read.csv('3b-MetroAreaCodes.csv')
CountryMap = read.csv('3c-CountryCodes.csv')

#How many observations are there in MetroAreaMap?
str(MetroAreaMap)

# How many observations are there in CountryMap?
str(CountryMap)

CPS = merge(CPS, MetroAreaMap, by.x = 'MetroAreaCode', by.y = 'Code', all.x = TRUE)

# What is the name of the variable that was added to the data frame by the merge() operation?
str(CPS)

# How many interviewees have a missing value for the new metropolitan area variable?
summary(CPS$MetroArea)

# Which metropolitan area has the largest number of interviewees?
sort(table(CPS$MetroArea))

# Which metropolitan area has the highest proportion of interviewees of Hispanic ethnicity?
sort(tapply(CPS$Hispanic, CPS$MetroArea, mean))

# Determine the number of metropolitan areas from which at least 20% of interviewees are Asian!
sort(tapply(CPS$Race == 'Asian', CPS$MetroArea, mean))

# Determine which metropolitan area has the smallest proportion of interiewees who have received no high school diploma!
sort(tapply(CPS$Education == 'No high school diploma', CPS$MetroArea, mean, na.rm = TRUE))

CPS = merge(CPS, CountryMap, by.x = 'CountryOfBirthCode', by.y = 'Code', all.x = TRUE)

# What is the name of the variable added to the CPS data frame by this merge operation?
str(CPS)

# How many interviewees have a missing value for the new country of birth variable?
summary(CPS$Country)

# Which country was the most common place of birth?
sort(table(CPS$Country))

# What proportion of the interviewees from the NY-NJ-PA metropolitan area have a country of birth that is not the US?
table(CPS$MetroArea == 'New York-Northern New Jersey-Long Island, NY-NJ-PA', CPS$Country != 'United States')

# Which metropolitan area has the largest number of interviewees with a country of birth in India?
sort(tapply(CPS$Country == 'India', CPS$MetroArea, sum, na.rm = TRUE))
