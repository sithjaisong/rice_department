# Apply gather() to bmi and save the result as bmi_long
bmi_long <- gather(data = bmi, year, bmi_val, -Country)

# View the first 20 rows of the result
head(bmi_long, 20)

# Apply spread() to bmi_long
bmi_wide <- spread(bmi_long, year, bmi_val)

# View the head of bmi_wide
head(bmi_wide)

# Apply separate() to bmi_cc
bmi_cc_clean <- separate(data = bmi_cc, col = Country_ISO, into = c("Country", "ISO"), sep = "/")

# Print the head of the result
head(bmi_cc_clean)

# Apply unite() to bmi_cc_clean
bmi_cc <- unite(data = bmi_cc_clean, col = Country_ISO, Country, ISO, sep = "-")

# View the head of the result
head(bmi_cc)
## tidyr and dplyr are already loaded for you

# View the head of census
head(census)

# Gather the month columns
census2 <- gather(data = census, key = month, value = amount, -YEAR)

# Arrange rows by YEAR using dplyr's arrange
census2 <- arrange(census2, YEAR)

# View first 20 rows of census2
head(census2, 20)

## tidyr is already loaded for you

# View first 50 rows of census_long
head(census_long, 50)

# Spread the type column
census_long2 <- spread(census_long, type, amount)

# View first 20 rows of census_long2
head(census_long2, 20)

## tidyr is already loaded for you

# View the head of census_long3
head(census_long3)

# Separate the yr_month column into two
census_long4 <- separate(data = census_long3, col = yr_month, c("year", "month") )

# View the first 6 rows of the result
head(census_long4, 6)

# Make this evaluate to character
class("true")

# Make this evaluate to numeric
class(8484.00)

# Make this evaluate to integer
class(99L)

# Make this evaluate to factor
class(as.factor("factor"))

# Make this evaluate to logical
class(FALSE)

# Preview students with str()
str(students)

# Coerce Grades to character
students$Grades <- as.character(students$Grades)

# Coerce Medu to factor
students$Medu <- as.factor(students$Medu)

# Coerce Fedu to factor
students$Fedu <- as.factor(students$Fedu)

# Look at students once more with str()
str(students)

# Preview students2 with str()
str(students2)

# Load the lubridate package
library(lubridate)

# Parse as date
dmy("17 Sep 2015")

# Parse as date and time (with no seconds!)
mdy_hm("July 15, 2012 12:56")

# Coerce dob to a date (with no time)
students2$dob <- ymd(students2$dob)

# Coerce nurse_visit to a date and time
students2$nurse_visit <- ymd_hms(students2$nurse_visit)

# Look at students2 once more with str()
str(students2)