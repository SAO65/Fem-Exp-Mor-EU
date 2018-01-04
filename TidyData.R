# Measuring the rate of expansion of female morbidity across 28 Countries of the European Union 1995-2015: a reproducible Bayesian data analysis
# Stefano Olgiati-1*, Michele Gragnolati-2, Ankur Kalra-3, Alessandro Danovi-1
# 1 Dept. of Economics, Management and Quantitative Methods / University of Bergamo, Bergamo, Italy
# 2 Health, Nutrition and Population Global Practice / World Bank, Washington DC, USA
# 3 Div. of Cardiovascular Medicine, Dept. of Medicine / Case Western Reserve University School of Medicine, Cleveland, Ohio, USA
# * Corresponding author: stefano.olgiati@unibg.it


######################################################

library(data.table)
######################################################

# Raw Data
HALE.raw.data <- read.csv("./IHME-GBD_2015_DATA-7c514cb9-1/IHME-GBD_2015_DATA-7c514cb9-1.csv")
LE.raw.data <- read.csv("./IHME-GBD_2015_DATA-b8689aa2-1/IHME-GBD_2015_DATA-b8689aa2-1.csv")

head(LE.raw.data)
head(HALE.raw.data)

attach(LE.raw.data)
attach(HALE.raw.data)

DT.LE <- data.table(LE.raw.data)
DT.HALE <- data.table(HALE.raw.data)
DT.HALE

tables()

######################################################

# Tidy Data

# Expansion of morbidity

#Years lived with disability (YLDs)
# Years of life lived with any short-term or long-term health loss.

# Healthy life expectancy, or health-adjusted life expectancy (HALE)
#The number of years that a person at a given age can expect to live in good health, 
# taking into account mortality and disability.

data.LE.1995.female <- DT.LE[DT.LE$year == "1995" & DT.LE$sex == "Female", ]
data.HALE.1995.female <- DT.HALE[DT.HALE$year == "1995" & DT.HALE$sex == "Female", ]
data.YLD.1995.female <- as.data.frame(data.LE.1995.female$val - data.HALE.1995.female$val)

data.LE.2015.female <- DT.LE[DT.LE$year == "2015" & DT.LE$sex == "Female", ]
data.HALE.2015.female <- DT.HALE[DT.HALE$year == "2015" & DT.HALE$sex == "Female", ]
data.YLD.2015.female <- as.data.frame(data.LE.2015.female$val - data.HALE.2015.female$val)

tidy.data <- data.frame("HALE.F.95" = data.HALE.1995.female$val,
                        "YLD.F.95" = data.YLD.1995.female$`data.LE.1995.female$val - data.HALE.1995.female$val`,
                        "HALE.F.15" = data.HALE.2015.female$val,
                        "YLD.F.15" = data.YLD.2015.female$`data.LE.2015.female$val - data.HALE.2015.female$val`)
                        
tidy.data

# Saving Tidy Dataset to .csv file
write.csv(tidy.data, "TidyData.csv")

######################################################
