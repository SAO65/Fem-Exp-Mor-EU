# Preliminary evidence of an increasing rate of expansion of female disability across the European Union, 1995–2015: policy implications and challenges for the Health Programme post-2020. A reproducible research
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


data.LE.1995.male <- DT.LE[DT.LE$year == "1995" & DT.LE$sex == "Male", ]
data.HALE.1995.male <- DT.HALE[DT.HALE$year == "1995" & DT.HALE$sex == "Male", ]
data.YLD.1995.male <- as.data.frame(data.LE.1995.male$val - data.HALE.1995.male$val)

data.LE.2015.male <- DT.LE[DT.LE$year == "2015" & DT.LE$sex == "Male", ]
data.HALE.2015.male <- DT.HALE[DT.HALE$year == "2015" & DT.HALE$sex == "Male", ]
data.YLD.2015.male <- as.data.frame(data.LE.2015.male$val - data.HALE.2015.male$val)


tidy.data <- data.frame("Country" = data.HALE.1995.male$location,
                        "LE.F.95" = data.LE.1995.female$val,
                        "HALE.F.95" = data.HALE.1995.female$val,
                        "YLD.F.95" = data.YLD.1995.female$`data.LE.1995.female$val - data.HALE.1995.female$val`,
                        "LE.F.15" = data.LE.2015.female$val,
                        "HALE.F.15" = data.HALE.2015.female$val,
                        "YLD.F.15" = data.YLD.2015.female$`data.LE.2015.female$val - data.HALE.2015.female$val`,
                        "LE.M.95" = data.LE.1995.male$val,
                        "HALE.M.95" = data.HALE.1995.male$val,
                        "YLD.M.95" = data.YLD.1995.male$`data.LE.1995.male$val - data.HALE.1995.male$val`,
                        "LE.M.15" = data.LE.2015.male$val,
                        "HALE.M.15" = data.HALE.2015.male$val,
                        "YLD.M.15" = data.YLD.2015.male$`data.LE.2015.male$val - data.HALE.2015.male$val`)
                
attach(tidy.data)

tidy.data$DELTA.F.LE <- LE.F.15 - LE.F.95
tidy.data$DELTA.F.HALE <- HALE.F.15 - HALE.F.95
tidy.data$DELTA.F.YLD <- YLD.F.15 - YLD.F.95

tidy.data$DELTA.M.LE <- LE.M.15 - LE.M.95
tidy.data$DELTA.M.HALE <- HALE.M.15 - HALE.M.95
tidy.data$DELTA.M.YLD <- YLD.M.15 - YLD.M.95


# Saving Tidy Dataset to .csv file
write.csv(tidy.data, "TidyData.csv")

######################################################
