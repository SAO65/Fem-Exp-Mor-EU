data <- read.csv("TidyData.csv")

attach(data)

# GAP WITHIN M AND 

# FEMALE
max(DELTA.F.LE)
subset(data, DELTA.F.LE == max(DELTA.F.LE))
min(DELTA.F.LE)
subset(data, DELTA.F.LE == min(DELTA.F.LE))


max(DELTA.F.HALE)
subset(data, DELTA.F.HALE == max(DELTA.F.HALE))
min(DELTA.F.HALE)
subset(data, DELTA.F.HALE == min(DELTA.F.HALE))

max(DELTA.F.YLD)
subset(data, DELTA.F.YLD == max(DELTA.F.YLD))
min(DELTA.F.YLD)
subset(data, DELTA.F.YLD == min(DELTA.F.YLD))


# MALE
max(DELTA.M.LE)
subset(data, DELTA.M.LE == max(DELTA.M.LE))
min(DELTA.M.LE)
subset(data, DELTA.M.LE == min(DELTA.M.LE))


max(DELTA.M.HALE)
subset(data, DELTA.M.HALE == max(DELTA.M.HALE))
min(DELTA.M.HALE)
subset(data, DELTA.M.HALE == min(DELTA.M.HALE))

max(DELTA.M.YLD)
subset(data, DELTA.M.YLD == max(DELTA.M.YLD))
min(DELTA.M.YLD)
subset(data, DELTA.M.YLD == min(DELTA.M.YLD))




#######################################

sum(LE.F.15 > LE.F.95)
sum(HALE.F.15 > HALE.F.95)
sum(YLD.F.15 > YLD.F.95)


sum(LE.M.15 > LE.M.95)
sum(HALE.M.15 > HALE.M.95)
sum(YLD.M.15 > YLD.M.95)

#########################################








