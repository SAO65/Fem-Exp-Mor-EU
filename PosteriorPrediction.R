# Preliminary evidence of an increasing rate of expansion of female disability across the European Union, 1995–2015: policy implications and challenges for the Health Programme post-2020. A reproducible research
# Stefano Olgiati-1*, Michele Gragnolati-2, Ankur Kalra-3, Alessandro Danovi-1
# 1 Dept. of Economics, Management and Quantitative Methods / University of Bergamo, Bergamo, Italy
# 2 Health, Nutrition and Population Global Practice / World Bank, Washington DC, USA
# 3 Div. of Cardiovascular Medicine, Dept. of Medicine / Case Western Reserve University School of Medicine, Cleveland, Ohio, USA
# * Corresponding author: stefano.olgiati@unibg.it


# Stefano Olgiati, PhD, MS
# Department of Quantitative Methods, via dei Caniana 2, Bergamo (ITA) 24129; tel  +39 
# 035 20 52 638; fax +39 035 20 52 549; email <stefano.olgiati@unibg.it>

# Based on:
# Kruschke, J. K. (2014). Jags-Ymet-Xmet-Mrobust.R 

# Accompanies the book:
# Kruschke, J. K. (2014). Doing Bayesian Data Analysis: 
# A Tutorial with R, JAGS, and Stan 2nd Edition. Academic Press / Elsevier.
# Web Open Access: https://sites.google.com/site/doingbayesiandataanalysis/software-installation

#===============================================================================
library(rjags)
#===============================================================================
source("Jags.R")
source("DBDA2E-utilities.R")
#===============================================================================

# FUNCTION 1/3: DATA, MODEL, INITIALIZE AND RUN THE CHAINS
codaSamples <- genMCMC(data)

# FUNCTION 2/3: SUMMARY INFO 
smryMCMC(codaSamples)

# FUNCTION 3/3: PLOT
plotMCMC(codaSamples , data)

# MCMC DIAGNOSTICS
diagMCMC(codaObject = codaSamples, parName = "beta1", saveName = title)


