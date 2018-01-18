# Preliminary evidence of an increasing rate of expansion of female disability across the European Union, 1995–2015: policy implications and challenges for the Health Programme post-2020. A reproducible research
# Stefano Olgiati-1*, Michele Gragnolati-2, Ankur Kalra-3, Alessandro Danovi-1
# 1 Dept. of Economics, Management and Quantitative Methods / University of Bergamo, Bergamo, Italy
# 2 Health, Nutrition and Population Global Practice / World Bank, Washington DC, USA
# 3 Div. of Cardiovascular Medicine, Dept. of Medicine / Case Western Reserve University School of Medicine, Cleveland, Ohio, USA
# * Corresponding author: stefano.olgiati@unibg.it

# Adapted by:
# Stefano Olgiati, PhD, MS
# Department of Quantitative Methods, via dei Caniana 2, Bergamo (ITA) 24129; tel  +39 
# 035 20 52 638; fax +39 035 20 52 549; email <stefano.olgiati@unibg.it>

# Based on:
# Kruschke, J. K. (2014) Jags-Ymet-Xmet-Mrobust.R 

# Accompanies the book:
# Kruschke, J. K. (2014). Doing Bayesian Data Analysis: 
# A Tutorial with R, JAGS, and Stan 2nd Edition. Academic Press / Elsevier.

# Web Open Access: https://sites.google.com/site/doingbayesiandataanalysis/software-installation

source("DBDA2E-utilities.R")


#===============================================================================

data <- read.csv("TidyData.csv")
attach(data)

#===============================================================================
# INPUT AREA

year <- 2015  # 1995 or 2015
sex <- "male"    # male or female

ID <- NULL
xpoint <- NULL
ypoint <- NULL

imageCompressionFormat = c("tiff")

# OUTPUT AREA

if(year == 1995 & sex == "male") {
        title = paste("Rate of expansion of", sex, "disability across the EU -- Year", year)
        x_Name = "HALE.M.95"
        y_Name = "YLD.M.95"
        point = 1
        color = "blue"
} else if(year == 1995 & sex == "female") {
        title = paste("Rate of expansion of", sex, "disability across the EU -- Year", year)
        x_Name = "HALE.F.95"
        y_Name = "YLD.F.95"
        point = 1
        color = "red"
} else if(year == 2015 & sex == "male") {
        title = paste("Rate of expansion of", sex, "disability across the EU -- Year", year)
        x_Name = "HALE.M.15"
        y_Name = "YLD.M.15"
        point = 16
        color = "blue"
} else if(year == 2015 & sex == "female") {
        title = paste("Rate of expansion of", sex, "disability across the EU -- Year", year)
        x_Name = "HALE.F.15"
        y_Name = "YLD.F.15"
        point = 16
        color = "red"
} else {NULL}

#===============================================================================

# CHECK OUTPUT
year
sex
title
x_Name
y_Name
point
color

ID
xpoint
ypoint

#===============================================================================
# FUNCTION 1/3: DATA, MODEL, INITIALIZE AND RUN THE CHAINS
#===============================================================================

genMCMC = function( data, xName=x_Name, yName=y_Name, 
                    numSavedSteps=50000 , saveName=NULL ) {
  require(rjags)
        
#-----------------------------------------------------------------------------

  # THE DATA.
  y = data[,yName]
  x = data[,xName]
  # Do some checking that data make sense:
  if ( any( !is.finite(y) ) ) { stop("All y values must be finite.") }
  if ( any( !is.finite(x) ) ) { stop("All x values must be finite.") }
  #Ntotal = length(y)
  # Specify the data in a list, for later shipment to JAGS:
  dataList = list(
    x = x ,
    y = y 
  )
   #-----------------------------------------------------------------------------

  # THE MODEL.
  modelString = "
  
# Standardize the data:
  data {
    Ntotal <- length(y)
    xm <- mean(x)
    ym <- mean(y)
    xsd <- sd(x)
    ysd <- sd(y)
    for ( i in 1:length(y) ) {
      zx[i] <- ( x[i] - xm ) / xsd
      zy[i] <- ( y[i] - ym ) / ysd
    }
  }
  

# Specify the model for standardized data:
  model {
    for ( i in 1:Ntotal ) {
      zy[i] ~ dt( zbeta0 + zbeta1 * zx[i] , 1/zsigma^2 , nu )
    }

    # Priors vague on standardized scale:
    zbeta0 ~ dnorm( 0 , 1/(10)^2 )  
    zbeta1 ~ dnorm( 0 , 1/(10)^2 )
    zsigma ~ dunif( 1.0E-3 , 1.0E+3 )
    nu <- nuMinusOne+1
    nuMinusOne ~ dexp(1/29.0)

    # Transform to original scale:
    beta1 <- zbeta1 * ysd / xsd  
    beta0 <- zbeta0 * ysd  + ym - zbeta1 * xm * ysd / xsd 
    sigma <- zsigma * ysd
  }

  " # close quote for modelString


  # Write out modelString to a text file
  writeLines( modelString , con="TEMPmodel.txt" )
  #-----------------------------------------------------------------------------

  # INTIALIZE THE CHAINS.
  # Let JAGS do it...
  
  #-----------------------------------------------------------------------------

  # RUN THE CHAINS
  parameters = c( "beta0" ,  "beta1" ,  "sigma", 
                  "zbeta0" , "zbeta1" , "zsigma", "nu" )
  adaptSteps = 500  # Number of steps to "tune" the samplers
  burnInSteps = 1000
  nChains = 4 
  thinSteps = 1
  nIter = ceiling( ( numSavedSteps * thinSteps ) / nChains )

  # Create, initialize, and adapt the model:
  jagsModel = jags.model( "TEMPmodel.txt" , data=dataList , #inits=initsList , 
                          n.chains=nChains , n.adapt=adaptSteps )

  # Burn-in:
  cat( "Burning in the MCMC chain...\n" )
  update( jagsModel , n.iter=burnInSteps )

  # The saved MCMC chain:
  cat( "Sampling final MCMC chain...\n" )
  codaSamples = coda.samples( jagsModel , variable.names=parameters , 
                              n.iter=nIter , thin=thinSteps )

  # resulting codaSamples object has these indices: 

  #   codaSamples[[ chainIdx ]][ stepIdx , paramIdx ]
  if ( !is.null(saveName) ) {
    save( codaSamples , file=paste(saveName,"Mcmc.Rdata",sep="") )
  }
  return( codaSamples )

} # end function


#===============================================================================
# FUNCTION 2/3: SUMMARY INFO 
#===============================================================================

smryMCMC = function(  codaSamples, 
                      compValBeta0=NULL , ropeBeta0=NULL , 
                      compValBeta1=NULL , ropeBeta1=NULL , 
                      compValSigma=NULL , ropeSigma=NULL , 
                      saveName=title) {
  summaryInfo = NULL
  mcmcMat = as.matrix(codaSamples,chains=TRUE)
  summaryInfo = rbind( summaryInfo , 
                       "beta0" = summarizePost( mcmcMat[,"beta0"] , 
                                                compVal=compValBeta0 , 
                                                ROPE=ropeBeta0 ) )
  summaryInfo = rbind( summaryInfo , 
                       "beta1" = summarizePost( mcmcMat[,"beta1"] , 
                                                compVal=compValBeta1 , 
                                                ROPE=ropeBeta1 ) )
  summaryInfo = rbind( summaryInfo , 
                       "sigma" = summarizePost( mcmcMat[,"sigma"] , 
                                                compVal=compValSigma , 
                                                ROPE=ropeSigma ) )
  summaryInfo = rbind( summaryInfo , 
                       "nu" = summarizePost( mcmcMat[,"nu"] , 
                                             compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo , 
                       "log10(nu)" = summarizePost( log10(mcmcMat[,"nu"]) , 
                                             compVal=NULL , ROPE=NULL ) )
  if ( !is.null(saveName) ) {
    write.csv( summaryInfo , file=paste(saveName,"-- SummaryInfo.csv",sep="") )
  }
  return( summaryInfo )
}

#===============================================================================
# FUNCTION 3/3: PLOT
#===============================================================================

plotMCMC = function( codaSamples , data , xName=x_Name, yName=y_Name,
                     compValBeta0=NULL , ropeBeta0=NULL , 
                     compValBeta1=NULL , ropeBeta1=NULL , 
                     compValSigma=NULL , ropeSigma=NULL , 
                     showCurve=FALSE ,  pairsPlot=FALSE ,
                     saveName=title , 
                     saveType=imageCompressionFormat ) {
  # showCurve is TRUE or FALSE and indicates whether the posterior should
  #   be displayed as a histogram (by default) or by an approximate curve.
  # pairsPlot is TRUE or FALSE and indicates whether scatterplots of pairs
  #   of parameters should be displayed.
  #-----------------------------------------------------------------------------
  y = data[,yName]
  x = data[,xName]
  mcmcMat = as.matrix(codaSamples,chains=TRUE)
  chainLength = NROW( mcmcMat )
  zbeta0 = mcmcMat[,"zbeta0"]
  zbeta1 = mcmcMat[,"zbeta1"]
  zsigma = mcmcMat[,"zsigma"]
  beta0 = mcmcMat[,"beta0"]
  beta1 = mcmcMat[,"beta1"]
  sigma = mcmcMat[,"sigma"]
  nu = mcmcMat[,"nu"]
  log10nu = log10(nu)
  #-----------------------------------------------------------------------------
  if ( pairsPlot ) {
    # Plot the parameters pairwise, to see correlations:
    openGraph()
    nPtToPlot = 1000
    plotIdx = floor(seq(1,chainLength,by=chainLength/nPtToPlot))
    panel.cor = function(x, y, digits=2, prefix="", cex.cor, ...) {
      usr = par("usr"); on.exit(par(usr))
      par(usr = c(0, 1, 0, 1))
      r = (cor(x, y))
      txt = format(c(r, 0.123456789), digits=digits)[1]
      txt = paste(prefix, txt, sep="")
      if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
      text(0.5, 0.5, txt, cex=1.25 ) # was cex=cex.cor*r
    }
    pairs( cbind( beta0 , beta1 , sigma , log10nu )[plotIdx,] ,
           labels=c( expression(beta[0]) , expression(beta[1]) , 
                     expression(sigma) ,  expression(log10(nu)) ) , 
           lower.panel=panel.cor , col="skyblue" )
    if ( !is.null(saveName) ) {
      saveGraph( file=paste(saveName,"PostPairs",sep=""), type=saveType)
    }
  }
  #-----------------------------------------------------------------------------
  
  # Marginal histograms:
  # Set up window and layout:
  nPtToPlot = 1000
  plotIdx = floor(seq(1,chainLength,by=chainLength/nPtToPlot))
  openGraph(width=8,height=5)
  layout( matrix( 1:6 , nrow=2, byrow=TRUE ) )
  par( mar=c(4,4,2.5,0.5) , mgp=c(2.5,0.7,0) )
  histInfo = plotPost( beta0 , cex.lab = 1.75 , showCurve=showCurve ,
                       compVal=compValBeta0 , ROPE=ropeBeta0 ,
                       xlab=bquote(beta[0]) , main=paste("Intercept") )
  histInfo = plotPost( beta1 , cex.lab = 1.75 , showCurve=showCurve ,
                       compVal=compValBeta1 , ROPE=ropeBeta1 ,
                       xlab=bquote(beta[1]) , main=paste("Slope") )
  plot( beta1[plotIdx] , beta0[plotIdx] , 
        xlab=bquote(beta[1]) , ylab=bquote(beta[0]) ,
        col="skyblue" , cex.lab = 1.75 )
  histInfo = plotPost( sigma , cex.lab = 1.75 , showCurve=showCurve ,
                       compVal=compValSigma , ROPE=ropeSigma ,
                       xlab=bquote(sigma) , main=paste("Scale") )
  histInfo = plotPost( log10nu , cex.lab = 1.75 , showCurve=showCurve ,
                       compVal=NULL , ROPE=NULL ,
                       xlab=bquote(log10(nu)) , main=paste("Normality") )
  plot( log10nu[plotIdx] , sigma[plotIdx] , 
        xlab=bquote(log10(nu)) ,ylab=bquote(sigma) , 
        col="skyblue" , cex.lab = 1.75)
  if ( !is.null(saveName) ) {
    saveGraph( file=paste(saveName, "-- PostMarg",sep=""), type=saveType)
  }
  #-----------------------------------------------------------------------------
  
  # Data with superimposed regression lines and noise distributions:
  openGraph(width=8,height=5)
  par( mar=c(5.1, 4.1, 4.1, 2.1) , mgp=c(2,.5,0) )
  # Plot data values:
  postPredHDImass = 0.95
  xRang = max(x)-min(x)
  yRang = max(y)-min(y)
  xLimMult = 0.25
  yLimMult = 0.45
  xLim= c( min(x)-xLimMult*xRang , max(x)+xLimMult*xRang+.1)
  yLim= c( min(y)-yLimMult*yRang , max(y)+yLimMult*yRang )
  plot( x , y , cex=1 , lwd=2 , col=color , xlim=xLim , ylim=yLim ,
        xlab="Health-Adjusted Life Expectancy (HALE)" , 
        ylab="Number of Years Lived with Disease (YLD)" , 
        cex.lab=1,
        main=paste( title, "\n", "Data with Posterior Prediction and ",
                    postPredHDImass*100,
                    "% HDI",
                    sep=""),
        cex.main=1,
        axes=F,
        pch=point)
  
  axis(1, col.axis="black", las=1, cex.axis=0.7, tck=-.01)
  axis(4, col.axis="black", las=2, cex.axis=0.7, tck=-.01)
  
  
  # Superimpose a smattering of believable regression lines:
  nPredCurves=30
  xComb = seq(xLim[1],xLim[2],length=501)
  for ( i in floor(seq(from=1,to=chainLength,length=nPredCurves)) ) {
    lines( xComb , beta0[i] + beta1[i]*xComb , col="skyblue" )
  }
  
  
  
  # Superimpose some vertical distributions to indicate spread:
  # source("HDIofICDF.R")
  nSlice = 5
  curveXpos = seq(min(x),max(x),length=nSlice)
  curveWidth = (max(x)-min(x))/(nSlice+2)
  for ( i in floor(seq(from=1,to=chainLength,length=nPredCurves)) ) {
    for ( j in 1:length(curveXpos) ) {
      yHDI = HDIofICDF( qt , credMass=postPredHDImass , df=nu[i] )
      yComb = seq(yHDI[1],yHDI[2],length=75)
      xVals = dt( yComb , df=nu[i] ) 
      xVals = curveWidth * xVals / dt(0,df=nu[i])
      yPred = beta0[i] + beta1[i]*curveXpos[j] 
      yComb = yComb*sigma[i] + yPred
      lines( curveXpos[j] - xVals , yComb , col="skyblue" )
      lines( curveXpos[j] - 0*xVals , yComb , col="skyblue" , lwd=2 )
    }
  }
  
 
  # replot the data, in case they are obscured by lines:
  points( x , y , cex=1, pch=point, col=color)
  # add ID in red + label:
  # points(xpoint, ypoint, col="red", cex=1.5, pch=22, lty = "solid", lwd = 2)
  # text(xpoint, ypoint, labels=ID, cex= 0.7, pos=4)
  if ( !is.null(saveName) ) {
    saveGraph( file=paste(saveName,"-- PostPred",sep=""), type=saveType)
  }
 
  
  # if you want to show the y intercept, set this to TRUE:
  showIntercept=FALSE
  if ( showIntercept ) {
    openGraph()
    par( mar=c(3,3,2,1)+0.5 , mgp=c(2.1,0.8,0) )
    # Plot data values:
    xRang = max(x)-min(x)
    yRang = max(y)-min(y)
    xLimMult = 0.25
    yLimMult = 0.45
    xLim= c( min(x)-xLimMult*xRang , max(x)+xLimMult*xRang )
    xLim = c(0,max(xLim))
    yLim= c( min(y)-yLimMult*yRang , max(y)+yLimMult*yRang )
    nPredCurves=30
    pltIdx = floor(seq(from=1,to=chainLength,length=nPredCurves))
    intRange = range( beta0[pltIdx] )
    yLim = range( c(yLim,intRange) )
    postPredHDImass = 0.95
    plot( x , y , cex=1.5 , lwd=2 , col="black" , xlim=xLim , ylim=yLim ,
          xlab=xName , ylab=yName , cex.lab=1.5 ,
          main=paste( "Data w. Post. Pred. & ",postPredHDImass*100,"% HDI" ,sep="") , 
          cex.main=1.33  )
    abline(v=0,lty="dashed")
    # Superimpose a smattering of believable regression lines:
    xComb = seq(xLim[1],xLim[2],length=501)
    for ( i in pltIdx  ) {
      lines( xComb , beta0[i] + beta1[i]*xComb , col="skyblue" )
    }
    # Superimpose some vertical distributions to indicate spread:
    #source("HDIofICDF.R")
    nSlice = 5
    curveXpos = seq(min(x),max(x),length=nSlice)
    curveWidth = (max(x)-min(x))/(nSlice+2)
    for ( i in floor(seq(from=1,to=chainLength,length=nPredCurves)) ) {
      for ( j in 1:length(curveXpos) ) {
        yHDI = HDIofICDF( qt , credMass=postPredHDImass , df=nu[i] )
        yComb = seq(yHDI[1],yHDI[2],length=75)
        xVals = dt( yComb , df=nu[i] ) 
        xVals = curveWidth * xVals / dt(0,df=nu[i])
        yPred = beta0[i] + beta1[i]*curveXpos[j] 
        yComb = yComb*sigma[i] + yPred
        lines( curveXpos[j] - xVals , yComb , col="skyblue" )
        lines( curveXpos[j] - 0*xVals , yComb , col="skyblue" , lwd=2 )
      }
    }
    # replot the data, in case they are obscured by lines:
    points( x , y , cex=1, pch=point, col=color)
    if ( !is.null(saveName) ) {
      saveGraph( file=paste(saveName,"-- PostPredInt",sep=""), type=saveType)
    }
  }
}

#-----------------------------------------------------------------------------


