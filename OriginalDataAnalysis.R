rm(list=ls()); gc();

### FEF ###

setwd('C:/Users/heath/OneDrive/Project Report/Data/FEF')

participantsFiles <- dir( pattern = 'sub-')
dfFEF <- c()

for ( i in 1:length( participantsFiles ) ) {
  
  load( participantsFiles[ i ] )
  dfSubj <- dfOut
  dfSubjLH <- dfSubj[ dfSubj$hemi=='LH' , 10:17 ]
  dfSubjRH <- dfSubj[ dfSubj$hemi=='RH' , 10:17 ]
  
  dfSubjLHAvg <- apply( dfSubjLH, 2, mean )
  dfSubjRHAvg <- apply( dfSubjRH, 2, mean )
  subjData <- rep( dfSubj$subj[1], length( dfSubjLHAvg ) )
  corticalDeptData <- round( seq( 0,1, length.out=length( dfSubjLHAvg ) ), 2 )
  lhData <- rep( 'LH', length( dfSubjLHAvg ) )
  rhData <- rep( 'RH', length( dfSubjLHAvg ) )
  roiData <- rep('FEF', length( dfSubjLHAvg ) )
  
  dataTemp <- data.frame( rep( subjData, 2 ), rep( corticalDeptData, 2 ), c( dfSubjLHAvg, dfSubjRHAvg ), c( lhData, rhData ), rep( roiData, 2 ) )
  names( dataTemp ) <- c('subject','corticalDepth','T1w','hemi','roi')
  
  dfFEF <- rbind( dfFEF, dataTemp )
  dfFEF
}


###SEF###

setwd('C:/Users/heath/OneDrive/Project Report/Data/SEF')

participantsFiles <- dir( pattern = 'sub-')
dfSEF <- c()

for ( i in 1:length( participantsFiles ) ) {
  
  load( participantsFiles[ i ] )
  dfSubj <- dfOut
  dfSubjLH <- dfSubj[ dfSubj$hemi=='LH' , 10:17 ]
  dfSubjRH <- dfSubj[ dfSubj$hemi=='RH' , 10:17 ]
  
  dfSubjLHAvg <- apply( dfSubjLH, 2, mean )
  dfSubjRHAvg <- apply( dfSubjRH, 2, mean )
  subjData <- rep( dfSubj$subj[1], length( dfSubjLHAvg ) )
  corticalDeptData <- round( seq( 0,1, length.out=length( dfSubjLHAvg ) ), 2 )
  lhData <- rep( 'LH', length( dfSubjLHAvg ) )
  rhData <- rep( 'RH', length( dfSubjLHAvg ) )
  roiData <- rep('SEF', length( dfSubjLHAvg ) )
  
  dataTemp <- data.frame( rep( subjData, 2 ), rep( corticalDeptData, 2 ), c( dfSubjLHAvg, dfSubjRHAvg ), c( lhData, rhData ), rep( roiData, 2 ) )
  names( dataTemp ) <- c('subject','corticalDepth','T1w','hemi','roi')
  
  dfSEF <- rbind( dfSEF, dataTemp )
  dfSEF
  
}


###PEF###

setwd('C:/Users/heath/OneDrive/Project Report/Data/PEF')


participantsFiles <- dir( pattern = 'sub-')
dfPEF <- c()

for ( i in 1:length( participantsFiles ) ) {
  
  load( participantsFiles[ i ] )
  dfSubj <- dfOut
  dfSubjLH <- dfSubj[ dfSubj$hemi=='LH' , 10:17 ]
  dfSubjRH <- dfSubj[ dfSubj$hemi=='RH' , 10:17 ]
  
  dfSubjLHAvg <- apply( dfSubjLH, 2, mean )
  dfSubjRHAvg <- apply( dfSubjRH, 2, mean )
  subjData <- rep( dfSubj$subj[1], length( dfSubjLHAvg ) )
  corticalDeptData <- round( seq( 0,1, length.out=length( dfSubjLHAvg ) ), 2 )
  lhData <- rep( 'LH', length( dfSubjLHAvg ) )
  rhData <- rep( 'RH', length( dfSubjLHAvg ) )
  roiData <- rep('PEF', length( dfSubjLHAvg ) )
  
  dataTemp <- data.frame( rep( subjData, 2 ), rep( corticalDeptData, 2 ), c( dfSubjLHAvg, dfSubjRHAvg ), c( lhData, rhData ), rep( roiData, 2 ) )
  names( dataTemp ) <- c('subject','corticalDepth','T1w','hemi','roi')
  
  dfPEF <- rbind( dfPEF, dataTemp )
  dfPEF
  
}

  
###V1###

setwd('C:/Users/heath/OneDrive/Project Report/Data/V1')

participantsFiles <- dir( pattern = 'sub-')
dfV1 <- c()

for ( i in 1:length( participantsFiles ) ) {
  
  load( participantsFiles[ i ] )
  dfSubj <- dfOut
  dfSubjLH <- dfSubj[ dfSubj$hemi=='LH' , 10:17 ]
  dfSubjRH <- dfSubj[ dfSubj$hemi=='RH' , 10:17 ]
  
  dfSubjLHAvg <- apply( dfSubjLH, 2, mean )
  dfSubjRHAvg <- apply( dfSubjRH, 2, mean )
  subjData <- rep( dfSubj$subj[1], length( dfSubjLHAvg ) )
  corticalDeptData <- round( seq( 0,1, length.out=length( dfSubjLHAvg ) ), 2 )
  lhData <- rep( 'LH', length( dfSubjLHAvg ) )
  rhData <- rep( 'RH', length( dfSubjLHAvg ) )
  roiData <- rep('V1', length( dfSubjLHAvg ) )
  
  dataTemp <- data.frame( rep( subjData, 2 ), rep( corticalDeptData, 2 ), c( dfSubjLHAvg, dfSubjRHAvg ), c( lhData, rhData ), rep( roiData, 2 ) )
  names( dataTemp ) <- c('subject','corticalDepth','T1w','hemi','roi')
  
  dfV1 <- rbind( dfV1, dataTemp )
  dfV1
  
}


###V2###

setwd('C:/Users/heath/OneDrive/Project Report/Data/V2')

participantsFiles <- dir( pattern = 'sub-')
dfV2 <- c()

for ( i in 1:length( participantsFiles ) ) {
  
  load( participantsFiles[ i ] )
  dfSubj <- dfOut
  dfSubjLH <- dfSubj[ dfSubj$hemi=='LH' , 10:17 ]
  dfSubjRH <- dfSubj[ dfSubj$hemi=='RH' , 10:17 ]
  
  dfSubjLHAvg <- apply( dfSubjLH, 2, mean )
  dfSubjRHAvg <- apply( dfSubjRH, 2, mean )
  subjData <- rep( dfSubj$subj[1], length( dfSubjLHAvg ) )
  corticalDeptData <- round( seq( 0,1, length.out=length( dfSubjLHAvg ) ), 2 )
  lhData <- rep( 'LH', length( dfSubjLHAvg ) )
  rhData <- rep( 'RH', length( dfSubjLHAvg ) )
  roiData <- rep('V2', length( dfSubjLHAvg ) )
  
  dataTemp <- data.frame( rep( subjData, 2 ), rep( corticalDeptData, 2 ), c( dfSubjLHAvg, dfSubjRHAvg ), c( lhData, rhData ), rep( roiData, 2 ) )
  names( dataTemp ) <- c('subject','corticalDepth','T1w','hemi','roi')
  
  dfV2 <- rbind( dfV2, dataTemp )
  dfV2
  
}


###V3###

setwd('C:/Users/heath/OneDrive/Project Report/Data/V3')

participantsFiles <- dir( pattern = 'sub-')
dfV3 <- c()

for ( i in 1:length( participantsFiles ) ) {
  
  load( participantsFiles[ i ] )
  dfSubj <- dfOut
  dfSubjLH <- dfSubj[ dfSubj$hemi=='LH' , 10:17 ]
  dfSubjRH <- dfSubj[ dfSubj$hemi=='RH' , 10:17 ]
  
  dfSubjLHAvg <- apply( dfSubjLH, 2, mean )
  dfSubjRHAvg <- apply( dfSubjRH, 2, mean )
  subjData <- rep( dfSubj$subj[1], length( dfSubjLHAvg ) )
  corticalDeptData <- round( seq( 0,1, length.out=length( dfSubjLHAvg ) ), 2 )
  lhData <- rep( 'LH', length( dfSubjLHAvg ) )
  rhData <- rep( 'RH', length( dfSubjLHAvg ) )
  roiData <- rep('V3', length( dfSubjLHAvg ) )
  
  dataTemp <- data.frame( rep( subjData, 2 ), rep( corticalDeptData, 2 ), c( dfSubjLHAvg, dfSubjRHAvg ), c( lhData, rhData ), rep( roiData, 2 ) )
  names( dataTemp ) <- c('subject','corticalDepth','T1w','hemi','roi')
  
  dfV3 <- rbind( dfV3, dataTemp )
  dfV3
  
}


###V3A###

setwd('C:/Users/heath/OneDrive/Project Report/Data/V3A')

participantsFiles <- dir( pattern = 'sub-')
dfV3A <- c()

for ( i in 1:length( participantsFiles ) ) {
  
  load( participantsFiles[ i ] )
  dfSubj <- dfOut
  dfSubjLH <- dfSubj[ dfSubj$hemi=='LH' , 10:17 ]
  dfSubjRH <- dfSubj[ dfSubj$hemi=='RH' , 10:17 ]
  
  dfSubjLHAvg <- apply( dfSubjLH, 2, mean )
  dfSubjRHAvg <- apply( dfSubjRH, 2, mean )
  subjData <- rep( dfSubj$subj[1], length( dfSubjLHAvg ) )
  corticalDeptData <- round( seq( 0,1, length.out=length( dfSubjLHAvg ) ), 2 )
  lhData <- rep( 'LH', length( dfSubjLHAvg ) )
  rhData <- rep( 'RH', length( dfSubjLHAvg ) )
  roiData <- rep('V3A', length( dfSubjLHAvg ) )
  
  dataTemp <- data.frame( rep( subjData, 2 ), rep( corticalDeptData, 2 ), c( dfSubjLHAvg, dfSubjRHAvg ), c( lhData, rhData ), rep( roiData, 2 ) )
  names( dataTemp ) <- c('subject','corticalDepth','T1w','hemi','roi')
  
  dfV3A <- rbind( dfV3A, dataTemp )
  dfV3A
  
}


###V3B###

setwd('C:/Users/heath/OneDrive/Project Report/Data/V3B')

participantsFiles <- dir( pattern = 'sub-')
dfV3B <- c()

for ( i in 1:length( participantsFiles ) ) {
  
  load( participantsFiles[ i ] )
  dfSubj <- dfOut
  dfSubjLH <- dfSubj[ dfSubj$hemi=='LH' , 10:17 ]
  dfSubjRH <- dfSubj[ dfSubj$hemi=='RH' , 10:17 ]
  
  dfSubjLHAvg <- apply( dfSubjLH, 2, mean )
  dfSubjRHAvg <- apply( dfSubjRH, 2, mean )
  subjData <- rep( dfSubj$subj[1], length( dfSubjLHAvg ) )
  corticalDeptData <- round( seq( 0,1, length.out=length( dfSubjLHAvg ) ), 2 )
  lhData <- rep( 'LH', length( dfSubjLHAvg ) )
  rhData <- rep( 'RH', length( dfSubjLHAvg ) )
  roiData <- rep('V3B', length( dfSubjLHAvg ) )
  
  dataTemp <- data.frame( rep( subjData, 2 ), rep( corticalDeptData, 2 ), c( dfSubjLHAvg, dfSubjRHAvg ), c( lhData, rhData ), rep( roiData, 2 ) )
  names( dataTemp ) <- c('subject','corticalDepth','T1w','hemi','roi')
  
  dfV3B <- rbind( dfV3B, dataTemp )
  dfV3B
  
}


###Analysis###

outDf <- rbind( dfFEF, dfPEF, dfSEF, dfV1, dfV2, dfV3, dfV3A, dfV3B )

aggregateDf <- aggregate( T1w ~ corticalDepth*roi, mean, data=outDf  )
aggregateDf$sd <- aggregate( T1w ~ corticalDepth*roi, sd, data=outDf  )$T1w

outDf <- rbind( dfSEF, dfFEF, dfPEF )
outDf$roi <- relevel( as.factor( outDf$roi ), ref='FEF' ) 
outDf$corticalDepthCenter <- outDf$corticalDepth-0.5 
summary( lm( T1w ~ corticalDepthCenter * roi, data=outDf  ) )

outDf <- rbind( dfV1, dfV2, dfV3, dfV3A, dfV3B )
outDf$roi <- relevel( as.factor( outDf$roi ), ref='V1' ) 
outDf$corticalDepthCenter <- outDf$corticalDepth-0.5 
summary( lm( T1w ~ corticalDepthCenter * roi, data=outDf  ) )


###Visualise###

X11()
plotData <- subset( aggregateDf, roi=='FEF' )
plot( T1w ~ corticalDepth, data=plotData, bty='n', las=1, ylim=c(1000, 2000 ), type = "o", pch=20, cex=2, col = "red", xlab = "Cortical Depth", ylab = "T1-weighted Signal")
segments( plotData$corticalDepth, plotData$T1w+plotData$sd/2, plotData$corticalDepth, plotData$T1w-plotData$sd/2, col='red' )
plotData <- subset( aggregateDf, roi=='SEF' )
points( plotData$corticalDepth-0.01, plotData$T1w, type = "o", pch=20, cex=2, col = "blue")
segments( plotData$corticalDepth-0.01, plotData$T1w+plotData$sd/2, plotData$corticalDepth-0.01, plotData$T1w-plotData$sd/2, col='blue' )
plotData <- subset( aggregateDf, roi=='PEF' )
points( plotData$corticalDepth+0.01, plotData$T1w, type = "o", pch=20, cex=2, col = "green" )
segments( plotData$corticalDepth+0.01, plotData$T1w+plotData$sd/2, plotData$corticalDepth+0.01, plotData$T1w-plotData$sd/2, col='green' )
title(main = "T1w Signal at each Cortical Depth in the frontal lobe ROIs",
      font.main = 1)
legend(x = "topright",
       legend = c("FEF", "SEF", "PEF"),
       fill = 2:4,
       title = "ROIs")

X11()
plotData <- subset( aggregateDf, roi=='V1' )
plot( T1w ~ corticalDepth, data=plotData, bty='n', las=1, ylim=c(1000, 2000 ), type = "o", pch=20, cex=2, col = "red", xlab = "Cortical Depth", ylab = "T1-weighted Signal")
segments( plotData$corticalDepth, plotData$T1w+plotData$sd/2, plotData$corticalDepth, plotData$T1w-plotData$sd/2, col='red' )
plotData <- subset( aggregateDf, roi=='V2' )
points( plotData$corticalDepth+0.01, plotData$T1w, type = "o", pch=20, cex=2, col = "blue" )
segments( plotData$corticalDepth+0.01, plotData$T1w+plotData$sd/2, plotData$corticalDepth+0.01, plotData$T1w-plotData$sd/2, col='blue' )
plotData <- subset( aggregateDf, roi=='V3' )
points( plotData$corticalDepth-0.01, plotData$T1w, type = "o", pch=20, cex=2, col = "green" )
segments( plotData$corticalDepth-0.01, plotData$T1w+plotData$sd/2, plotData$corticalDepth-0.01, plotData$T1w-plotData$sd/2, col='green' )
plotData <- subset( aggregateDf, roi=='V3A' )
points( plotData$corticalDepth+0.02, plotData$T1w, type = "o", pch=20, cex=2, col = "pink" )
segments( plotData$corticalDepth+0.02, plotData$T1w+plotData$sd/2, plotData$corticalDepth+0.02, plotData$T1w-plotData$sd/2, col='pink' )
plotData <- subset( aggregateDf, roi=='V3B' )
points( plotData$corticalDepth-0.02, plotData$T1w, type = "o", pch=20, cex=2, col = "orange" )
segments( plotData$corticalDepth-0.02, plotData$T1w+plotData$sd/2, plotData$corticalDepth-0.02, plotData$T1w-plotData$sd/2, col='orange' )
title(main = "T1w Signal at each Cortical Depth in the EVC ROIs",
      font.main = 1)
legend(x = "topright",
       legend = c("V1", "V2", "V3", "V3A", "V3B"),
       fill = c("red", "blue", "green", "pink", "orange"),
       title = "ROIs")
