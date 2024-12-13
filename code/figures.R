# working area ####
rm(list=ls())

main_dir = '/home/josema/Desktop/1. Work/1 research/PhD Antwerp/#thesis/paper2/paper2_manuscript/images/figures/'



# functions ####
colorArea <- function(from, to, density, ..., col="lightgray", dens=NULL){
  y_seq = seq(from, to, length.out=500)
  d = c(0, density(y_seq, ...), 0)
  polygon(c(from, y_seq, to), d, col=col, density=dens, border=NA)
}



# figure 1 ####

# distributional parameters
dpA = c(0, 0.5) # distribution text A (mean, sd)
dpB = c(1, 1) # distribution text B (mean, sd)
rAB = 0 # correlation between discriminal processes

# figure
png( filename=file.path(main_dir, 'discriminal_process.png'), 
     height=10, width=15, units='cm', res=400  )

# initial figure
par( mar=c(2,0,0.5,0) )
plot( NULL, xlim=c(-2,5), ylim=c(0,0.9), 
      xaxt='n', yaxt='n', xlab='', ylab='', axes=F )
axis( side=1, labels=F, lwd.ticks=0 )
axis( side=1, at=c(dpA[1], dpB[1]), tick=F, 
      label=c( expression( S[A] ), expression( S[B] ) ) )

# figure parameters
pplotA = c( dnorm( dpA[1], mean=dpA[1], sd=dpA[2] ),
            dnorm( dpA[1], mean=dpA[1], sd=dpA[2] ) + 0.05,
            dnorm( dpA[1] + dpA[2], mean=dpA[1], sd=dpA[2]),
            dnorm( dpA[1]+0.25, mean=dpA[1], sd=dpA[2] )-0.25 )

pplotB = c( dnorm( dpB[1], mean=dpB[1], sd=dpB[2] ),
            dnorm( dpB[1], mean=dpB[1], sd=dpB[2] ) + 0.05,
            dnorm( dpB[1] + dpB[2], mean=dpB[1], sd=dpB[2]),
            dnorm( dpB[1]+0.45, mean=dpB[1], sd=dpB[2] )-0.15 )


# discriminal process text A
curve( dnorm(x, mean=dpA[1], sd=dpA[2]), lwd=1.5, add=T ) 
text( x=dpA[1], y=pplotA[2], cex=0.8, 'Text A' ) 
lines( x=c(dpA[1], dpA[1]), y=c(pplotA[1], -0.1), lty=2, lwd=0.7 )
lines( x=c(dpA[1], dpA[1]+dpA[2]), y=rep( pplotA[3], 2), lty=2, lwd=0.7 )
text( x=dpA[1]+0.25, y=pplotA[4], cex=0.8, expression( sigma[A] ) ) 

# discriminal process text B
curve( dnorm(x, mean=dpB[1], sd=dpB[2]), lwd=1.5, add=T ) 
text( x=dpB[1], y=pplotB[2], cex=0.8, 'Text B' ) 
lines( x=c(dpB[1], dpB[1]), y=c(pplotB[1], -0.1), lty=2, lwd=0.7 )
lines( x=c(dpB[1], dpB[1]+dpB[2]), y=rep( pplotB[3], 2), lty=2, lwd=0.7 )
text( x=dpB[1]+0.45, y=pplotB[4], cex=0.8, expression( sigma[B] ) ) 

dev.off()



# figure 2 ####

# distributional parameters
dd = c( dpB[1]-dpA[1], sqrt( dpB[2]^2 + dpA[2]^2 - 2*rAB*dpB[2]*dpA[2] ) )

# figure
png( filename=file.path(main_dir, 'discriminal_difference.png'), 
     height=10, width=15, units='cm', res=400  )

# initial figure
par( mar=c(2,0,0.5,0) )
plot( NULL, xlim=c(-2,5), ylim=c(0,0.4), 
      xaxt='n', yaxt='n', xlab='', ylab='', axes=F )
axis( side=1, labels=F, lwd.ticks=0 )
axis( side=1, at=c(0, dd[1]), tick=F, 
      label=c(0, expression( S[B] - S[A] ) ) )

# figure parameters
pplotD = c( dnorm( 0, mean=dd[1], sd=dd[2] ),
            dnorm( dd[1], mean=dd[1], sd=dd[2] ),
            dnorm( dd[1]+2*dd[2], mean=dd[1], sd=dd[2]),
            dnorm( dd[1], mean=dd[1], sd=dd[2] ) + 0.05,
            dnorm( dd[1]+0.4, mean=dd[1], sd=dd[2] )-0.14 )

# discriminal difference
colorArea( from=0, to=7, density=dnorm, mean=dd[1], sd=dd[2], col=rgb(0,0,0,0.05) )
curve( dnorm(x, mean=dd[1], sd=dd[2]), lwd=1.5, add=T ) 
lines( x=c(0,0), y=c(pplotD[1],-0.1), lty=2, lwd=0.7 )
text( x=0.45, y=0.03, cex=0.8, expression( P(B>A) ) ) 
lines( x=c(dd[1], dd[1]), y=c(pplotD[2], -0.1), lty=2, lwd=0.7 )
lines( x=c(dd[1], dd[1]+2*dd[2]), y=rep( pplotD[3], 2), lty=2, lwd=0.7 )
text( x=dd[1]+1, y=pplotD[3]-0.02, cex=0.8, expression( sigma[BA] ) ) 
text( x=dd[1]+1.7, y=0.25, cex=0.8, 'where:' )
text( x=dd[1]+2.4, y=0.22, cex=0.8,
      expression( sigma[BA] == sqrt( sigma[B]^2 + sigma[A]^2 - rho*sigma[B]*sigma[A]) ) )

dev.off()





# figure XX ####
png( filename=file.path(main_dir, 'density.png'), 
     height=10, width=15, units='cm', res=400  )

# initial figure
par( mar=c(2,0,0.5,0) )
plot( NULL, xlim=c(-6,6), ylim=c(0,0.25), 
      xaxt='n', yaxt='n', xlab='', ylab='', axes=F )
axis( side=1, labels=F, lwd.ticks=0 )

# distributions
curve( dlogis(x, location=0, scale=1), lwd=1.5, add=T ) 
curve( dnorm(x, mean=0, sd=1.7), lwd=1.5, lty=2, add=T ) 
legend('topleft', lwd=rep(1.5,2), lty=c(1,2), bty='n',
       legend=c('Logistic', expression('Normal ' (sigma== 1.7) ) ) )

dev.off()



# figure XX ####
png( filename=file.path(main_dir, 'cummulative.png'), 
     height=10, width=15, units='cm', res=400  )

# initial figure
par( mar=c(2,0,0.5,0) )
plot( NULL, xlim=c(-6,6), ylim=c(0,1), 
      xaxt='n', yaxt='n', xlab='', ylab='', axes=F )
axis( side=1, labels=F, lwd.ticks=0 )

# distributions
curve( plogis(x, location=0, scale=1), lwd=1.5, add=T ) 
curve( pnorm(x, mean=0, sd=1.7), lwd=1.5, lty=2, add=T ) 
legend('topleft', lwd=rep(1.5,2), lty=c(1,2), bty='n',
       legend=c('Logistic', expression('Normal ' (sigma== 1.7) ) ) )

dev.off()




# figure 3 ####
# distributional parameters
dpB_d = round( seq(0.5, 1.5, by=0.25), 2 ) # correlation between discriminal processes
dd = list()
dd[[1]] = dpB[1]-dpA[1]
dd[[2]] = round( sqrt( dpB_d^2 + dpA[2]^2 - 2*rAB*dpB_d*dpA[2] ), 2 )


png( filename=file.path(main_dir, 'dispersion.png'),
     height=10, width=15, units='cm', res=400  )

# initial figure
par( mar=c(2,0,0.5,0) )
plot( NULL, xlim=c(-2,5), ylim=c(0,0.6),
      xaxt='n', yaxt='n', xlab='', ylab='', axes=F )
axis( side=1, labels=F, lwd.ticks=0 )
axis( side=1, at=0, tick=F, label='0' )
axis( side=1, at=c(0, dd[1]), tick=F, 
      label=c(0, expression( S[B] - S[A] ) ) )

# figure parameters
l_ty = c( 1, rep(2,2), rep(3,2) )
l_wd = c( 1.5, seq(1,0.5,length.out=4) )
text = paste0( ' ~ sigma[B] - sigma[A] == ', dpB_d - dpA[2] )
pplotD = c( dnorm( 0, mean=dd[[1]], sd=dd[[2]][1] ),
            dnorm( dd[[1]], mean=dd[[1]], sd=dd[[2]][1] ),
            dnorm( dd[[1]]+2*dd[[2]][1], mean=dd[[1]], sd=dd[[2]][1] ),
            dnorm( dd[[1]]+0.4, mean=dd[[1]], sd=dd[[2]][1] )-0.14 )


# distributions
colorArea( from=0, to=7, density=dnorm, mean=dd[[1]], sd=dd[[2]][1], col=rgb(0,0,0,0.05) )
for(i in 1:length(dd[[2]])){
  # colorArea( from=0, to=7, density=dnorm, mean=dd[[1]], sd=dd[[2]][i], col=rgb(0,0,0,0.05) )
  curve( dnorm(x, mean=dd[[1]], sd=dd[[2]][i]), lwd=l_wd[i], lty=l_ty[i], add=T )
}
lines( x=c(0,0), y=c(pplotD[1],-0.1), lty=2, lwd=0.7 )
text( x=0.45, y=0.03, cex=0.8, expression( P(B>A) ) ) 
lines( x=c(dd[[1]], dd[[1]]), y=c(pplotD[2], -0.1), lty=2, lwd=0.7 )
lines( x=c(dd[[1]], dd[[1]]+2*dd[[2]][1]), y=rep(pplotD[3], 2), lty=2, lwd=0.7 )
text( x=dd[[1]]+0.65, y=0.05, cex=0.8, expression( sigma[BA] ) ) 
text( x=dd[[1]]+1.7, y=0.26, cex=0.8, 'where:' )
text( x=dd[[1]]+2.45, y=0.22, cex=0.8,
      expression( sigma[BA] == sqrt( sigma[B]^2 + sigma[A]^2 - 2*rho*sigma[B]*sigma[A]) ) )
legend('topleft', lwd=l_wd, lty=l_ty, bty='n', legend=parse(text=text) )

dev.off()




# figure 4 ####
# distributional parameters
rAB = round( seq(-0.5, 0.5, by=0.25), 2 ) # correlation between discriminal processes
dd = list()
dd[[1]] = dpB[1]-dpA[1]
dd[[2]] = round( sqrt( dpB[2]^2 + dpA[2]^2 - 2*rAB*dpB[2]*dpA[2] ), 2 )


png( filename=file.path(main_dir, 'correlation.png'),
     height=10, width=15, units='cm', res=400  )

# initial figure
par( mar=c(2,0,0.5,0) )
plot( NULL, xlim=c(-2,5), ylim=c(0,0.5),
      xaxt='n', yaxt='n', xlab='', ylab='', axes=F )
axis( side=1, labels=F, lwd.ticks=0 )
axis( side=1, at=0, tick=F, label='0' )
axis( side=1, at=c(0, dd[1]), tick=F, 
      label=c(0, expression( S[B] - S[A] ) ) )

# figure parameters
l_ty = c( rep(3,2), 1, rep(2,2) )
l_wd = c(seq(0.5,1,length.out=2), 1.5, seq(1,0.5,length.out=2) )
text = paste0( ' ~ rho == ', rAB )
pplotD = c( dnorm( 0, mean=dd[[1]], sd=dd[[2]][3] ),
            dnorm( dd[[1]], mean=dd[[1]], sd=dd[[2]][5] ),
            dnorm( dd[[1]]+2*dd[[2]][3], mean=dd[[1]], sd=dd[[2]][3] ),
            dnorm( dd[[1]]+0.4, mean=dd[[1]], sd=dd[[2]][1] )-0.14 )

# distributions
colorArea( from=0, to=7, density=dnorm, mean=dd[[1]], sd=dd[[2]][3], col=rgb(0,0,0,0.05) )
for(i in 1:length(dd[[2]])){
  # colorArea( from=0, to=7, density=dnorm, mean=dd[[1]], sd=dd[[2]][i], col=rgb(0,0,0,0.05) )
  curve( dnorm(x, mean=dd[[1]], sd=dd[[2]][i]), lwd=l_wd[i], lty=l_ty[i], add=T )
}
lines( x=c(0,0), y=c(pplotD[1],-0.1), lty=2, lwd=0.7 )
text( x=0.45, y=0.03, cex=0.8, expression( P(B>A) ) ) 
lines( x=c(dd[[1]], dd[[1]]), y=c(pplotD[2], -0.1), lty=2, lwd=0.7 )
lines( x=c(dd[[1]], dd[[1]]+2*dd[[2]][3]), y=rep(pplotD[3], 2), lty=2, lwd=0.7 )
text( x=dd[[1]]+0.8, y=0.025, cex=0.8, expression( sigma[BA] ) ) 
text( x=dd[[1]]+1.7, y=0.26, cex=0.8, 'where:' )
text( x=dd[[1]]+2.45, y=0.22, cex=0.8,
      expression( sigma[BA] == sqrt( sigma[B]^2 + sigma[A]^2 - 2*rho*sigma[B]*sigma[A]) ) )
legend('topleft', lwd=l_wd, lty=l_ty, bty='n', legend=parse(text=text) )

dev.off()