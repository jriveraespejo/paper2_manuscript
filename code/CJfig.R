# working area ####
rm(list=ls())

main_dir = '/home/josema/Desktop/1. Work/1 research/PhD Antwerp/#thesis/paper2/paper2_manuscript/images/png/'

require(rethinking)
require(tidyverse)
require(metafor)



# functions ####
colorArea <- function(from, to, density, ..., col="lightgray", dens=NULL){
  y_seq = seq(from, to, length.out=500)
  d = c(0, density(y_seq, ...), 0)
  polygon(c(from, y_seq, to), d, col=col, density=dens, border=NA)
}

sim_me = function( n=100, b=0.3, s1=1, s2=0.8){
  
  # # test
  # n = 100
  # b = 0.2
  # s1 = 1
  # s2 = 0.8

  # simulation
  x = rnorm( n=n, mean=0, sd=1 )
  mu_y = b*x
  y = rnorm( n=n, mean=mu_y, sd=s1 )
  y_star = rnorm( n=n, mean=y, sd=s2 )
  d = data.frame( obs=1:n, y, y_star, y_sd=s2, x)
  
  # regressions
  reg1 = lm( y ~ x, data=d )
  reg2 = lm( y_star ~ x, data=d )

  # saving coefficients
  res = c( coef( reg1 )[2], coef( reg2 )[2] )
  
  # return object
  return(res)
  
}



# figure 1 ####

## figure 1a ####

# distributional parameters
dpA = c(0, 0.5) # distribution text A (mean, sd)
dpB = c(1, 1) # distribution text B (mean, sd)
rAB = 0 # correlation between discriminal processes


png( filename=file.path(main_dir, 'discriminal_process.png'), 
     height=10, width=15, units='cm', res=400  )

# initial figure
par( mar=c(2,0,0.5,0) )
plot( NULL, xlim=c(-2,5), ylim=c(0,0.9), 
      xaxt='n', yaxt='n', xlab='', ylab='', axes=F )
axis( side=1, labels=F, lwd.ticks=0 )
axis( side=1, at=c(dpA[1], dpB[1]), tick=F, 
      label=c( expression( T[A] ), expression( T[B] ) ) )

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



## figure 1b ####

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
      label=c(0, expression( T[B] - T[A] ) ) )

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
      expression( sigma[BA] == sqrt( sigma[B]^2 + sigma[A]^2 - 2*rho*sigma[B]*sigma[A]) ) )

dev.off()










# figure 2 ####

## figure 2a ####

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
      label=c(0, expression( T[B] - T[A] ) ) )

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




## figure 2b ####

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
      label=c(0, expression( T[B] - T[A] ) ) )

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






# figure 3 ####

# simulation
b = 0.2
s2 = c(0.3, 0.6, 1)
dlist = list()
for( s in seq_along(s2) ){
  dlist[[s]] = t( replicate( n=2000, sim_me( b=b, s2=s2[s]) ) )
  dlist[[s]] = data.frame( dlist[[s]] )
  names(dlist[[s]]) = c('true', 'mean')
  dlist[[s]] = c( sapply(dlist[[s]], mean), sapply(dlist[[s]], sd) )
}
names(dlist) = c('s2.1','s2.2','s2.3')
# str(dlist)


## figure 3 ####

# plot
png( filename=file.path(main_dir, 'measurement_error.png'),
     height=9, width=25, units='cm', res=400  )

par(mfrow=c(1,3))

with(dlist, {
  curve( dnorm( x, mean=s2.1[1], sd=s2.1[3]), 
         lty=2, lwd=1, xlim=c(-0.5,1), ylab='', xlab='X', 
         main=bquote( n == 100 ~ ',' ~ beta[X] == .(b) ~ ',' ~ sigma[T] == .(s2[1]) ) )
  curve( dnorm( x, mean=s2.1[2], sd=s2.1[4]), lty=1, lwd=1.5, xlim=c(-0.5,1), add=T )
  colorArea( from=0, to=1, density=dnorm, mean=s2.1[2], sd=s2.1[4], col=rgb(0,0,0,0.1) )
  abline( v=b, lty=2, lwd=0.5 )
  legend( 'topleft', legend=c('true','with m.e.'),
          bty='n', lty=c(2,1), lwd=c(1,1.5) )
})

with(dlist, {
  curve( dnorm( x, mean=s2.2[1], sd=s2.2[3]), 
         lty=2, lwd=1, xlim=c(-0.5,1), ylab='', xlab='X', 
         main=bquote( n == 100 ~ ',' ~ beta[X] == .(b) ~ ',' ~ sigma[T] == .(s2[2]) ) )
  curve( dnorm( x, mean=s2.2[2], sd=s2.2[4]), lty=1, lwd=1.5, xlim=c(-0.5,1), add=T )
  colorArea( from=0, to=1, density=dnorm, mean=s2.2[2], sd=s2.2[4], col=rgb(0,0,0,0.1) )
  abline( v=b, lty=2, lwd=0.5 )
  legend( 'topleft', legend=c('true','with m.e.'),
          bty='n', lty=c(2,1), lwd=c(1,1.5) )
})

with(dlist, {
  curve( dnorm( x, mean=s2.3[1], sd=s2.3[3]), 
         lty=2, lwd=1, xlim=c(-0.5,1), ylab='', xlab='X', 
         main=bquote( n == 100 ~ ',' ~ beta[X] == .(b) ~ ',' ~ sigma[T] == .(s2[3]) ) )
  curve( dnorm( x, mean=s2.3[2], sd=s2.3[4]), lty=1, lwd=1.5, xlim=c(-0.5,1), add=T )
  colorArea( from=0, to=1, density=dnorm, mean=s2.3[2], sd=s2.3[4], col=rgb(0,0,0,0.1) )
  abline( v=b, lty=2, lwd=0.5 )
  legend( 'topleft', legend=c('true','with m.e.'),
          bty='n', lty=c(2,1), lwd=c(1,1.5) )
})

par(mfrow=c(1,1))

dev.off()



## figure 3a ####

png( filename=file.path(main_dir, 'measurement_error.png'),
     height=13, width=13, units='cm', res=400  )
 
with(dlist, {
  curve( dnorm( x, mean=s2.1[1], sd=s2.1[3]), 
         lty=2, lwd=1, xlim=c(-0.5,1), ylab='', xlab='X', 
         main=bquote( n == 100 ~ ',' ~ beta[X] == .(b) ~ ',' ~ sigma[T] == .(s2[1]) ) )
  curve( dnorm( x, mean=s2.1[2], sd=s2.1[4]), lty=1, lwd=1.5, xlim=c(-0.5,1), add=T )
  colorArea( from=0, to=1, density=dnorm, mean=s2.1[2], sd=s2.1[4], col=rgb(0,0,0,0.1) )
  abline( v=b, lty=2, lwd=0.5 )
  legend( 'topleft', legend=c('true','with m.e.'),
          bty='n', lty=c(2,1), lwd=c(1,1.5) )
  })

dev.off()


## figure 3b ####

png( filename=file.path(main_dir, 'measurement_error2.png'),
     height=13, width=13, units='cm', res=400  )

with(dlist, {
  curve( dnorm( x, mean=s2.2[1], sd=s2.2[3]), 
         lty=2, lwd=1, xlim=c(-0.5,1), ylab='', xlab='X', 
         main=bquote( n == 100 ~ ',' ~ beta[X] == .(b) ~ ',' ~ sigma[T] == .(s2[2]) ) )
  curve( dnorm( x, mean=s2.2[2], sd=s2.2[4]), lty=1, lwd=1.5, xlim=c(-0.5,1), add=T )
  colorArea( from=0, to=1, density=dnorm, mean=s2.2[2], sd=s2.2[4], col=rgb(0,0,0,0.1) )
  abline( v=b, lty=2, lwd=0.5 )
  legend( 'topleft', legend=c('true','with m.e.'),
          bty='n', lty=c(2,1), lwd=c(1,1.5) )
})

dev.off()


## figure 3c ####

png( filename=file.path(main_dir, 'measurement_error3.png'),
     height=13, width=13, units='cm', res=400  )

with(dlist, {
  curve( dnorm( x, mean=s2.3[1], sd=s2.3[3]), 
         lty=2, lwd=1, xlim=c(-0.5,1), ylab='', xlab='X', 
         main=bquote( n == 100 ~ ',' ~ beta[X] == .(b) ~ ',' ~ sigma[T] == .(s2[3]) ) )
  curve( dnorm( x, mean=s2.3[2], sd=s2.3[4]), lty=1, lwd=1.5, xlim=c(-0.5,1), add=T )
  colorArea( from=0, to=1, density=dnorm, mean=s2.3[2], sd=s2.3[4], col=rgb(0,0,0,0.1) )
  abline( v=b, lty=2, lwd=0.5 )
  legend( 'topleft', legend=c('true','with m.e.'),
          bty='n', lty=c(2,1), lwd=c(1,1.5) )
})

dev.off()










# figure extras ####

## figure XX ####
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



## figure XX ####
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



## figure XX ####

# distributional parameters
dpA = c(-0.9, 0.5) # distribution text A (mean, sd)
dpB = c(0, 1) # distribution text B (mean, sd)
dpC = c(0.9, 0.5) # distribution text B (mean, sd)
rAB = 0 # correlation between discriminal processes


# figure
png( filename=file.path(main_dir, 'diff_discriminal_dispersion.png'),
     height=10, width=15, units='cm', res=400  )

# initial figure
par( mar=c(2,0,0.5,0) )
plot( NULL, xlim=c(-3,3), ylim=c(0,0.9), 
      xaxt='n', yaxt='n', xlab='', ylab='', axes=F )
axis( side=1, labels=F, lwd.ticks=0 )
axis( side=1, at=c(dpA[1], dpB[1], dpC[1]), tick=F, 
      label=c( expression( T[A] ), expression( T[B] ), expression( T[C] ) ) )

# figure parameters
pplotA = c( dnorm( dpA[1], mean=dpA[1], sd=dpA[2] ),
            dnorm( dpA[1], mean=dpA[1], sd=dpA[2] ) + 0.05,
            dnorm( dpA[1] + dpA[2], mean=dpA[1], sd=dpA[2]),
            dnorm( dpA[1]+0.25, mean=dpA[1], sd=dpA[2] )-0.25 )

pplotB = c( dnorm( dpB[1], mean=dpB[1], sd=dpB[2] ),
            dnorm( dpB[1], mean=dpB[1], sd=dpB[2] ) + 0.05,
            dnorm( dpB[1] + dpB[2], mean=dpB[1], sd=dpB[2]),
            dnorm( dpB[1]+0.45, mean=dpB[1], sd=dpB[2] )-0.15 )

pplotC = c( dnorm( dpC[1], mean=dpC[1], sd=dpC[2] ),
            dnorm( dpC[1], mean=dpC[1], sd=dpC[2] ) + 0.05,
            dnorm( dpC[1] + dpC[2], mean=dpC[1], sd=dpC[2]),
            dnorm( dpC[1]+0.45, mean=dpC[1], sd=dpC[2] )-0.08 )


# discriminal process text A
curve( dnorm(x, mean=dpA[1], sd=dpA[2]), lwd=1.5, col=rgb(0,0,0,0.6), add=T ) 
text( x=dpA[1], y=pplotA[2], cex=0.8, 'Text A', col=rgb(0,0,0,0.6) ) 
lines( x=c(dpA[1], dpA[1]), y=c(pplotA[1], -0.1), lty=2, lwd=0.7, col=rgb(0,0,0,0.6) )
lines( x=c(dpA[1], dpA[1]+dpA[2]), y=rep( pplotA[3], 2), lty=2, lwd=0.7, col=rgb(0,0,0,0.6) )
text( x=dpA[1]+0.25, y=pplotA[4], cex=0.8, expression( sigma[A] ), col=rgb(0,0,0,0.6) ) 

# discriminal process text B
curve( dnorm(x, mean=dpB[1], sd=dpB[2]), lwd=1.5, add=T ) 
text( x=dpB[1], y=pplotB[2], cex=0.8, 'Text B' ) 
lines( x=c(dpB[1], dpB[1]), y=c(pplotB[1], -0.1), lty=2, lwd=0.7 )
lines( x=c(dpB[1], dpB[1]+dpB[2]), y=rep( pplotB[3], 2), lty=2, lwd=0.7 )
text( x=dpB[1]+0.45, y=pplotB[4], cex=0.8, expression( sigma[B] ) ) 

curve( dnorm(x, mean=dpC[1], sd=dpC[2]), lwd=1.5, col=rgb(0,0,0,0.6), add=T ) 
text( x=dpC[1], y=pplotA[2], cex=0.8, 'Text C', col=rgb(0,0,0,0.6) ) 
lines( x=c(dpC[1], dpC[1]), y=c(pplotC[1], -0.1), lty=2, lwd=0.7, col=rgb(0,0,0,0.6) )
lines( x=c(dpC[1], dpC[1]+dpC[2]), y=rep( pplotC[3], 2), lty=2, lwd=0.7, col=rgb(0,0,0,0.6) )
text( x=dpC[1]+0.25, y=pplotC[4], cex=0.8, expression( sigma[C] ), col=rgb(0,0,0,0.6) ) 


dev.off()