require(docstring)

rm(list=ls())


#' Simulating predictor variables
#'
#' @param n sample size, default=10
#' @param sup_c support for continuous variable, default=c(-3,3)
#' @param sup_d support for discrete variable, default=1:2
#' @param prop_d sample proportion for sup_d, default=c(0.5,0.5) 
#'
#' @return data.frame of generated variables
#' @export
#'
#' @examples
sim_var = function( n = 30,
                    sup_c = c(-3,3),
                    sup_d = 1:2,
                    prop_d = c(0.5,0.5) ){
  
  # # test
  # n = 30
  # sup_c = c(-3,3)
  # sup_d = 1:2
  # prop_d = c(0.4,0.6)
  
  
  # continuous variable
  delta = ( sup_c[2]-sup_c[1] ) / n # effective minimum difference
  start = sup_c[1] + delta/2
  end = sup_c[2] - delta/2 
  Xc = seq( start, end, length.out=n )
  
  idx = sample( 1:n, size=n, replace=F ) # break correlation
  Xc = Xc[idx]
  
  
  # dichotomous variable
  n_d = round( n*prop_d ) # samples based on proportion
  Xd = c()
  for( i in sup_d ){
    Xd = c( Xd, rep( sup_d[i], each=n_d[i] ) )
  }
  if( sum(n_d) < n ){
    extra = sample(sup_d, size=n-sum(n_d), replace=T)
    Xd = c(Xd, extra)
  }
  
  idx = sample( 1:n, size=n, replace=F ) # break correlation
  Xd = Xd[idx]
  
  
  # join data
  X = data.frame( Xc, Xd )
  
  
  # return object
  return(X)
  
}



#' Simulate judges' bias
#'
#' @param Jn sample size, default=10
#' @param Jb vector with betas, size=1+length(Jsup_d), default=c(0,0,0), no judges differences
#' @param Js vector with standard error, size=length(Jsup_d), default=c(1,1)
#' @param Jsup_c support for continuous variable, default=c(-3,3)
#' @param Jsup_d support for discrete variable, default=1:2
#' @param Jprop_d sample proportion for Jsup_d, default=c(0.5,0.5) 
#' @param Jseed seed for replication, default=NULL
#'
#' @return
#' @export
#'
#' @examples
sim_judges_bias = function( Jn = 20,
                            Jb = c(0,0,0),
                            Js = c(1,1),
                            Jsup_c = c(-3,3),
                            Jsup_d = 1:2,
                            Jprop_d = c(0.5,0.5),
                            Jseed = NULL ){
  
  # # test
  # Jn = 30
  # Jsup_c = c(-3,3)
  # Jsup_d = 1:2
  # Jprop_d = c(0.5,0.5)
  # Jb = c(0, 0, 0) # 1 + ncJ
  # Js = 0.02
  # Jseed = NULL

  # defining storage
  d = data.frame( Jid = 1:Jn )
  
  # predictors 
  JX = sim_var( n=Jn, sup_c=Jsup_c, sup_d=Jsup_d, prop_d=Jprop_d )
  names(JX) = paste0( 'J', names(JX) )
  d = cbind( d, JX ) # storage
  
  
  # bias: signal
  JX$JXd = factor( JX$JXd )
  Jm = model.matrix( ~ -1 + JXc + JXd, data=JX )
  if( length(Jb) != ncol(Jm) ){
    stop( paste0('you need to define ', 
                 abs( ncol(Jm)-length(Jb) ),
                 ' more/less value(s) for Jb') )
  }
  Jm = Jm %*% Jb
  d = cbind( d, Jm ) # storage
  
  
  # bias: noise
  if( length(Js) > length(Jsup_d) ){
    stop( paste0('you need to define ', 
                 abs( length(Js)-length(Jsup_d) ),
                 ' less value(s) for Js') )
  }
  if( !is.null(Jseed) ){
    set.seed(Jseed)
  }
  Je = rnorm( n=Jn, mean=0, sd=Js[JX$JXd] )
  Je = round( Je, 2 )
  d = cbind( d, Je ) # storage
  
  
  # bias: signal + noise
  Jbias = c( with( d, Jm + Je ) )
  d = cbind( d, Jbias ) # storage
  
  
  # return object
  return( list( Jd = d, 
                Jp = list( Jn = Jn,
                           Jb = Jb,
                           Js = Js,
                           Jsup_c = Jsup_c,
                           Jsup_d = Jsup_d,
                           Jprop_d = Jprop_d,
                           Jseed = Jseed ) ) )
  
}


#' Simulate units and subunits traits
#'
#' @param Un sample size, default=60
#' @param Ub vector with betas, size=1+length(Usup_d), default=c(0,0,0), no trait differences
#' @param Us vector with standard error, size=length(Usup_d), default=c(1,1)
#' @param Sn sample size, default=10
#' @param Sb vector with betas, size=1+length(Ssup_d), default=c(0,0,0), no trait differences
#' @param Ss vector with standard error, size=length(Ssup_d), default=c(0.8,0.8), more homogeneous stimuli
#' @param Usup_c support for continuous variable, default=c(-3,3)
#' @param Usup_d support for discrete variable, default=1:2
#' @param Uprop_d sample proportion for Usup_d, default=c(0.5,0.5) 
#' @param Useed seed for replication, default=NULL
#' @param Ssup_c support for continuous variable, default=c(-3,3)
#' @param Ssup_d support for discrete variable, default=1:2
#' @param Sprop_d sample proportion for Ssup_d, default=c(0.5,0.5) 
#' @param Sseed seed for replication, default=NULL
#'
#' @return
#' @export
#'
#' @examples
sim_units_trait = function( Un = 60,
                            Ub = c(0,0,0),
                            Us = c(1,1),
                            Sn = 10,
                            Sb = c(0,0,0),
                            Ss = c(0.8,0.8),
                            Usup_c = c(-3,3),
                            Usup_d = 1:2,
                            Uprop_d = c(0.5,0.5),
                            Useed = NULL,
                            Ssup_c = c(-3,3),
                            Ssup_d = 1:2,
                            Sprop_d = c(0.5,0.5),
                            Sseed = NULL ){
  
  # # test
  # Un = 60
  # Ub = c(0, 0, 0)
  # Us = c(0.02,0.02)
  # Sn = 10
  # Sb = c(0, 0, 0)
  # Ss = c(1,1)
  # Usup_c = c(-3,3)
  # Usup_d = 1:2
  # Uprop_d = c(0.5,0.5)
  # Useed = 12345
  # Ssup_c = c(-3,3)
  # Ssup_d = 1:2
  # Sprop_d = c(0.5,0.5)
  # Sseed = 12345
  
  # defining storage
  d = data.frame( Uid = rep( 1:Un, each=Sn ),
                  Sid = rep( 1:Sn, Un ) )
  
  
  # units
  
  # predictors 
  UX = sim_var( n=Un, sup_c=Usup_c, sup_d=Usup_d, prop_d=Uprop_d )
  names(UX) = paste0( 'U', names(UX) )
  UX = UX[d$Uid,] # storage
  rownames(UX) = NULL
  d = cbind( d, UX ) 
  
  
  # trait: signal
  UX$UXd = factor( UX$UXd )
  Um = model.matrix( ~ -1 + UXc + UXd, data=UX )
  if( length(Ub) != ncol(Um) ){
    stop( paste0('you need to define ', 
                 abs( ncol(Um)-length(Ub) ),
                 ' more/less value(s) for Ub') )
  }
  Um = Um %*% Ub
  d = cbind( d, Um ) # storage
  
  
  # trait: noise
  if( length(Us) > length(Usup_d) ){
    stop( paste0('you need to define ', 
                 abs( length(Us)-length(Usup_d) ),
                 ' less value(s) for Us') )
  }
  if( !is.null(Useed) ){
    set.seed(Useed)
  }
  Ue = rnorm( n=Un, mean=0, sd=Us[UX$UXd] )
  Ue = round( Ue, 2 )
  Ue = Ue[d$Uid] # storage
  d = cbind( d, Ue ) 
  
  
  # trait: signal + noise
  Utrait = c( with( d, Um + Ue ) )
  d = cbind( d, Utrait ) # storage
  
  
  
  
  # sub-units
  
  # predictors 
  SX = sim_var( n=Sn, sup_c=Ssup_c, sup_d=Ssup_d, prop_d=Sprop_d )
  names(SX) = paste0( 'S', names(SX) )
  SX = SX[d$Sid,] # storage
  rownames(SX) = NULL
  d = cbind( d, SX ) 
  
  
  # trait: signal
  SX$SXd = factor( SX$SXd )
  Sm = model.matrix( ~ -1 + SXc + SXd, data=SX )
  if( length(Sb) != ncol(Sm) ){
    stop( paste0('you need to define ', 
                 abs( ncol(Sm)-length(Sb) ),
                 ' more/less value(s) for Sb') )
  }
  Sm = Utrait + Sm %*% Sb
  d = cbind( d, Sm ) # storage
  
  
  # trait: noise
  if( length(Ss) > length(Ssup_d) ){
    stop( paste0('you need to define ', 
                 abs( length(Ss)-length(Ssup_d) ),
                 ' less value(s) for Ss') )
  }
  if( !is.null(Sseed) ){
    set.seed(Sseed)
  }
  Se = rnorm( n=Sn, mean=0, sd=Ss[SX$SXd] )
  Se = round( Se, 2 )
  Se = Se[d$Sid] # storage
  d = cbind( d, Se ) 
  
  
  # trait: signal + noise
  Strait = c( with( d, Sm + Se ) )
  d = cbind( d, Strait ) # storage
  
  
  # return object
  return( list( USd = d, 
                USp = list( Un = Un,
                            Ub = Ub,
                            Us = Us,
                            Sn = Sn,
                            Sb = Sb,
                            Ss = Ss,
                            Usup_c = Usup_c,
                            Usup_d = Usup_d,
                            Uprop_d = Uprop_d,
                            Useed = Useed,
                            Ssup_c = Ssup_c,
                            Ssup_d = Ssup_d,
                            Sprop_d = Sprop_d,
                            Sseed = Sseed ) ) )
  
}


#' Inverse logit
#'
#' @param v numeric vector
#'
#' @return numeric vector
#' @export
#'
#' @examples
inv_logit = function(v){
  
  p = 1/(1 + exp(-v))
  p = ifelse( v==Inf, 1, p )
  
  # return object
  return(p)
  
}




#' Comparison mechanism: population full comparison
#'
#' @param USd data generated with sim_units_trait()
#' @param Jd data generated with sim_judges_bias()
#'
#' @return data.frames for full comparison
#' @export
#'
#' @examples
cm_full = function( USd, Jd ){
  
  # # test
  # USd = sim_units_trait( Un=10, 
  #                        Us=c(1,1),
  #                        Ub=c(0,-0.2,0.2),
  #                        Sn=6,
  #                        Ss=c(0.8,0.8),
  #                        Sb=c(0,0,0) )
  # 
  # Jd = sim_judges_bias( Jn=10, 
  #                       Js=c(0.02,0.02) )
  # 
  # USd = USd$USd
  # Jd = Jd$Jd
  
  
  # collecting parameters
  n1 = nrow(USd) # number of sub-units to compare
  n2 = nrow(Jd) # number of judges that will compare
  
  
  # comparison data
  idx1 = t( combn( 1:n1, 2) )
  idx2 = rep( 1:n2, each=length(idx1))
  
  
  # full comparison data
  d = Jd[idx2, ]
  
  dm = USd[idx1[,1], ]
  names(dm) = paste0( names(dm), '1')
  d = cbind( d, dm )
  
  dm = USd[idx1[,2], ]
  names(dm) = paste0( names(dm), '2')
  d = cbind( d, dm )
  
  rownames(d) = NULL
  # str(d)

  # outcome calculation
  d$P = round( with( d, (Strait1-Strait2) + Jbias ), 2)
  d$D = rbinom( n=nrow(d), size=1, prob=inv_logit(d$P) )
  
  
  # return object
  return( d )
  
}


#' list+_data: convert data.frame to a list ready for BTLm.stan 
#'
#' @param d comparison data generated with any cm_*() function
#'
#' @return data.list
#' @export
#'
#' @examples
list_data = function( d ){
  
  # # test
  # d
  
  # storage
  dL = list()
  
  
  # dimensions
  dL$N = nrow( d )
  dL$U = max( unlist( d[,c('Uid1','Uid2')] ) )
  dL$S = max( unlist( d[,c('Sid1','Sid2')] ) )
  dL$J = max( d$Jid )
  dL$cUXd = max( unlist( d[,c('UXd1','UXd1')] ) )
  dL$cSXd = max( unlist( d[,c('SXd1','SXd1')] ) )
  dL$cJXd = max( d$JXd )
  # str(dL)
  
  
  # comparison data
  var_int = c('Uid1','Sid1','Uid2','Sid2','Jid','D')
  dL1 = d[,var_int]
  names(dL1)[ names(dL1) == 'Jid'] = 'Jid_'
  dL = c( dL, as.list( dL1 ) )
  # str(dL)
  
  
  # units data
  var_int = c('Uid1','UXc1','UXd1')
  dL1 = d[, var_int]
  names(dL1) = c('Uid','UXc','UXd')
  
  var_int = c('Uid2','UXc2','UXd2')
  dL2 = d[, var_int]
  names(dL2) = c('Uid','UXc','UXd')
  
  dL3 = rbind( dL1, dL2 )
  dL = c( dL, as.list( unique( dL3 ) ) )
  # str(dL)
  
  
  # sub-units data
  var_int = c('Sid1','SXc1','SXd1')
  dL1 = d[, var_int]
  names(dL1) = c('Sid','SXc','SXd')
  
  var_int = c('Sid2','SXc2','SXd2')
  dL2 = d[, var_int]
  names(dL2) = c('Sid','SXc','SXd')
  
  dL3 = rbind( dL1, dL2 )
  dL = c( dL, as.list( unique( dL3 ) ) )
  # str(dL)
  
  
  # judges data
  var_int = c('Jid','JXc','JXd')
  dL1 = unique( d[,var_int] )
  dL = c( dL, as.list( dL1 ) )
  # str(dL)
  
  
  # return object
  return( dL )
  
}