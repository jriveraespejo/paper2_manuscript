require(docstring)


#' Variable generator
#'
#' @param N sample size
#' @param support_c support for continuous variable, default=c(-3,3)
#' @param support_d support for discrete variable, default=1:2
#' @param dec decimal points, default=3
#'
#' @return data.frame of generated variables
#' @export
#'
#' @examples
var_gen = function( 
    N = NULL,
    support_c = c(-3,3),
    support_d = 1:2, 
    dec = 3
    ){
  
  # # test
  # N = 30
  # support_c = c(-3,3)
  # support_d = 1:2
  # dec = 3
  
  if( is.null(N) ){
    stop('Declare number of observations N')
  }
  
  # +++++++++++++++++++++
  # continuous
  # +++++++++++++++++++++
  start = support_c[1]
  delta = ( support_c[2]-support_c[1] ) / N # effective minimum difference
  end = support_c[2] - delta 
  Xc = seq( start, end, length.out=N)
  Xc = round(Xc, dec)
  
  # +++++++++++++++++++++
  # dichotomous
  # +++++++++++++++++++++
  Xd = rep(support_d, each=N/length(support_d) ) # ensuring equal numbers in categories
  if( length(Xd) < length(Xc) ){
    extra = sample(support_d, size=length(Xc)-length(Xd), replace=T)
    Xd = c(Xd, extra)
  }
  idx = sample(1:length(Xd), size=length(Xd), replace=F) # no correlation with Xc
  Xd = Xd[idx]
  Xd = as.factor(Xd)
  
  # join
  X = data.frame( Xc, Xd )
  
  # return object
  return(X)
  
}





#' Population simulation
#'
#' @param I sample of units, default=1000 (I=60 -> effective minimum difference = ( 3-(-3) )/1000 = 0.1 )
#' @param Ri sample of sub-units, default=200 (I=30 -> effective minimum difference = ( 3-(-3) )/30 = 0.2 ) 
#' @param K sample of judges, default=1000 (I=60 -> effective minimum difference = ( 3-(-3) )/60 =0.1 )
#' @param bX Covariate effects for units, default=c( bXc=0, bXd=c(0,0) ) according to RQ1 -> no effect
#' @param bY Covariate effects for sub-units, default=c( bYc=0, bYd=c(0,0) ) according to RQ1 -> no effect
#' @param bZ Covariate effects for judges, default=c( bZc=0, bZd=c(0,0) ) according to RQ1 -> no effect
#' @param ei_s Standard error for units, default=c(1,1) for identification
#' @param eir_s Standard error for sub-units in units, default=c(0.5,0.5) as a percentage of Ti_s 
#' @param log_ek_s Standard error for log-judges, default=c(0.01,0.01) for identification
#' @param X_sup_c support for continuous X variable, default=c(-3,3)
#' @param Y_sup_c support for continuous Y variable, default=c(-3,3)
#' @param Z_sup_c support for continuous Z variable, default=c(-3,3)
#' @param X_sup_d support for discrete X variable, default=1:2
#' @param Y_sup_d support for discrete Y variable, default=1:2
#' @param Z_sup_d support for discrete Z variable, default=1:2
#' @param seed seed for replication, default=NULL
#' @param dec decimal points, default=3
#'
#' @return list of data.frames for units, judges and parameters
#' @export
#'
#' @examples
pop_sim = function( 
    I = 1000, 
    R = 200, 
    K = 1000, 
    bX = c( bXc=0, bXd=c(0,0) ),
    bY = c( bYc=0, bYd=c(0,0) ),
    log_bZ = c( bZc=0, bZd=c(0,0) ),
    ei_s = c(1,1),
    eir_s = c(0.5,0.5),
    log_ek_s = c(0.1,0.1), 
    X_sup_c = c(-3,3),
    Y_sup_c = c(-3,3),
    Z_sup_c = c(-3,3),
    X_sup_d = 1:2,
    Y_sup_d = 1:2,
    Z_sup_d = 1:2,
    seed = NULL,
    dec = 3 
    ){
  
  # # test
  # I = 500
  # R = 50
  # K = 500
  # bX = c( bXc=0.3, bXd=c(-0.5,0.5) )
  # bY = c( bYc=0.3, bYd=c(-0.5,0.5) )
  # log_bZ = c( bZc=0.3, bZd=c(-0.5,0.5) )
  # ei_s = c(1,0.5)
  # eir_s = c(0.5,0.5)
  # log_ek_s = c(0.4,0.2)
  # X_sup_c = c(-3,3)
  # Y_sup_c = c(-3,3)
  # Z_sup_c = c(-3,3)
  # X_sup_d = 1:2
  # Y_sup_d = 1:2
  # Z_sup_d = 1:2
  # seed = NULL
  # dec = 3
  
  
  # seed for simulation
  if( !is.null(seed) ){
    set.seed(seed)
  }
  

  # +++++++++++++++++++++
  # Units and Sub-units
  # +++++++++++++++++++++
  # storage
  d1 = data.frame( iid = rep(1:I, each=R),
                   rid = rep(1:R, I) )
  
  
  # X covariates
  X = var_gen( N=I, support_c=X_sup_c, support_d=X_sup_d )
  names(X) = c('Xi_c','Xi_d')
  d1 = cbind( d1, X[d1$iid,] )
  
  # Ti trait: signal
  Xi = with(X, model.matrix( ~ -1 + Xi_c + Xi_d) )
  Ti_m = Xi %*% bX
  d1 = cbind( d1, Ti_m=Ti_m[d1$iid] )
  
  # Ti trait: noise
  ei = rnorm( n=I, mean=0, sd=ei_s[X$Xi_d] )
  d1 = cbind( d1, ei=round( ei[d1$iid], dec ) ) 
  
  # Ti trait: signal + noise
  d1$Ti = with(d1, Ti_m + ei )
  
  
  # Y covariates
  Y = var_gen( N=R, support_c=Y_sup_c, support_d=Y_sup_d )
  names(Y) = c('Yr_c','Yr_d')
  d1 = cbind( d1, Y[d1$rid,] )
  
  # Tr trait: signal
  Yr = with(Y, model.matrix( ~ -1 + Yr_c + Yr_d) )
  Tr_m = Yr %*% bY
  d1 = cbind( d1, Tr_m=Tr_m[d1$rid] ) 
  
  # Tir trait: noise
  eir = rnorm( n=nrow(d1), mean=0, sd=eir_s[d1$Yr_d] )
  d1$eir = round( eir, dec)
  
  # Tir trait: signal + noise
  d1$Tir = with(d1, Ti + Tr_m + eir)
  
  # converting factors
  d1$Xi_d = as.numeric( as.character(d1$Xi_d) )
  d1$Yr_d = as.numeric( as.character(d1$Yr_d) )
  rownames(d1) = NULL
  
  
  # +++++++++++++++++++++
  # Judges
  # +++++++++++++++++++++
  # storage
  d2 = data.frame( kid=1:K )
  
  # covariates
  Z = var_gen( N=K, support_c=Z_sup_c, support_d=Z_sup_d )
  names(Z) = c('Zk_c','Zk_d')
  d2 = cbind( d2, Z)
  
  # trait: signal
  Zk = with(Z, model.matrix( ~ -1 + Zk_c + Zk_d) )
  d2$log_Gk_m = c( Zk %*% log_bZ )
  
  # trait: noise
  log_ek = rnorm( n=K, mean=0, sd=log_ek_s[Z$Zk_d] )
  d2$log_ek = round( log_ek, dec )  
  
  # trait: signal + noise
  log_Gk = c( with( d2, log_Gk_m + log_ek ) )
  d2$log_Gk = round( log_Gk, dec )
  
  # trait: signal + noise (exponentiated)
  d2$Gk_m = round( exp(d2$log_Gk_m), dec )
  d2$ek = round( exp(d2$log_ek), dec )
  d2$Gk = round( exp(d2$log_Gk), dec )
  
  # converting factors
  d2$Zk_d = as.numeric( as.character(d2$Zk_d) )
  rownames(d2) = NULL
  
  
  # return object
  return( list( units=d1, judges=d2,
                param=list( I=I, R=R, K=K, 
                            bX=bX, bY=bY, log_bZ=log_bZ,
                            ei_s=ei_s, eir_s=eir_s, log_ek_s=log_ek_s,
                            X_sup_c=X_sup_c, Y_sup_c=Y_sup_c, Z_sup_c=Z_sup_c,
                            X_sup_d=X_sup_d, Y_sup_d=Y_sup_d, Z_sup_d=Z_sup_d,
                            seed=seed, dec=dec ) ) )
  
}



#' Data of interest: for units or judges
#'
#' @param pop_data population data (use `pop_sim()` function )
#' @param req_diff minimum required signal between individuals, default=0.2
#' @param idx units/judges of interest, default=NULL
#' @param type two options: units, judges or both, default='units'
#' @param nsize units to sample when is.null(idx_u)==T, default=1
#'
#' @return data.
#' @export
#'
#' @examples
data_int = function( 
    pop_data,
    req_diff = 0.2,
    idx = NULL,
    type = 'both',
    nsize = 1 
    ){
  
  # # test
  # pop_data
  # req_diff = 0.2
  # idx = NULL
  # nsize = 1
  # type = 'both'
  
  
  # units index
  d = unique( pop_data$units[, c('iid','Ti')] )
  if( is.null(idx) ){
    idx1 = sample( 1:pop_data$param$I, size=nsize, replace=F )
  }
  uref = matrix( rep( d$Ti[idx1], each=nrow(d)), ncol=length(idx1), byrow=F )
  udif = abs( d$Ti - req_diff - uref )
  idx1 = c( idx1, apply( udif, 2, which.min ) )
  units = d[idx1,]
  
  
  # judges index
  d = pop_data$judges[, c('kid','Gk') ]
  if( is.null(idx) ){
    idx2 = sample( 1:pop_data$param$K, size=nsize, replace=F )
  }
  uref = matrix( rep( d$Gk[idx2], each=nrow(d)), ncol=length(idx2), byrow=F )
  udif = abs( d$Gk - req_diff - uref )
  idx2 = c( idx2, apply( udif, 2, which.min ) )
  judges = d[idx2,]
  
  
  # return object
  if( type=='units' ){
    return( data.frame(units) )
  } else if( type=='judges' ){
    return( data.frame(judges) )
  } else if( type=='both' ){
    return( list(units=units, judges=judges) )
  }
  
}



#' Sample sizes
#'
#' @param nI units to sample, default=30
#' @param nR sub-units to sample, default=10
#' @param nK judges to sample, default=50
#' @param pI proportion of units per group, default=c(0.5,0.5) (balanced design with two groups)
#' @param pR proportion of sub-units per group, default=c(0.5,0.5) (balanced design with two groups)
#' @param pK proportion of judges per group, default=c(0.5,0.5) (balanced design with two groups)
#'
#' @return list of sample sizes
#' @export
#'
#' @examples
sample_sizes = function( 
    nI = 30,
    nR = 10,
    nK = 50,
    pI = c(0.5, 0.5),
    pR = c(0.5, 0.5),
    pK = c(0.5, 0.5)
    ){
  
  # # test
  # nI = 30
  # nR = 10
  # nK = 100
  # pI = c(0.5, 0.5)
  # pR = c(0.5, 0.5)
  # pK = c(0.5, 0.5)
  
  
  # checks
  if( any( sum(pI)!=1, sum(pR)!=1, sum(pK)!=1 ) ){
    stop( 'proportions do not add to 1' )
  }
  
  
  # sample sizes
  nIp = c()
  for( i in 1:(length(pI)-1) ){
    nIp = c(nIp, round( nI*pI[i], 0 ) )
  }
  nIp = c(nIp, nI-sum(nIp)) 
  
  nRp = c()
  for( i in 1:(length(pR)-1) ){
    nRp = c(nRp, round( nR*pR[i], 0 ) )
  }
  nRp = c(nRp, nR-sum(nRp))
  
  nKp = c()
  for( i in 1:(length(pK)-1) ){
    nKp = c(nKp, round( nK*pK[i], 0 ) )
  }
  nKp = c(nKp, nK-sum(nKp))
  
  
  # return object
  return( list(nIp=nIp, nRp=nRp, nKp=nKp) )
  
}





#' Sample mechanism: (semi)random
#'
#' @param pop_data population data (use `pop_sim()` function )
#' @param sizes list of sample sizes for units, sub-units and judges (use `sample_sizes()` function )
#' @param idx index of particular units/judges to be sampled, default=NULL
#' @param type threw options: units, judges or both, default='units'
#' @param seed seed for replication, default=NULL
#'
#' @return list of data.frames for units and judges
#' @export
#'
#' @examples
sm_srandom = function( 
    pop_data,
    sizes,
    idx = NULL,
    type = 'both',
    seed = NULL 
    ){
  
  # # test
  # pop_data
  # idx = d_int
  # type = 'units'
  # sizes
  # seed=NULL
  
  
  # seed for simulation
  if( !is.null(seed) ){
    set.seed(seed)
  }
  
  
  # random sample of units
  # (fully random sample)
  units = unique( pop_data$units[,c('iid','Xi_c','Xi_d')] )
  idx_units = c()
  for( i in 1:length(sizes$nIp) ){
    idx_g = units$Xi_d == i
    idx_m = sample( units$iid[idx_g], size=sizes$nIp[i], replace=F )
    idx_units = c(idx_units, idx_m)  
  }
  
  
  # selecting particular units 
  # (less random the bigger the length(idx)/2 )
  if( type %in% c('units','both') & !is.null(idx) ){
    if( is.data.frame(idx) ){
      idx1 = idx$iid
    } else{
      idx1 = idx$units$iid
    }
    min_l = min( length(idx1), length(idx_units) )
    idx1 = idx1[1:min_l]
    idx_units[1:min_l] = idx1  
  }
  idx_units = idx_units[order(idx_units)]
  
  
  # random sample of sub-units within units
  units = pop_data$units
  units = units[units$iid %in% idx_units, ]
  d1 = c()
  for(i in idx_units){
    idx_u = units$iid == i
    for( j in 1:length(sizes$nRp) ){
      idx_g = units$Yr_d == j
      idx_m = sample( units$rid[idx_u & idx_g], size=sizes$nRp[j], replace=F )
      idx_m = units$rid %in% idx_m
      dmom = units[idx_u & idx_m, ]
      d1 = rbind(d1, dmom)
    }
  }
  rownames(d1) = NULL
  
  
  # selected units
  d1$unit_sel = 0
  d1$unit_sel[ d1$iid %in% idx1 ] = 1
  # sum(d1$unit_sel)
  
  # extra steps for STAN
  # new unit indices
  units = unique( d1$iid )
  units = units[order(units)]
  units = data.frame( iid=units, iidm=1:length(units) )
  
  d1$iidm = NA
  for( i in 1:nrow(units)){
    idx = d1$iid == units$iid[i]
    d1$iidm[idx] = units$iidm[i]
  }
  
  
  # new sub-unit indices
  units = unique( d1[,c('iidm','rid')] )
  units = units[ with(units, order(iidm,rid)), ]
  units = data.frame( units, ridm = rep(1:sum(sizes$nRp), sum(sizes$nIp) ) )
  
  d1$ridm = NA
  for( i in 1:nrow(units)){
    idx1 = d1$iidm == units$iidm[i]
    idx2 = d1$rid == units$rid[i]
    d1$ridm[idx1 & idx2] = units$ridm[i]
  }
  
  # reorder
  var_int = c('iid','rid','iidm','ridm','unit_sel','Xi_c','Xi_d',
              'Ti_m','ei','Ti','Yr_c','Yr_d','Tr_m','eir','Tir' )
  idx = with(d1, order(iidm,ridm))
  d1 = d1[idx, var_int]
  
  
  
  # random sample of judges
  # (fully random sample)
  judges = pop_data$judges
  idx_judges = c()
  for( i in 1:length(sizes$nKp) ){
    idx_g = judges$Zk_d == i
    idx_m = sample( judges$kid[idx_g], size=sizes$nKp[i], replace=F )
    idx_judges = c(idx_judges, idx_m)  
  }
  
  
  # selecting particular judges 
  # (less random the bigger the length(idx)/2 )
  if( type %in% c('judges','both') & !is.null(idx) ){
    if( is.data.frame(idx) ){
      idx2 = idx$kid
    } else{
      idx2 = idx$judges$kid
    }
    min_l = min( length(idx2), length(idx_judges) )
    idx2 = idx2[1:min_l]
    idx_judges[1:min_l] = idx2  
  }
  idx_judges = idx_judges[order(idx_judges)]
  
  
  # final data of judges
  d2 = judges[ judges$kid %in% idx_judges, ]
  rownames(d2) = NULL
  
  # selected judges
  d1$judge_sel = 0
  d1$judge_sel[ d2$kid %in% idx2 ] = 1
  # sum(d2$judge_sel)
  
  # extra steps for STAN
  # new judge indices
  judges = unique( d2[,'kid'] )
  judges = judges[order(judges)]
  judges = data.frame( kid=judges, kidm=1:length(judges) )
  
  d2$kidm = NA
  for( i in 1:nrow(judges)){
    idx = d2$kid == judges$kid[i]
    d2$kidm[idx] = judges$kidm[i]
  }
  
  # reorder
  var_int = c('kid','kidm','Zk_c','Zk_d','log_Gk_m','log_ek','log_Gk','Gk_m','ek','Gk')
  idx = with(d2, order(kidm))
  d2 = d2[idx, var_int]
  
  
  # return object
  return( list( units=d1, judges=d2, 
                param=list( sizes=sizes, idx=idx, type=type ) ) )
  
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



#' Comparison mechanism: random (Comproved)
#'
#' @param data_sample sample data (use `sm_srandom()` function )
#' @param ncomp number of comparisosn per sub-unit, default=20 ( https://comproved.com/en/faq/ )
#' @param seed seed for replication, default=NULL
#'
#' @return list of data.frames for true and observed data
#' @export
#'
#' @examples
cm_random = function( 
    data_sample,
    ncomp = 20,
    seed = NULL 
    ){

  # # test
  # data_sample
  # ncomp = 20
  # seed = NULL

  
  # collecting parameters
  t = nrow(data_sample$units) # number of sub-units to compare
  b = nrow(data_sample$judges) # number of judges that will compare
  
  # seed for simulation
  if( !is.null(seed) ){
    set.seed(seed)
  }
  
  # algorithm
  full_design = data.frame( t( combn( 1:t, 2 ) ) )
  names(full_design) = paste0('row_u', 1:2)
  
  # count
  count = data.frame( row_u=1:t, r=0 )
  design = c()
  
  for( i in count$row_u ){
    
    # combinations of interest
    idx1 = with( full_design, row_u1==i | row_u2==i )
    # full_design[idx1, ]
    
    # design
    nsize = ncomp - count$r[i]
    if( nsize > 0 ){
      
      # design sample
      if( length(which(idx1)) < nsize ){
        idx2 = which(idx1)
      } else{
        idx2 = sample( which(idx1), size=nsize, replace=F )
      }
      design = rbind( design, full_design[idx2,] )
      
      # update count
      count_up = data.frame( table( unlist(design) ) )
      count_up$Var1 = as.numeric( as.character( count_up$Var1) )
      count$r[count_up$Var1] = count_up$Freq
      
      # update full design
      idx3 = count$row_u[ count$r >= ncomp ]
      idx1 = with( full_design, row_u1 %in% idx3 | row_u2 %in% idx3 )
      full_design = full_design[!idx1,]
      
    }
  
  }
  # count
  # sum( table( unlist(design) ) < ncomp )
  
  
  # judge assignment
  idx1 = sample( 1:nrow(design), size=nrow(design), replace=F )
  design$row_j = NA
  design$row_j[idx1] = 1:b
  # table(design$row_j)
  
  
  
  # getting data
  d1 = data_sample$units[ design$row_u1, ]
  names(d1) = paste0( names(d1), '_1' )
  d2 = data_sample$units[ design$row_u2, ]
  names(d2) = paste0( names(d2), '_2' )
  d3 = data_sample$judges[ design$row_j, ]
  d = cbind( d1, d2, d3 )
  
  
  # making comparisons
  d$Dkijr = c( with( d, Gk*(Tir_1-Tir_2) ) )
  d$Okijr = rbinom( n=nrow(d), size=1, prob=inv_logit(d$Dkijr) )
  
  
  # re-sort data
  idx = with(d, order( iidm_1,ridm_1,iidm_2,ridm_2,kid ) )
  d = d[idx, ]
  rownames(d) = NULL
  # str(d)
  
  
  
  # record design characteristics
  design_char = vector('list',2)
  names(design_char) = c('replication','blocks')
  
  var_int = c('iid','rid','iidm','ridm','Xi_c','Xi_d','Yr_c','Yr_d')
  design_char$replication = cbind( data_sample$units[count$row_u, var_int], r=count$r )
  rownames(design_char$replication) = NULL
  
  var_int = c('kid','kidm','Zk_c','Zk_d')
  count = data.frame( table( design$row_j ) )
  count$Var1 = as.numeric( as.character( count$Var1) )
  design_char$blocks = cbind( data_sample$judges[count$Var1, var_int], k=count$Freq )
  
  
  
  # return object
  var_int = c('iid_1','iidm_1','rid_1','ridm_1','Xi_c_1','Xi_d_1','Yr_c_1','Yr_d_1',
              'iid_2','iidm_2','rid_2','ridm_2','Xi_c_2','Xi_d_2','Yr_c_2','Yr_d_2',
              'kid','kidm','Zk_c','Zk_d','Okijr')
  return( list( d_true=d, d_obs=d[,var_int], design=design_char ) )

}




#' List data1: convert data.frame to a list ready for BTLm.stan 
#'
#' @param RQ research question to be answered, default=1
#' @param data_obs observed data (use `cm_random()` function )
#'
#' @return data list
#' @export
#'
#' @examples
list_data = function( data_obs ){
  
  # # test
  # data_obs = data_comp$d_obs
  
  
  # storage
  d = list()
  
  
  # dimensions
  d$N = nrow( data_obs )
  d$I = max( unlist( data_obs[,c('iidm_1','iidm_2')] ) )
  d$R = max( unlist( data_obs[,c('ridm_1','ridm_2')] ) )
  d$K = max( data_obs$kidm )
  d$cXd = max( unlist( data_obs[,c('Xi_d_1','Xi_d_2')] ) )
  d$cYd = max( unlist( data_obs[,c('Yr_d_1','Yr_d_2')] ) )
  d$cZd = max( data_obs$Zk_d )
  
  
  # comparison data
  var_int = c('iidm_1','ridm_1','iidm_2','ridm_2','kidm','Okijr')
  d1 = data_obs[,var_int]
  names(d1)[names(d1) == 'kidm'] = 'kidm_'
  d = c(d, as.list( d1 ) )
  
  
  # units data
  var_int = c('iidm_1','Xi_c_1','Xi_d_1')
  d1 = data_obs[, var_int]
  names(d1) = c('iidm','Xi_c','Xi_d')
  
  var_int = c('iidm_2','Xi_c_2','Xi_d_2')
  d2 = data_obs[, var_int]
  names(d2) = c('iidm','Xi_c','Xi_d')
  
  d3 = rbind(d1,d2)
  d = c(d, as.list( unique(d3) ) )
  
  
  # sub-units data
  var_int = c('iidm_1','ridm_1','Yr_c_1','Yr_d_1')
  d1 = data_obs[, var_int]
  names(d1) = c('iidm_','ridm','Yr_c','Yr_d')
  
  var_int = c('iidm_2','ridm_2','Yr_c_2','Yr_d_2')
  d2 = data_obs[, var_int]
  names(d2) = c('iidm_','ridm','Yr_c','Yr_d')
  
  d3 = rbind(d1,d2)
  d = c(d, as.list( unique(d3) ) )
  
  
  # judges data
  var_int = c('kidm','Zk_c','Zk_d')
  d1 = unique( data_obs[,var_int] )
  d = c(d, as.list( d1 ) )
  # str(d)
  
  # return object
  return( d )
  
}




#' trace_plot
#' It creates a trace plot for a stanfit object
#'
#' @param draws read_cmdstan_csv()$post_warmup_draws
#' @param pars character with the name of a parameter
#'
#' @return trace plot
#' @export
#'
#' @examples
trace_plot = function(
    draws, 
    pars
    ){
  
  # # test
  # draws = BTLm$draws
  # pars = 'bXd[1]'
  
  # packages 
  require(RColorBrewer)
  require(rethinking)
  
  # posterior
  idx = attr(draws,'dimnames')$variable == pars
  if( length(dim(draws))==2 ){
    post = draws[, idx]
  } else{
    post = draws[, ,idx]
  }
  # str(post)
  
  # parameters
  n_chains = dim(post)[2]
  chain.cols = rep_len(rethink_palette, n_chains)
  wstart = 1
  wend = dim(post)[1]
  if( length(dim(draws))==2 ){
    ylim = range(post[wstart:wend, ])
  } else{
    ylim = range(post[wstart:wend, , ])
  }
  ytick = (ylim[2] - ylim[1])/6
  yaxis = round( seq(ylim[1], ylim[2], by=ytick), 2)
  
  # plot
  plot(NULL, type="l", xlim=c(wstart, wend), ylim=ylim,
       xlab="", ylab="", axes=F)
  box(bty="l")
  axis(side=1, at=seq(0, wend, by=100))
  axis(side=2, at=yaxis, las=1 )
  mtext(pars, 3, adj = 0, cex=1.1)
  if( length(dim(draws))==2 ){
    for(c in 1:n_chains){
      lines(1:wend, post[, c], col=chain.cols[c], lwd = 0.5)
    }
  } else{
    for(c in 1:n_chains){
      lines(1:wend, post[, c, ], col=chain.cols[c], lwd = 0.5)
    }
  }
  
}




#' trank_plot
#' It creates a trank plot for a stanfit object
#'
#' @param draws read_cmdstan_csv()$post_warmup_draws
#' @param pars character with the name of a parameter
#' @param wide controls the number of iterations (in the chain) considered. (default 50)
#'
#' @return trank plot
#' @export
#'
#' @examples
trank_plot = function(
    draws, 
    pars, 
    wide=50
    ){
  
  # # test
  # draws = BTLm$draws
  # pars = 'bXc'
  # wide=50
  
  # for colors
  require(RColorBrewer)
  require(rethinking)
  
  # posterior
  idx = attr(draws,'dimnames')$variable == pars
  if( length(dim(draws))==2 ){
    post = draws[, idx]
  } else{
    post = draws[, ,idx]
  }
  # str(post)
  
  # parameters
  n_chains = dim(post)[2]
  chain.cols = rep_len(rethink_palette, n_chains)
  wstart = 1
  wend = dim(post)[1]
  
  # rank calculation
  ranks = list()
  xrange = rep(1:wide, each=2)
  yrange = vector('list', n_chains)
  if( length(dim(draws))==2 ){
    for(c in 1:n_chains){
      ranks[[c]] = rank( post[1:(wide+1), c] )
      y_ran = c()
      for(i in 2:(wide+1)){
        y_ran = c(y_ran, c( ranks[[c]][i-1], ranks[[c]][i] ) )
      }
      yrange[[c]] = y_ran
    }
  } else{
    for(c in 1:n_chains){
      ranks[[c]] = rank( post[1:(wide+1), c, ] )
      y_ran = c()
      for(i in 2:(wide+1)){
        y_ran = c(y_ran, c( ranks[[c]][i-1], ranks[[c]][i] ) )
      }
      yrange[[c]] = y_ran
    }
  }
  
  
  # plot
  plot(NULL, type='l', xlim=c(0, wide+1), ylim=c(0, wide+1),
       xlab="", ylab="", xaxt ="n", yaxt ="n", axes=F)
  box(bty="l")
  for(c in 1:n_chains){
    lines(xrange, yrange[[c]], col=chain.cols[c], lwd=1.5)  
  }
  
}



#' acf_plot
#' It creates a acf plot for a stanfit object
#'
#' @param draws read_cmdstan_csv()$post_warmup_draws
#' @param pars character with the name of a parameter
#'
#' @return acf plot
#' @export
#'
#' @examples
acf_plot = function(
    draws, 
    pars
    ){
  
  # # test
  # draws = BTLm$post_warmup_draws
  # pars = 'bXc'
  
  # posterior
  idx = attr(draws,'dimnames')$variable == pars
  if( length(dim(draws))==2 ){
    post = draws[, idx]
  } else{
    post = draws[, ,idx]
  }
  # str(post)
  
  # plot
  n_chains = sample(1:dim(post)[2], size=1, replace=F)
  if( length(dim(draws))==2 ){
    acf( post[, n_chains], main='', xlab='', ylab='', mar = c(0, 0, 0, 0) )
  } else{
    acf( post[, n_chains, ], main='', xlab='', ylab='', mar = c(0, 0, 0, 0) )
  }
  
}



#' tri_plot
#' it plots trace, trank, and ACF plots for a maximum of 5 parameters
#'
#' @param draws read_cmdstan_csv()$post_warmup_draws
#' @param pars character VECTOR with the names of parameters of interest.
#' only plots a maximum of five (5) parameters.
#'
#' @return trace, trank and acf plot
#' @export
#'
#' @examples
tri_plot = function(
    draws, 
    pars
    ){
  
  # # test
  # draws = BTLm$post_warmup_draws
  # pars = c('bXc','bXd[1]','bXd[2]')
  
  # figure parameters
  opar = par()
  
  # ensure there is only 5 paramaters
  if(length(pars)>5){
    pars = pars[1:5]
  }
  
  # plot
  par(mfrow=c(length(pars), 3), mar=c(3,3.5,1.5,1)+0.1)
  
  for(i in 1:length(pars)){
    trace_plot(draws=draws, pars=pars[i]) 
    trank_plot(draws=draws, pars=pars[i])
    acf_plot(draws=draws, pars=pars[i])
  }
  
  par(mfrow=c(1,1), mar=opar$mar)
  
}



#' RMSE of parameters
#'
#' @param par_draws draws of parameters
#' @param true_par true parameter values
#'
#' @return vector of rmse
#' @export
#'
#' @examples
rmse = function(
    par_draws, 
    true_par
    ){
  
  # # test
  # par_draws = par_contr[,1:3]
  # true_par
  
  # check
  if( length(true_par) != ncol(par_draws) ){
    stop( 'you need to provide the same number of true parameters and samples')
  }
  
  # calculations
  par_true = matrix( rep(true_par, each=nrow(par_draws)), 
                     nrow=nrow(par_draws), ncol=ncol(par_draws), byrow=F)
  rmse_val = sqrt( sapply( (par_draws-par_true)^2, mean ) )
  
  
  # return object
  return(rmse_val)
  
}



#' Generate region of practical equivalence (ROPE) for parameters
#'
#' @param par_true vector of true parameter values
#' @param cuts cuts of interest, default=c(0,0.1,0.2,0.5,0.8,1.2,2), see Cohen (1988) and Sawilowsky (2009) 
#' @param hl half-length of ROPE, default=c(0.05,0.1,0.1,0.15,0.2,0.3), see Cohen (1988) and Sawilowsky (2009) 
#'
#' @return data.frame with ROPE values for parameters 
#' @export
#'
#' @examples
set_rope = function( 
    par_true, 
    cuts = c(0,0.2,0.5,0.8,1.2,2), 
    hl = c(0.1,0.1,0.15,0.2,0.25,0.5)
    ){ 
  
  # # test
  # par_true = c(0,0.2,0.5,0.8,1.2,2)
  # cuts = c(0,0.2,0.5,0.8,1.2,2)
  # hlength = c(0.05,0.1,0.1,0.15,0.2,0.3)
  
  # storage
  par_rope = data.frame( par_true = par_true,
                         rope_lower = par_true, 
                         rope_upper = par_true,
                         rope_prec = 0)
  
  # ROPE
  rope_val = abs(par_true)
  
  # cuts==0
  idx = rope_val==cuts[1]
  par_rope$rope_lower[idx] = par_true[idx] - hl[1]
  par_rope$rope_upper[idx] = par_true[idx] + hl[1]
  
  
  # other cuts
  for( j in 2:length(cuts)){
    idx = rope_val > cuts[j-1] & rope_val <= cuts[j]
    par_rope$rope_lower[idx] = par_true[idx] - hl[j]
    par_rope$rope_upper[idx] = par_true[idx] + hl[j]
  }
  
  # final cut
  idx = rope_val > cuts[length(cuts)]
  par_rope$rope_lower[idx] = par_true[idx] - 0.8
  par_rope$rope_upper[idx] = par_true[idx] + 0.8
  
  # precision
  par_rope$rope_prec = with( par_rope, (rope_upper-rope_lower)/4 )
  
  # return object
  return(par_rope)
  
}




#' ROPE goals
#'
#' @param par_true true parameter
#' @param par_sum summary of pramaters (use summarise_draws() function)
#' @param type type of intervals to use, default='hpdi'
#'
#' @return data.frame of results
#' @export
#'
#' @examples
rope_goals = function( 
    par_true, 
    par_sum,
    type='hpdi'
    ){
  
  # # test
  # par_true = true_par
  # par_sum
  # type='hpdi'
  
  # introduce ROPE
  rope = set_rope( par_true=par_true )
  par_sum = cbind( par_sum, rope )
  par_sum$true_gt0 = as.integer(par_sum$par_true>0)
  par_sum$true_sign = sign(par_sum$par_true)
  par_sum$est_sign = sign(par_sum$est)
  par_sum$equal_sign = as.integer(with(par_sum, true_sign==est_sign))
  
  # searching for variable
  idx = stringr::str_detect( names(par_sum), type )
  var_comp = names(par_sum)[idx]
  
  # ROPE goals
  # reject null
  Ho = set_rope( par_true = 0 )
  par_sum$reject_null = as.integer( 
    par_sum[var_comp[2]]<Ho$rope_lower | par_sum[var_comp[1]]>Ho$rope_upper )
  
  # accept alternative
  par_sum$accept_alt = as.integer( 
    with(par_sum,  true_gt0*(par_sum[var_comp[1]]>rope_lower) + (1-true_gt0)*(par_sum[var_comp[2]]<rope_upper) ) )
  
  # precision
  par_sum$precision = as.integer( 
    with(par_sum, (par_sum[var_comp[2]]-par_sum[var_comp[1]]) <= rope_prec ) )
  
  rownames(par_sum) = NULL
  
  # return object
  return(par_sum)
  
}



#' Report results of power
#'
#' @param goals_full results for goals (not summary) 
#'
#' @return data.frame with summary of power simulations
#' @export
#'
#' @examples
power_report = function( goals_full ){
  
  # # test
  # goals_full = goals_mom
  
  # package requirement
  require(dplyr)
  
  # calculation
  power_res = goals_full %>%
    group_by( variable ) %>%
    summarise( nsim = n(),
               mean_est = mean(est),
               sd_est = sd(est),
               mean_hpdi2.5 = mean(hpdi2.5),
               mean_hpdi97.5 = mean(hpdi97.5),
               sd_hpdi2.5 = sd(hpdi2.5),
               sd_hpdi97.5 = sd(hpdi97.5),
               mean_rmse = mean(rmse),
               sd_rmse = sd(rmse),
               mean_true = mean(par_true),
               sd_true = sd(par_true),
               equal_sign = mean(equal_sign),
               reject_null = mean(reject_null),
               accept_alt = mean(accept_alt),
               precision = mean(precision) 
               ) %>%
    data.frame()
  
  power_res[,2:ncol(power_res)] = round(power_res[,2:ncol(power_res)], 3)
  
  # return object
  return(power_res)
  
}



#' Simulation for RQ1, using HMC sampling
#'
#' @param nsim number of simulation, default=1000
#' @param p probability, default=0.95
#' @param params grid of parameter to simulate
#' @param seed_pop seed for population replication, default=NULL 
#' @param seed_sample list of seeds for sample replication, default=NULL
#' @param save_dir directory for saving results, default=getwd()
#' @param mod_dir directory for the compiled model, default=getwd()
#'
#' @return data.frame with power test
#' @export
#'
#' @examples
sim_RQ1_hmc = function(
    nsim = 1000,
    p = 0.95,
    params,
    seed_pop = NULL,
    seed_sample = NULL,
    save_dir = getwd(),
    mod_dir = getwd() ){
  
  # # test
  # nsim = 5
  # p = 0.95
  # params
  # seed_pop = NULL
  # seed_sample = NULL
  # save_dir = file.path(dir, '_results')
  # mod_dir = file.path(dir, 'code')
  
  
  # defined parameters
  # population
  I = 1000
  R = 200 
  K = 1000 
  bX = c( bXc=0, bXd=c(0,0) )
  bY = c( bYc=0, bYd=c(0,0) )
  log_bZ = c( bZc=0, bZd=log(c(0.75,0.95)) )
  ei_s = c(1,1)
  # eir_s = c(0.5,0.5)
  log_ek_s = c(0.02,0.02) 
  X_sup_c = c(-3,3)
  Y_sup_c = c(-3,3)
  Z_sup_c = c(-3,3)
  X_sup_d = 1:2
  Y_sup_d = 1:2
  Z_sup_d = 1:2
  # seed = NULL
  dec = 3 
  
  # sample mechanism
  nI = 30
  # nR = 10
  # nK = 50
  pI = rep(1/max(X_sup_d), max(X_sup_d))
  pR = rep(1/max(Y_sup_d), max(Y_sup_d))
  pK = rep(1/max(Z_sup_d), max(Z_sup_d))
  
  idx = NULL
  type = 'units'
  # seed = NULL 
  
  # comparison mechanism
  # ncomp = 20
  # seed = NULL
  
  
  
  # storage of results
  power_storage = c()
  
  
  # seeds for replication
  check1 = is.null(seed_pop)
  check2 = nrow(params) != length(seed_pop)
  if( check1 ){
    seed_pop = sample( 1:10^6, size=nrow(params), replace=F )
  } else if( check2 ){
    warning( paste0('you need to provide ', nrow(params), ' seeds ',
                    'for a fully replicable population simulation',
                    '\n','you provided ', length(seed_pop),' seed(s), ', 
                    nrow(params)-length(seed_pop), ' seed(s) have been simulated.') )
    seed_mom = sample( 1:10^6, size=nrow(params)-length(seed_pop), replace=F )
    seed_pop = c(seed_pop, seed_mom)
  }
  
  check1 = is.null(seed_sample)
  check2 = nrow(params) != length(seed_sample)
  check3 = any( sapply(seed_sample, length) != rep(nsim, nrow(params)) )
  if( check1 ){
    seed_sample = vector('list',nrow(params))
    for( i in 1:nrow(params) ){
      seed_sample[[i]] = sample( 1:10^6, size=nsim, replace=F )
    }
  } else if( check2 | check3 ){
    warning( paste0('you need to provide a list with ', nrow(params), ' objects, ',
                    'each with a length of ', nsim, ' seeds ',
                    '\n','for a fully replicable sample simulation',
                    '\n','you provided a list of ', length(seed_sample),' objects, ', 
                    'each with different or equal lengths', 
                    '\n','new objects and seed(s) have been simulated.') )
    for( i in 1:nrow(params) ){
      
      if( i <= length(seed_sample) ){
        seed = sample( 1:10^6, size=nsim-length(seed_sample[[i]]), replace=F )
        seed_sample[[i]] = c(seed_sample[[i]], seed)  
      } else{
        seed = sample( 1:10^6, size=nsim, replace=F )
        seed_sample = c(seed_sample, seed)  
      }
      
    }
    
  }
  
  
  # load model
  set_cmdstan_path('/home/josema/cmdstan')
  model_nam = 'BTLm'
  mod = cmdstan_model( file.path( mod_dir, paste0(model_nam,'.stan') ) )
  

  # simulation
  # pr=2
  for( pr in 1:nrow(params) ){ # 
    
    # population simulation
    pop_data = pop_sim( I = I, 
                        R = R, 
                        K = K, 
                        bX = bX,
                        bY = bY,
                        log_bZ = log_bZ,
                        ei_s = ei_s,
                        eir_s = unlist( params[pr, c('eir_s1','eir_s2')] ),
                        log_ek_s = log_ek_s,
                        X_sup_c = X_sup_c,
                        Y_sup_c = Y_sup_c,
                        Z_sup_c = Z_sup_c,
                        X_sup_d = X_sup_d,
                        Y_sup_d = Y_sup_d,
                        Z_sup_d = Z_sup_d,
                        seed = seed_pop[pr],
                        dec = dec ) 
    # hist(pop_data$judges$Gk)

    # storage goal
    goals_mom = c()
    
    # sample and comparison simulation
    # s=1
    for(s in 1:nsim ){  
      
      # selecting units/judges
      # (ensuring at least 2 with minimum req_diff are included)
      d_int = data_int( pop_data = pop_data, 
                        req_diff = params$req_diff[pr], 
                        type = type )
      
      # sample mechanism
      sizes = sample_sizes( nI = nI,
                            nR = params$nR[pr], 
                            nK = params$nK[pr],
                            pI = pI, 
                            pR = pR, 
                            pK = pK )
      
      data_sample = sm_srandom( pop_data = pop_data, 
                                sizes = sizes,
                                idx = d_int, 
                                type = type, 
                                seed = seed_sample[[pr]][s] )
      # hist(data_sample$judges$Gk)
      
      
      # selected data
      if( type %in% c('units','both') ){
        idx = data_sample$units$unit_sel==1
        units = data_sample$units[idx,]
        units = unique( units[,c('iidm','Ti')])
        rownames(units) = paste0( colnames(units)[2],'[',units$iidm,']')
      } 
      if( type %in% c('judges','both') ){
        idx = data_sample$judges$judge_sel==1
        judges = data_sample$judges[idx,]
        judges = unique( judges[,c('kidm','Gk')])
        rownames(judges) = paste0( colnames(judges)[2],'[',judges$kidm,']')
      }
      
      
      # comparison mechanism
      data_comp = cm_random( data_sample = data_sample, 
                             ncomp = params$ncomp[pr],
                             seed = seed_sample[[pr]][s] )  
      # str(data_comp)
      
      
      # data list
      data_list = list_data( data_obs=data_comp$d_obs )
      
      equal_ei_s = as.numeric( ei_s[1]==ei_s[2] )
      equal_eir_s = as.numeric( params$eir_s1[pr] == params$eir_s2[pr] )
      equal_log_ek_s = as.numeric( log_ek_s[1]==log_ek_s[2] )
      data_list = c( equal_ei_s=equal_ei_s,
                     equal_eir_s=equal_eir_s,
                     equal_log_ek_s=equal_log_ek_s,
                     data_list )
      # str(data_list)
      
      
      # ensuring no prior files 
      files = file.path( save_dir, paste0( model_nam, '-', 1:4, '.csv') )
      if( any(file.exists(files)) ) {
        sapply(files, file.remove)
      }
      
      
      model_noexist = T
      while( model_noexist ){
        
        tryCatch(
          expr = {
            # run model
            mod$sample( data = data_list,
                        output_dir = save_dir,
                        output_basename = model_nam,
                        iter_warmup = 1000, 
                        iter_sampling = 1000,
                        chains = 4, 
                        parallel_chains = 4,
                        max_treedepth=20 ) #, adapt_delta=0.95 ,init=0


            # # checking results
            # files = file.path( save_dir, paste0( model_nam, '-', 1:4, '.csv') )
            # var_int = c('bXc','bXd','bYc','bYd','log_bZc','log_bZd',
            #             'ei_s','eir_s1','eir_s2','log_ek_s')
            # BTLm = read_cmdstan_csv( files=files, variables=var_int )
            # par_draw = as_draws_df( BTLm$post_warmup_draws )
            # par_sum = summarise_draws( par_draw,
            #                            mean, median, sd, mad, min, max,
            #                            ~quantile2(.x, probs=c(0.025,0.975)),
            #                            ~HPDI(samples=.x, prob=0.95),
            #                            default_convergence_measures() )
            # par_sum[,-1] = round(par_sum[,-1],3)
            # View( par_sum[,c(1:4,8:ncol(par_sum))] )
            # tri_plot(draws=BTLm$post_warmup_draws, pars=c('bXc','bXd[1]','bXd[2]'))
            # tri_plot(draws=BTLm$post_warmup_draws, pars=c('bYc','bYd[1]','bYd[2]'))
            # tri_plot(draws=BTLm$post_warmup_draws, pars=c('log_bZc','log_bZd[1]','log_bZd[2]'))
            # tri_plot(draws=BTLm$post_warmup_draws, pars=c('ei_s[1]','ei_s[2]'))
            # tri_plot(draws=BTLm$post_warmup_draws, pars=c('eir_s1','eir_s2[1]','eir_s2[2]'))
            # tri_plot(draws=BTLm$post_warmup_draws, pars=c('log_ek_s[1]','log_ek_s[2]'))
            
            
            # file location
            files = file.path( save_dir, paste0( model_nam, '-', 1:4, '.csv') )
            
            
            # load desired results
            var_int = c()
            if( type %in% c('units','both') ){
              var_int = c( var_int, rownames(units) )
            } 
            if( type %in% c('judges','both') ){
              var_int = c( var_int, rownames(judges) )
            }
            var_int = c( var_int,'log_bZd','eir_s1')
            BTLm = read_cmdstan_csv( files=files, variables=var_int )
            
            
            # stoping condition
            model_noexist <<- !exists('BTLm')
          },
          error = function(e){
            # stoping condition
            model_noexist <<- !exists('BTLm')
          }
        )
        
      }
      
      
      # store results
      par_draw = as_draws_df( BTLm$post_warmup_draws )
      par_draw = par_draw[,-c( (ncol(par_draw)-2):ncol(par_draw) )]
      # str(par_draw)
      
      
      # make contrasts
      colm = rbind(c(1,2),c(3:4))
      for( cl in 1:nrow(colm) ){
        par_mom = par_draw[,colm[cl,1]] - par_draw[,colm[cl,2]]
        names(par_mom) = paste( names(par_draw)[colm[cl,]], collapse='-')
        if( cl==1 ){
          par_contr = par_mom
        } else{
          par_contr = cbind(par_contr, par_mom)
        }
      }
      names(par_contr)[1] = 'Ti[a]-Ti[b]'
      
      
      # add remaining parameters
      par_contr = cbind(par_contr, par_draw[,(max(colm)+1):ncol(par_draw)])
      par_contr = as_draws_df(par_contr)
      
      
      # summary
      pv = (1-p)/2
      probs = c(pv, 1-pv)
      par_sum = summarise_draws( par_contr, 
                                 mean, median, sd, mad, min, max, 
                                 ~quantile2(.x, probs=probs),
                                 ~HPDI(samples=.x, prob=p),
                                 default_convergence_measures() )
      names(par_sum)[c(2,10:11)] = c('est',paste0( 'hpdi', probs*100 ) )
      # View(par_sum)
      
      
      # RMSE
      true_par = c( with(d_int, Ti[1]-Ti[2]), 
                    with(pop_data$param, log_bZ[2]-log_bZ[3]),
                    pop_data$param$eir_s[1] )
      par_rmse = rmse( par_draws=par_contr[,1:3] , true_par=true_par )
      par_sum$rmse = par_rmse
      
      
      # rope goals
      goals_mom = rbind( goals_mom, 
                         rope_goals( par_true=true_par, par_sum=par_sum ))
      
      
      # print progress
      print( paste0( 'finished simulation ',s,'/',nsim,', param comb: ',pr,'/',nrow(params) ) )
      
    }
    
    # calculating power
    power_storage = rbind( power_storage, 
                           cbind( params[pr,], 
                                  power_report( goals_full = goals_mom ) ) )
    
    # save to file
    results = list(power_storage, seed_pop=seed_pop, seed_sample=seed_sample )
    saveRDS(results, file.path(save_dir, "sim_RQ1.rds") )
    
    
  }

}




#' #' Simulation for RQ1, using variational sampling
#' #'
#' #' @param nsim number of simulation, default=1000
#' #' @param p probability, default=0.95
#' #' @param params grid of parameter to simulate
#' #' @param seed_pop seed for population replication, default=NULL 
#' #' @param seed_sample list of seeds for sample replication, default=NULL
#' #' @param save_dir directory for saving results, default=getwd()
#' #' @param mod_dir directory for the compiled model, default=getwd()
#' #'
#' #' @return data.frame with power test
#' #' @export
#' #'
#' #' @examples
#' sim_RQ1_var = function(
#'     nsim = 1000,
#'     p = 0.95,
#'     params,
#'     seed_pop = NULL,
#'     seed_sample = NULL,
#'     save_dir = getwd(),
#'     mod_dir = getwd() ){
#'   
#'   # # test
#'   # nsim = 5
#'   # p = 0.95
#'   # params
#'   # seed_pop = NULL
#'   # seed_sample = NULL
#'   # save_dir = file.path(dir, '_results')
#'   # mod_dir = file.path(dir, 'code')
#'   
#'   
#'   # defined parameters
#'   # population
#'   I = 1000
#'   R = 200 
#'   K = 1000 
#'   bX = c( bXc=0, bXd=c(0,0) )
#'   bY = c( bYc=0, bYd=c(0,0) )
#'   log_bZ = c( bZc=0, bZd=log(c(0.75,0.95)) )
#'   ei_s = c(1,1)
#'   # eir_s = c(0.5,0.5)
#'   log_ek_s = c(0.02,0.02) 
#'   X_sup_c = c(-3,3)
#'   Y_sup_c = c(-3,3)
#'   Z_sup_c = c(-3,3)
#'   X_sup_d = 1:2
#'   Y_sup_d = 1:2
#'   Z_sup_d = 1:2
#'   # seed = NULL
#'   dec = 3 
#'   
#'   # sample mechanism
#'   nI = 30
#'   # nR = 10
#'   # nK = 50
#'   pI = rep(1/max(X_sup_d), max(X_sup_d))
#'   pR = rep(1/max(Y_sup_d), max(Y_sup_d))
#'   pK = rep(1/max(Z_sup_d), max(Z_sup_d))
#'   
#'   idx = NULL
#'   type = 'units'
#'   # seed = NULL 
#'   
#'   # comparison mechanism
#'   # ncomp = 20
#'   # seed = NULL
#'   
#'   
#'   
#'   # storage of results
#'   power_storage = c()
#'   
#'   
#'   # seeds for replication
#'   check1 = is.null(seed_pop)
#'   check2 = nrow(params) != length(seed_pop)
#'   if( check1 ){
#'     seed_pop = sample( 1:10^6, size=nrow(params), replace=F )
#'   } else if( check2 ){
#'     warning( paste0('you need to provide ', nrow(params), ' seeds ',
#'                     'for a fully replicable population simulation',
#'                     '\n','you provided ', length(seed_pop),' seed(s), ', 
#'                     nrow(params)-length(seed_pop), ' seed(s) have been simulated.') )
#'     seed_mom = sample( 1:10^6, size=nrow(params)-length(seed_pop), replace=F )
#'     seed_pop = c(seed_pop, seed_mom)
#'   }
#'   
#'   check1 = is.null(seed_sample)
#'   check2 = nrow(params) != length(seed_sample)
#'   check3 = any( sapply(seed_sample, length) != rep(nsim, nrow(params)) )
#'   if( check1 ){
#'     seed_sample = vector('list',nrow(params))
#'     for( i in 1:nrow(params) ){
#'       seed_sample[[i]] = sample( 1:10^6, size=nsim, replace=F )
#'     }
#'   } else if( check2 | check3 ){
#'     warning( paste0('you need to provide a list with ', nrow(params), ' objects, ',
#'                     'each with a length of ', nsim, ' seeds ',
#'                     '\n','for a fully replicable sample simulation',
#'                     '\n','you provided a list of ', length(seed_sample),' objects, ', 
#'                     'each with different or equal lengths', 
#'                     '\n','new objects and seed(s) have been simulated.') )
#'     for( i in 1:nrow(params) ){
#'       
#'       if( i <= length(seed_sample) ){
#'         seed = sample( 1:10^6, size=nsim-length(seed_sample[[i]]), replace=F )
#'         seed_sample[[i]] = c(seed_sample[[i]], seed)  
#'       } else{
#'         seed = sample( 1:10^6, size=nsim, replace=F )
#'         seed_sample = c(seed_sample, seed)  
#'       }
#'       
#'     }
#'     
#'   }
#'   
#'   
#'   # load model
#'   set_cmdstan_path('/home/josema/cmdstan')
#'   model_nam = 'BTLm'
#'   mod = cmdstan_model( file.path( mod_dir, paste0(model_nam,'.stan') ) )
#'   
#'   
#'   # simulation
#'   # pr=1
#'   for( pr in 1:nrow(params) ){ # 
#'     
#'     # population simulation
#'     pop_data = pop_sim( I = I, 
#'                         R = R, 
#'                         K = K, 
#'                         bX = bX,
#'                         bY = bY,
#'                         log_bZ = log_bZ,
#'                         ei_s = ei_s,
#'                         eir_s = unlist( params[pr, c('eir_s1','eir_s2')] ),
#'                         log_ek_s = log_ek_s,
#'                         X_sup_c = X_sup_c,
#'                         Y_sup_c = Y_sup_c,
#'                         Z_sup_c = Z_sup_c,
#'                         X_sup_d = X_sup_d,
#'                         Y_sup_d = Y_sup_d,
#'                         Z_sup_d = Z_sup_d,
#'                         seed = seed_pop[pr],
#'                         dec = dec ) 
#'     # hist(pop_data$judges$Gk)
#'     
#'     # storage goal
#'     goals_mom = c()
#'     
#'     # sample and comparison simulation
#'     # s=1
#'     for(s in 1:nsim ){  
#'       
#'       # selecting units/judges
#'       # (ensuring at least 2 with minimum req_diff are included)
#'       d_int = data_int( pop_data = pop_data, 
#'                         req_diff = params$req_diff[pr], 
#'                         type = type )
#'       
#'       # sample mechanism
#'       sizes = sample_sizes( nI = nI,
#'                             nR = params$nR[pr], 
#'                             nK = params$nK[pr],
#'                             pI = pI, 
#'                             pR = pR, 
#'                             pK = pK )
#'       
#'       data_sample = sm_srandom( pop_data = pop_data, 
#'                                 sizes = sizes,
#'                                 idx = d_int, 
#'                                 type = type, 
#'                                 seed = seed_sample[[pr]][s] )
#'       # hist(data_sample$judges$Gk)
#'       
#'       
#'       # selected data
#'       if( type %in% c('units','both') ){
#'         idx = data_sample$units$unit_sel==1
#'         units = data_sample$units[idx,]
#'         units = unique( units[,c('iidm','Ti')])
#'         rownames(units) = paste0( colnames(units)[2],'[',units$iidm,']')
#'       } 
#'       if( type %in% c('judges','both') ){
#'         idx = data_sample$judges$judge_sel==1
#'         judges = data_sample$judges[idx,]
#'         judges = unique( judges[,c('kidm','Gk')])
#'         rownames(judges) = paste0( colnames(judges)[2],'[',judges$kidm,']')
#'       }
#'       
#'       
#'       # comparison mechanism
#'       data_comp = cm_random( data_sample = data_sample, 
#'                              ncomp = params$ncomp[pr],
#'                              seed = seed_sample[[pr]][s] )  
#'       # str(data_comp)
#'       
#'       
#'       # data list
#'       data_list = list_data( data_obs=data_comp$d_obs )
#'       
#'       equal_ei_s = as.numeric( ei_s[1]==ei_s[2] )
#'       equal_eir_s = as.numeric( params$eir_s1[pr] == params$eir_s2[pr] )
#'       equal_log_ek_s = as.numeric( log_ek_s[1]==log_ek_s[2] )
#'       data_list = c( equal_ei_s=equal_ei_s,
#'                      equal_eir_s=equal_eir_s,
#'                      equal_log_ek_s=equal_log_ek_s,
#'                      data_list )
#'       # str(data_list)
#'       
#'       
#'       # ensuring no prior files 
#'       files = file.path( save_dir, paste0( model_nam, '-', 1:4, '.csv') )
#'       if( any(file.exists(files)) ) {
#'         sapply(files, file.remove)
#'       }
#'       
#'       
#'       model_noexist = T
#'       while( model_noexist ){
#'         
#'         tryCatch(
#'           expr = {
#'             # run model
#'             mod$variational( data = data_list,
#'                              output_dir = save_dir,
#'                              output_basename = model_nam,
#'                              draws = 2000 )
#'             
#'             
#'             # checking results
#'             files = file.path( save_dir, paste0( model_nam, '-', 1, '.csv') )
#'             var_int = c('bXc','bXd','bYc','bYd','log_bZc','log_bZd',
#'                         'ei_s','eir_s1','eir_s2','log_ek_s')
#'             BTLm = read_cmdstan_csv( files=files, variables=var_int )
#'             par_draw = as_draws_df( BTLm$draws )
#'             par_sum = summarise_draws( par_draw,
#'                                        mean, median, sd, mad, min, max,
#'                                        ~quantile2(.x, probs=c(0.025,0.975)),
#'                                        ~HPDI(samples=.x, prob=0.95),
#'                                        default_convergence_measures() )
#'             par_sum[,-1] = round(par_sum[,-1],3)
#'             View( par_sum[,c(1:4,8:ncol(par_sum))] )
#'             tri_plot(draws=BTLm$draws, pars=c('bXc','bXd[1]','bXd[2]'))
#'             tri_plot(draws=BTLm$draws, pars=c('bYc','bYd[1]','bYd[2]'))
#'             tri_plot(draws=BTLm$draws, pars=c('log_bZc','log_bZd[1]','log_bZd[2]'))
#'             tri_plot(draws=BTLm$draws, pars=c('ei_s[1]','ei_s[2]'))
#'             tri_plot(draws=BTLm$draws, pars=c('eir_s1','eir_s2[1]','eir_s2[2]'))
#'             tri_plot(draws=BTLm$draws, pars=c('log_ek_s[1]','log_ek_s[2]'))
#'             
#'             
#'             # file location
#'             files = file.path( save_dir, paste0( model_nam, '-', 1, '.csv') )
#'             
#'             
#'             # load desired results
#'             var_int = c()
#'             if( type %in% c('units','both') ){
#'               var_int = c( var_int, rownames(units) )
#'             } 
#'             if( type %in% c('judges','both') ){
#'               var_int = c( var_int, rownames(judges) )
#'             }
#'             var_int = c( var_int,'log_bZd','eir_s1')
#'             BTLm = read_cmdstan_csv( files=files, variables=var_int )
#'             
#'             
#'             # stoping condition
#'             model_noexist <<- !exists('BTLm')
#'           },
#'           error = function(e){
#'             # stoping condition
#'             model_noexist <<- !exists('BTLm')
#'           }
#'         )
#'         
#'       }
#'       
#'       
#'       # store results
#'       par_draw = as_draws_df( BTLm$draws )
#'       par_draw = par_draw[,-c( 1, (ncol(par_draw)-2):ncol(par_draw) )]
#'       # str(par_draw)
#'       
#'       
#'       # make contrasts
#'       colm = rbind(c(1,2),c(3:4))
#'       for( cl in 1:nrow(colm) ){
#'         par_mom = par_draw[,colm[cl,1]] - par_draw[,colm[cl,2]]
#'         names(par_mom) = paste( names(par_draw)[colm[cl,]], collapse='-')
#'         if( cl==1 ){
#'           par_contr = par_mom
#'         } else{
#'           par_contr = cbind(par_contr, par_mom)
#'         }
#'       }
#'       names(par_contr)[1] = 'Ti[a]-Ti[b]'
#'       
#'       
#'       # add remaining parameters
#'       par_contr = cbind(par_contr, par_draw[,(max(colm)+1):ncol(par_draw)])
#'       par_contr = as_draws_df(par_contr)
#'       
#'       
#'       # summary
#'       pv = (1-p)/2
#'       probs = c(pv, 1-pv)
#'       par_sum = summarise_draws( par_contr, 
#'                                  mean, median, sd, mad, min, max, 
#'                                  ~quantile2(.x, probs=probs),
#'                                  ~HPDI(samples=.x, prob=p),
#'                                  default_convergence_measures() )
#'       names(par_sum)[c(2,10:11)] = c('est',paste0( 'hpdi', probs*100 ) )
#'       # View(par_sum)
#'       
#'       
#'       # RMSE
#'       true_par = c( with(d_int, Ti[1]-Ti[2]), 
#'                     with(pop_data$param, log_bZ[2]-log_bZ[3]),
#'                     pop_data$param$eir_s[1] )
#'       par_rmse = rmse( par_draws=par_contr[,1:3] , true_par=true_par )
#'       par_sum$rmse = par_rmse
#'       
#'       
#'       # rope goals
#'       goals_mom = rbind( goals_mom, 
#'                          rope_goals( par_true=true_par, par_sum=par_sum ))
#'       
#'       
#'       # print progress
#'       print( paste0( 'finished simulation ',s,'/',nsim,', param comb: ',pr,'/',nrow(params) ) )
#'       
#'     }
#'     
#'     # calculating power
#'     power_storage = rbind( power_storage, 
#'                            cbind( params[pr,], 
#'                                   power_report( goals_full = goals_mom ) ) )
#'     
#'     # save to file
#'     results = list(power_storage, seed_pop=seed_pop, seed_sample=seed_sample )
#'     saveRDS(results, file.path(save_dir, "sim_RQ1.rds") )
#'     
#'     
#'   }
#'   
#' }





#' Simulation for RQ1, using HMC sampling
#'
#' @param nsim number of simulation, default=1000
#' @param p probability, default=0.95
#' @param params grid of parameter to simulate
#' @param seed_pop seed for population replication, default=NULL 
#' @param seed_sample list of seeds for sample replication, default=NULL
#' @param save_dir directory for saving results, default=getwd()
#' @param mod_dir directory for the compiled model, default=getwd()
#'
#' @return data.frame with power test
#' @export
#'
#' @examples
sim_RQ2_hmc = function(
    nsim = 1000,
    p = 0.95,
    params,
    seed_pop = NULL,
    seed_sample = NULL,
    save_dir = getwd(),
    mod_dir = getwd() ){
  
  # # test
  # nsim = 5
  # p = 0.95
  # params
  # seed_pop = NULL
  # seed_sample = NULL
  # save_dir = file.path(dir, '_results')
  # mod_dir = file.path(dir, 'code')
  
  
  # defined parameters
  # population
  I = 1000
  R = 200 
  K = 1000 
  bX = c( bXc=0, bXd=c(0,0) )
  bY = c( bYc=0, bYd=c(0,0) )
  log_bZ = c( bZc=0, bZd=log(c(0.75,0.95)) )
  ei_s = c(1,1)
  # eir_s = c(0.5,0.5)
  log_ek_s = c(0.02,0.02) 
  X_sup_c = c(-3,3)
  Y_sup_c = c(-3,3)
  Z_sup_c = c(-3,3)
  X_sup_d = 1:2
  Y_sup_d = 1:2
  Z_sup_d = 1:2
  # seed = NULL
  dec = 3 
  
  # sample mechanism
  nI = 30
  # nR = 10
  # nK = 50
  pI = rep(1/max(X_sup_d), max(X_sup_d))
  pR = rep(1/max(Y_sup_d), max(Y_sup_d))
  pK = rep(1/max(Z_sup_d), max(Z_sup_d))
  
  idx = NULL
  type = 'units'
  # seed = NULL 
  
  # comparison mechanism
  # ncomp = 20
  # seed = NULL
  
  
  
  # storage of results
  power_storage = c()
  
  
  # seeds for replication
  check1 = is.null(seed_pop)
  check2 = nrow(params) != length(seed_pop)
  if( check1 ){
    seed_pop = sample( 1:10^6, size=nrow(params), replace=F )
  } else if( check2 ){
    warning( paste0('you need to provide ', nrow(params), ' seeds ',
                    'for a fully replicable population simulation',
                    '\n','you provided ', length(seed_pop),' seed(s), ', 
                    nrow(params)-length(seed_pop), ' seed(s) have been simulated.') )
    seed_mom = sample( 1:10^6, size=nrow(params)-length(seed_pop), replace=F )
    seed_pop = c(seed_pop, seed_mom)
  }
  
  check1 = is.null(seed_sample)
  check2 = nrow(params) != length(seed_sample)
  check3 = any( sapply(seed_sample, length) != rep(nsim, nrow(params)) )
  if( check1 ){
    seed_sample = vector('list',nrow(params))
    for( i in 1:nrow(params) ){
      seed_sample[[i]] = sample( 1:10^6, size=nsim, replace=F )
    }
  } else if( check2 | check3 ){
    warning( paste0('you need to provide a list with ', nrow(params), ' objects, ',
                    'each with a length of ', nsim, ' seeds ',
                    '\n','for a fully replicable sample simulation',
                    '\n','you provided a list of ', length(seed_sample),' objects, ', 
                    'each with different or equal lengths', 
                    '\n','new objects and seed(s) have been simulated.') )
    for( i in 1:nrow(params) ){
      
      if( i <= length(seed_sample) ){
        seed = sample( 1:10^6, size=nsim-length(seed_sample[[i]]), replace=F )
        seed_sample[[i]] = c(seed_sample[[i]], seed)  
      } else{
        seed = sample( 1:10^6, size=nsim, replace=F )
        seed_sample = c(seed_sample, seed)  
      }
      
    }
    
  }
  
  
  # load model
  set_cmdstan_path('/home/josema/cmdstan')
  model_nam = 'BTLm'
  mod = cmdstan_model( file.path( mod_dir, paste0(model_nam,'.stan') ) )
  
  
  # simulation
  # pr=2
  for( pr in 1:nrow(params) ){ # 
    
    # population simulation
    pop_data = pop_sim( I = I, 
                        R = R, 
                        K = K, 
                        bX = bX,
                        bY = bY,
                        log_bZ = log_bZ,
                        ei_s = ei_s,
                        eir_s = unlist( params[pr, c('eir_s1','eir_s2')] ),
                        log_ek_s = log_ek_s,
                        X_sup_c = X_sup_c,
                        Y_sup_c = Y_sup_c,
                        Z_sup_c = Z_sup_c,
                        X_sup_d = X_sup_d,
                        Y_sup_d = Y_sup_d,
                        Z_sup_d = Z_sup_d,
                        seed = seed_pop[pr],
                        dec = dec ) 
    # hist(pop_data$judges$Gk)
    
    # storage goal
    goals_mom = c()
    
    # sample and comparison simulation
    # s=1
    for(s in 1:nsim ){  
      
      # selecting units/judges
      # (ensuring at least 2 with minimum req_diff are included)
      d_int = data_int( pop_data = pop_data, 
                        req_diff = params$req_diff[pr], 
                        type = type )
      
      # sample mechanism
      sizes = sample_sizes( nI = nI,
                            nR = params$nR[pr], 
                            nK = params$nK[pr],
                            pI = pI, 
                            pR = pR, 
                            pK = pK )
      
      data_sample = sm_srandom( pop_data = pop_data, 
                                sizes = sizes,
                                idx = d_int, 
                                type = type, 
                                seed = seed_sample[[pr]][s] )
      # hist(data_sample$judges$Gk)
      
      
      # selected data
      if( type %in% c('units','both') ){
        idx = data_sample$units$unit_sel==1
        units = data_sample$units[idx,]
        units = unique( units[,c('iidm','Ti')])
        rownames(units) = paste0( colnames(units)[2],'[',units$iidm,']')
      } 
      if( type %in% c('judges','both') ){
        idx = data_sample$judges$judge_sel==1
        judges = data_sample$judges[idx,]
        judges = unique( judges[,c('kidm','Gk')])
        rownames(judges) = paste0( colnames(judges)[2],'[',judges$kidm,']')
      }
      
      
      # comparison mechanism
      data_comp = cm_random( data_sample = data_sample, 
                             ncomp = params$ncomp[pr],
                             seed = seed_sample[[pr]][s] )  
      # str(data_comp)
      
      
      # data list
      data_list = list_data( data_obs=data_comp$d_obs )
      
      equal_ei_s = as.numeric( ei_s[1]==ei_s[2] )
      equal_eir_s = as.numeric( params$eir_s1[pr] == params$eir_s2[pr] )
      equal_log_ek_s = as.numeric( log_ek_s[1]==log_ek_s[2] )
      data_list = c( equal_ei_s=equal_ei_s,
                     equal_eir_s=equal_eir_s,
                     equal_log_ek_s=equal_log_ek_s,
                     data_list )
      # str(data_list)
      
      
      # ensuring no prior files 
      files = file.path( save_dir, paste0( model_nam, '-', 1:4, '.csv') )
      if( any(file.exists(files)) ) {
        sapply(files, file.remove)
      }
      
      
      model_noexist = T
      while( model_noexist ){
        
        tryCatch(
          expr = {
            # run model
            mod$sample( data = data_list,
                        output_dir = save_dir,
                        output_basename = model_nam,
                        iter_warmup = 1000, 
                        iter_sampling = 1000,
                        chains = 4, 
                        parallel_chains = 4,
                        max_treedepth=20 ) #, adapt_delta=0.95 ,init=0
            
            
            # # checking results
            # files = file.path( save_dir, paste0( model_nam, '-', 1:4, '.csv') )
            # var_int = c('bXc','bXd','bYc','bYd','log_bZc','log_bZd',
            #             'ei_s','eir_s1','eir_s2','log_ek_s')
            # BTLm = read_cmdstan_csv( files=files, variables=var_int )
            # par_draw = as_draws_df( BTLm$post_warmup_draws )
            # par_sum = summarise_draws( par_draw,
            #                            mean, median, sd, mad, min, max,
            #                            ~quantile2(.x, probs=c(0.025,0.975)),
            #                            ~HPDI(samples=.x, prob=0.95),
            #                            default_convergence_measures() )
            # par_sum[,-1] = round(par_sum[,-1],3)
            # View( par_sum[,c(1:4,8:ncol(par_sum))] )
            # tri_plot(draws=BTLm$post_warmup_draws, pars=c('bXc','bXd[1]','bXd[2]'))
            # tri_plot(draws=BTLm$post_warmup_draws, pars=c('bYc','bYd[1]','bYd[2]'))
            # tri_plot(draws=BTLm$post_warmup_draws, pars=c('log_bZc','log_bZd[1]','log_bZd[2]'))
            # tri_plot(draws=BTLm$post_warmup_draws, pars=c('ei_s[1]','ei_s[2]'))
            # tri_plot(draws=BTLm$post_warmup_draws, pars=c('eir_s1','eir_s2[1]','eir_s2[2]'))
            # tri_plot(draws=BTLm$post_warmup_draws, pars=c('log_ek_s[1]','log_ek_s[2]'))
            
            
            # file location
            files = file.path( save_dir, paste0( model_nam, '-', 1:4, '.csv') )
            
            
            # load desired results
            var_int = c()
            if( type %in% c('units','both') ){
              var_int = c( var_int, rownames(units) )
            } 
            if( type %in% c('judges','both') ){
              var_int = c( var_int, rownames(judges) )
            }
            var_int = c( var_int,'log_bZd','eir_s1')
            BTLm = read_cmdstan_csv( files=files, variables=var_int )
            
            
            # stoping condition
            model_noexist <<- !exists('BTLm')
          },
          error = function(e){
            # stoping condition
            model_noexist <<- !exists('BTLm')
          }
        )
        
      }
      
      
      # store results
      par_draw = as_draws_df( BTLm$post_warmup_draws )
      par_draw = par_draw[,-c( (ncol(par_draw)-2):ncol(par_draw) )]
      # str(par_draw)
      
      
      # make contrasts
      colm = rbind(c(1,2),c(3:4))
      for( cl in 1:nrow(colm) ){
        par_mom = par_draw[,colm[cl,1]] - par_draw[,colm[cl,2]]
        names(par_mom) = paste( names(par_draw)[colm[cl,]], collapse='-')
        if( cl==1 ){
          par_contr = par_mom
        } else{
          par_contr = cbind(par_contr, par_mom)
        }
      }
      names(par_contr)[1] = 'Ti[a]-Ti[b]'
      
      
      # add remaining parameters
      par_contr = cbind(par_contr, par_draw[,(max(colm)+1):ncol(par_draw)])
      par_contr = as_draws_df(par_contr)
      
      
      # summary
      pv = (1-p)/2
      probs = c(pv, 1-pv)
      par_sum = summarise_draws( par_contr, 
                                 mean, median, sd, mad, min, max, 
                                 ~quantile2(.x, probs=probs),
                                 ~HPDI(samples=.x, prob=p),
                                 default_convergence_measures() )
      names(par_sum)[c(2,10:11)] = c('est',paste0( 'hpdi', probs*100 ) )
      # View(par_sum)
      
      
      # RMSE
      true_par = c( with(d_int, Ti[1]-Ti[2]), 
                    with(pop_data$param, log_bZ[2]-log_bZ[3]),
                    pop_data$param$eir_s[1] )
      par_rmse = rmse( par_draws=par_contr[,1:3] , true_par=true_par )
      par_sum$rmse = par_rmse
      
      
      # rope goals
      goals_mom = rbind( goals_mom, 
                         rope_goals( par_true=true_par, par_sum=par_sum ))
      
      
      # print progress
      print( paste0( 'finished simulation ',s,'/',nsim,', param comb: ',pr,'/',nrow(params) ) )
      
    }
    
    # calculating power
    power_storage = rbind( power_storage, 
                           cbind( params[pr,], 
                                  power_report( goals_full = goals_mom ) ) )
    
    # save to file
    results = list(power_storage, seed_pop=seed_pop, seed_sample=seed_sample )
    saveRDS(results, file.path(save_dir, "sim_RQ1.rds") )
    
    
  }
  
}
