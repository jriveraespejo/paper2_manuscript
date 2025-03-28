# equal discriminal dispersions ####

mcmc_code = "
data{

    // data dimensions
    int ns;     // number of experimental runs
    int nsI;    // max. number of individuals
    int nsA;    // max. number of stimuli
    int nsJ;    // max. number of judges
    int nsK;    // max. number of judgments
    
    // categories discrete variables
    int sup_XId;  // max. number of categories in XI
    int sup_XAd;  // max. number of categories in XA
    int sup_ZJd;  // max. number of categories in ZJ
    int sup_ZKd;  // max. number of categories in ZK
    
    // comparison data
    array[ns] int<lower=1, upper=nsI> Rs1;   // individual (left)
    array[ns] int<lower=1, upper=nsA> Ps1;   // stimuli (left)
    array[ns] int<lower=1, upper=nsI> Rs2;   // individual (right)
    array[ns] int<lower=1, upper=nsA> Ps2;   // stimuli (right)
    array[ns] int<lower=1, upper=nsJ> Us;    // judges
    array[ns] int<lower=1, upper=nsK> Vs;    // judgments
    array[ns] int<lower=0, upper=1> ORPUV;   // outcome (dichotomous)
    
    // individual-stimuli data
    array[nsI*nsA] int<lower=1, upper=nsI> IA1s;    // individuals
    array[nsI*nsA] int<lower=1, upper=nsA> IA2s;    // stimuli
    array[nsI*nsA] real XIc;                        // individual predictor (continuous)
    array[nsI*nsA] int<lower=1, upper=sup_XId> XId; // individual predictor (discrete)
    array[nsI*nsA] real XIAc;                       // stimuli predictor (continuous)
    array[nsI*nsA] int<lower=1, upper=sup_XAd> XIAd;// stimuli predictor (discrete)
    
    // juges-repeated comparisons data
    array[nsJ*nsK] int<lower=1, upper=nsJ> JK1s;    // judges
    array[nsJ*nsK] int<lower=1, upper=nsK> JK2s;    // judgments
    array[nsJ*nsK] real ZJc;                        // judges predictor (continuous)
    array[nsJ*nsK] int<lower=1, upper=sup_ZJd> ZJd; // judges predictor (discrete)
    array[nsJ*nsK] real ZJKc;                       // judgments predictor (continuous)
    array[nsJ*nsK] int<lower=1, upper=sup_ZKd> ZJKd;// judgments predictor (discrete)
    
}
parameters{
    
    // covariate effects
    real bXIc;            // individuals (continuous)
    vector[sup_XId] bXId; // individuals (discrete)
    real bXAc;            // stimuli (continuous)
    vector[sup_XAd] bXAd; // stimuli (discrete)
    real bZJc;            // judges (continuous)
    vector[sup_ZJd] bZJd; // judges (discrete)
    real bZKc;            // repeated comparisons (continuous)
    vector[sup_ZKd] bZKd; // repeated comparisons (discrete)
    
    
    // error parameters 
    vector[nsI] zeI;            // individual errors (non-centered)
    real<lower=0,upper=1> pIA;  // stimuli proportion of variability
    matrix[nsI,nsA] zeIA;       // stimuli errors (non-centered)

    vector[nsJ] zeJ;            // judges errors (identification)
    real<lower=0,upper=1> pJK;  // judgments proportion of variability
    matrix[nsJ,nsK] zeJK;       // judgments errors (non-centered)

}
transformed parameters{
    
    // declaring
    vector[nsI] TI;       // individuals' trait
    vector[nsI] eI;       // individuals errors (identification)
    matrix[nsI,nsA] TIA;  // stimuli trait
    matrix[nsI,nsA] eIA;  // stimuli errors
    
    vector[nsJ] BJ;       // judges' trait
    vector[nsJ] eJ;       // judges' errors (identification)
    matrix[nsJ,nsK] BJK;  // judges-judgments trait
    matrix[nsJ,nsK] eJK;  // judgments errors
    
    
    // individuals-stimuli
    for( ia in 1:(nsI*nsA) ){
    
      // error calculation
      eI[ IA1s[ia] ] = zeI[ IA1s[ia] ];                             // identification: sI = 1
      eIA[ IA1s[ia], IA2s[ia] ] = pIA * zeIA[ IA1s[ia], IA2s[ia] ]; // identification: pIA prop. of previous level
      
      // trait calculation
      TI[ IA1s[ia] ] = bXIc*XIc[ ia ] + bXId[ XId[ia] ] + eI[ IA1s[ia] ];
      TIA[ IA1s[ia], IA2s[ia] ] = TI[ IA1s[ia] ] + bXAc*XIAc[ ia ] + bXAd[ XIAd[ia] ] + eIA[ IA1s[ia], IA2s[ia] ];
      
    }
    
    // judges-repeated comparisons
    if( nsK == 1 ){
      for( jk in 1:(nsJ*nsK) ){
      
        // error calculation
        eJ[ JK1s[jk] ] = zeJ[ JK1s[jk] ];                             // identification: sJ = 1
        eJK[ JK1s[jk], JK2s[jk] ] = pJK * zeJK[ JK1s[jk], JK2s[jk] ]; // identification: pJK prop. of previous level
        
        // trait calculation
        BJ[ JK1s[jk] ] = bZJc*ZJc[ jk ] + bZJd[ ZJd[jk] ] + eJ[ JK1s[jk] ];
        BJK[ JK1s[jk], JK2s[jk] ] = BJ[ JK1s[jk] ] + bZKc*ZJKc[ jk ] + bZKd[ ZJKd[jk] ]; // no repetitions -> log_eJK not possible
        
      }
    } else {
      for( jk in 1:(nsJ*nsK) ){
        
        // error calculation
        eJ[ JK1s[jk] ] = zeJ[ JK1s[jk] ];                             // identification: sJ = 1
        eJK[ JK1s[jk], JK2s[jk] ] = pJK * zeJK[ JK1s[jk], JK2s[jk] ]; // identification: pJK prop. of previous level
        
        // trait calculation
        BJ[ JK1s[jk] ] = bZJc*ZJc[ jk ] + bZJd[ ZJd[jk] ] + eJ[ JK1s[jk] ];
        BJK[ JK1s[jk], JK2s[jk] ] = BJ[ JK1s[jk] ] + bZKc*ZJKc[ jk ] + bZKd[ ZJKd[jk] ] + eJK[ JK1s[jk], JK2s[jk] ];
        
      }
    }
    
}
model{
    
    // no track
    vector[ns] DRPUV;               // discriminal difference
    
    
    // priors
    // covariate effects
    bXIc ~ normal( 0, 0.2 );
    bXId ~ normal( 0, 0.2 );
    bXAc ~ normal( 0, 0.2 );
    bXAd ~ normal( 0, 0.2 );
    bZJc ~ normal( 0, 0.2 );
    bZJd ~ normal( 0, 0.2 );
    bZKc ~ normal( 0, 0.2 );
    bZKd ~ normal( 0, 0.2 );
    
    
    // errors 
    zeI ~ std_normal();               // identification
    pIA ~ beta_proportion(0.5, 5);   // identification
    to_vector( zeIA ) ~ std_normal();

    zeJ ~ std_normal();               // identification
    pJK ~ beta_proportion(0.5, 5);   // identification
    to_vector( zeJK ) ~ std_normal();

    
    // likelihood
    for( n in 1:ns ){
      DRPUV[n] = ( TIA[ Rs1[n], Ps1[n] ] - TIA[ Rs2[n], Ps2[n] ] ) + BJK[ Us[n], Vs[n] ];
    }
    ORPUV ~ bernoulli_logit( DRPUV );
    
}
generated quantities{

    // track
    vector[ns] DRPUV;   // discriminal difference
    vector[ns] log_lik; // log-likelihood
    
    // log-likelihood
    for( n in 1:ns ){
      DRPUV[n] = ( TIA[ Rs1[n], Ps1[n] ] - TIA[ Rs2[n], Ps2[n] ] ) + BJK[ Us[n], Vs[n] ];
      log_lik[n] = bernoulli_logit_lpmf( ORPUV[n] | DRPUV[n] );
    }
    
}
"

# saving
dir = '/home/josema/Desktop/1. Work/1 research/PhD Antwerp/#thesis/paper2/paper2_manuscript/'
model_nam = "CJmod_add_equal.stan"
writeLines(mcmc_code, con=file.path(dir, 'code', model_nam) )




# different discriminal dispersions ####

mcmc_code = "
data{

    // data dimensions
    int ns;     // number of experimental runs
    int nsI;    // max. number of individuals
    int nsA;    // max. number of stimuli
    int nsJ;    // max. number of judges
    int nsK;    // max. number of judgments
    
    // categories discrete variables
    int sup_XId;  // max. number of categories in XI
    int sup_XAd;  // max. number of categories in XA
    int sup_ZJd;  // max. number of categories in ZJ
    int sup_ZKd;  // max. number of categories in ZK
    
    // comparison data
    array[ns] int<lower=1, upper=nsI> Rs1;   // individual (left)
    array[ns] int<lower=1, upper=nsA> Ps1;   // stimuli (left)
    array[ns] int<lower=1, upper=nsI> Rs2;   // individual (right)
    array[ns] int<lower=1, upper=nsA> Ps2;   // stimuli (right)
    array[ns] int<lower=1, upper=nsJ> Us;    // judges
    array[ns] int<lower=1, upper=nsK> Vs;    // judgments
    array[ns] int<lower=0, upper=1> ORPUV;   // outcome (dichotomous)
    
    // individual-stimuli data
    array[nsI*nsA] int<lower=1, upper=nsI> IA1s;    // individuals
    array[nsI*nsA] int<lower=1, upper=nsA> IA2s;    // stimuli
    array[nsI*nsA] real XIc;                        // individual predictor (continuous)
    array[nsI*nsA] int<lower=1, upper=sup_XId> XId; // individual predictor (discrete)
    array[nsI*nsA] real XIAc;                       // stimuli predictor (continuous)
    array[nsI*nsA] int<lower=1, upper=sup_XAd> XIAd;// stimuli predictor (discrete)
    
    // juges-repeated comparisons data
    array[nsJ*nsK] int<lower=1, upper=nsJ> JK1s;    // judges
    array[nsJ*nsK] int<lower=1, upper=nsK> JK2s;    // judgments
    array[nsJ*nsK] real ZJc;                        // judges predictor (continuous)
    array[nsJ*nsK] int<lower=1, upper=sup_ZJd> ZJd; // judges predictor (discrete)
    array[nsJ*nsK] real ZJKc;                       // judgments predictor (continuous)
    array[nsJ*nsK] int<lower=1, upper=sup_ZKd> ZJKd;// judgments predictor (discrete)
    
}
parameters{
    
    // covariate effects
    real bXIc;            // individuals (continuous)
    vector[sup_XId] bXId; // individuals (discrete)
    real bXAc;            // stimuli (continuous)
    vector[sup_XAd] bXAd; // stimuli (discrete)
    real bZJc;            // judges (continuous)
    vector[sup_ZJd] bZJd; // judges (discrete)
    real bZKc;            // repeated comparisons (continuous)
    vector[sup_ZKd] bZKd; // repeated comparisons (discrete)
    
    
    // error parameters 
    simplex[sup_XId] hsI;                     // individuals half-sigma
    array[sup_XId] real<lower=0,upper=1> pIA; // stimuli proportion of variability
    vector[nsI] zeI;                          // individual errors (non-centered)
    matrix[nsI,nsA] zeIA;                     // stimuli errors (non-centered)

    simplex[sup_ZJd] hsJ;                     // judges half-sigma
    array[sup_ZJd] real<lower=0,upper=1> pJK; // judgments proportion of variability
    vector[nsJ] zeJ;                          // judges errors (identification)
    matrix[nsJ,nsK] zeJK;                     // judgments errors (non-centered)

}
transformed parameters{
    
    // declaring
    vector[sup_XId] sI;   // individuals sigma
    vector[nsI] eI;       // individuals errors (identification)
    vector[nsI] TI;       // individuals' trait
    matrix[nsI,nsA] eIA;  // stimuli errors
    matrix[nsI,nsA] TIA;  // stimuli trait
    
    
    vector[sup_ZJd] sJ;   // judges sigma
    vector[nsJ] eJ;       // judges' errors (identification)
    vector[nsJ] BJ;       // judges' trait
    matrix[nsJ,nsK] eJK;  // judgments errors
    matrix[nsJ,nsK] BJK;  // judges-judgments trait
    
    
    // individuals-stimuli
    sI = sup_XId * hsI;   // identification: sum(sI)=sup_XId & mean(sI)=1
    
    for( ia in 1:(nsI*nsA) ){
    
      // error calculation
      eI[ IA1s[ia] ] = sI[ XId[ia] ] * zeI[ IA1s[ia] ];  
      eIA[ IA1s[ia], IA2s[ia] ] = pIA[ XId[ia] ] * zeIA[ IA1s[ia], IA2s[ia] ]; // remove sI[ XId[ia] ] to make different levels independent
       
      // trait calculation
      TI[ IA1s[ia] ] = bXIc*XIc[ ia ] + bXId[ XId[ia] ] + eI[ IA1s[ia] ];
      TIA[ IA1s[ia], IA2s[ia] ] = TI[ IA1s[ia] ] + bXAc*XIAc[ ia ] + bXAd[ XIAd[ia] ] + eIA[ IA1s[ia], IA2s[ia] ];
      
    }
    
    
    // judges-repeated comparisons
    sJ = sup_ZJd * hsJ;     // identification: sum(sJ)=sup_ZJd & mean(sJ)=1
    
    if( nsK == 1 ){
      for( jk in 1:(nsJ*nsK) ){
      
        // error calculation
        eJ[ JK1s[jk] ] = sJ[ ZJd[jk] ] * zeJ[ JK1s[jk] ];
        
        // trait calculation
        BJ[ JK1s[jk] ] = bZJc*ZJc[ jk ] + bZJd[ ZJd[jk] ] + eJ[ JK1s[jk] ];
        BJK[ JK1s[jk], JK2s[jk] ] = BJ[ JK1s[jk] ] + bZKc*ZJKc[ jk ] + bZKd[ ZJKd[jk] ]; // no repetitions -> eJK not possible
        
      }
    } else {
      for( jk in 1:(nsJ*nsK) ){
        
        // error calculation
        eJ[ JK1s[jk] ] = sJ[ ZJd[jk] ] * zeJ[ JK1s[jk] ];
        eJK[ JK1s[jk], JK2s[jk] ] = pJK[ ZJd[jk] ] * zeJK[ JK1s[jk], JK2s[jk] ]; // remove sJ[ ZJd[jk] ] to make different levels independent
        
        // trait calculation
        BJ[ JK1s[jk] ] = bZJc*ZJc[ jk ] + bZJd[ ZJd[jk] ] + eJ[ JK1s[jk] ];
        BJK[ JK1s[jk], JK2s[jk] ] = BJ[ JK1s[jk] ] + bZKc*ZJKc[ jk ] + bZKd[ ZJKd[jk] ] + eJK[ JK1s[jk], JK2s[jk] ];
        
      }
    }
    
}
model{
    
    // no track
    vector[ns] DRPUV;               // discriminal difference
    
    
    // priors
    // covariate effects
    bXIc ~ normal( 0, 0.2 );
    bXId ~ normal( 0, 0.2 );
    bXAc ~ normal( 0, 0.2 );
    bXAd ~ normal( 0, 0.2 );
    bZJc ~ normal( 0, 0.2 );
    bZJd ~ normal( 0, 0.2 );
    bZKc ~ normal( 0, 0.2 );
    bZKd ~ normal( 0, 0.2 );
    
    
    // errors 
    zeI ~ std_normal();                         // identification
    hsI ~ dirichlet( rep_vector(5, sup_XId) );  // identification: sum(hsI)=1 & mean(hsI)=1/sup_XId
    pIA ~ beta_proportion(0.5, 5);              // identification
    to_vector( zeIA ) ~ std_normal();
    
    zeJ ~ std_normal();                         // identification
    hsJ ~ dirichlet( rep_vector(5, sup_ZJd) );  // identification: sum(hsJ)=1 & mean(hsJ)=1/sup_XId
    pJK ~ beta_proportion(0.5, 5);              // identification
    to_vector( zeJK ) ~ std_normal();
    
    
    // likelihood
    for( n in 1:ns ){
      DRPUV[n] = ( TIA[ Rs1[n], Ps1[n] ] - TIA[ Rs2[n], Ps2[n] ] ) + BJK[ Us[n], Vs[n] ];
    }
    ORPUV ~ bernoulli_logit( DRPUV );
    
}
generated quantities{

    // track
    vector[ns] DRPUV;   // discriminal difference
    vector[ns] log_lik; // log-likelihood
    
    // log-likelihood
    for( n in 1:ns ){
      DRPUV[n] = ( TIA[ Rs1[n], Ps1[n] ] - TIA[ Rs2[n], Ps2[n] ] ) + BJK[ Us[n], Vs[n] ];
      log_lik[n] = bernoulli_logit_lpmf( ORPUV[n] | DRPUV[n] );
    }
    
}
"

# saving
dir = '/home/josema/Desktop/1. Work/1 research/PhD Antwerp/#thesis/paper2/paper2_manuscript/'
model_nam = "CJmod_add_unequal.stan"
writeLines(mcmc_code, con=file.path(dir, 'code', model_nam) )
