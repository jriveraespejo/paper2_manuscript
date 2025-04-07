
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
    array[ns] int<lower=1, upper=nsI> R1;   // individual (left)
    array[ns] int<lower=1, upper=nsA> R2;   // stimuli (left)
    array[ns] int<lower=1, upper=nsI> R3;   // individual (right)
    array[ns] int<lower=1, upper=nsA> R4;   // stimuli (right)
    array[ns] int<lower=1, upper=nsJ> R5;    // judges
    array[ns] int<lower=1, upper=nsK> R6;    // judgments
    array[ns] int<lower=0, upper=1> OR;   // outcome (dichotomous)
    
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
        eJK[ JK1s[jk], JK2s[jk] ] = pJK * zeJK[ JK1s[jk], JK2s[jk] ]; // identification: pJK prop. of 1
        
        // trait calculation
        BJ[ JK1s[jk] ] = bZJc*ZJc[ jk ] + bZJd[ ZJd[jk] ] + eJ[ JK1s[jk] ];
        BJK[ JK1s[jk], JK2s[jk] ] = BJ[ JK1s[jk] ] + bZKc*ZJKc[ jk ] + bZKd[ ZJKd[jk] ]; // no repetitions -> log_eJK not possible
        
      }
    } else {
      for( jk in 1:(nsJ*nsK) ){
        
        // error calculation
        eJ[ JK1s[jk] ] = zeJ[ JK1s[jk] ];                             // identification: sJ = 1
        eJK[ JK1s[jk], JK2s[jk] ] = pJK * zeJK[ JK1s[jk], JK2s[jk] ]; // identification: pJK prop. of 1
        
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
      DRPUV[n] = ( TIA[ R1[n], R2[n] ] - TIA[ R3[n], R4[n] ] ) + BJK[ R5[n], R6[n] ];
    }
    OR ~ bernoulli_logit( DRPUV );
    
}
generated quantities{

    // track
    vector[ns] DRPUV;   // discriminal difference
    vector[ns] log_lik; // log-likelihood
    
    // log-likelihood
    for( n in 1:ns ){
      DRPUV[n] = ( TIA[ R1[n], R2[n] ] - TIA[ R3[n], R4[n] ] ) + BJK[ R5[n], R6[n] ];
      log_lik[n] = bernoulli_logit_lpmf( OR[n] | DRPUV[n] );
    }
    
}

