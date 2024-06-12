
data{

    // model vars
    int equal_ei_s;       // same variance for both groups of units
    int equal_eir_s;      // same variance for both groups of sub-units
    int equal_log_ek_s;   // same variance for both groups of judges
    
    // dimensions
    int N;    // number of experimental runs
    int I;    // max. number of units
    int R;    // max. number of sub-units
    int K;    // max. number of judges
    int cXd;  // max. number of categories in Xd
    int cYd;  // max. number of categories in Yd
    int cZd;  // max. number of categories in Zd
    
    
    // comparison data
    array[N] int<lower=1, upper=I> iidm_1;    // unit (left)
    array[N] int<lower=1, upper=R> ridm_1;    // sub-unit (left)
    array[N] int<lower=1, upper=I> iidm_2;    // unit (right)
    array[N] int<lower=1, upper=R> ridm_2;    // sub_unit (right)
    array[N] int<lower=1, upper=K> kidm_;     // judges
    array[N] int<lower=0, upper=1> Okijr;     // outcome (dichotomous)
    
    
    // units data
    array[I] int<lower=1, upper=I> iidm;      // (modified) index units
    vector[I] Xi_c;                           // covariate unit (continuous)
    array[I] int<lower=1, upper=cXd> Xi_d;    // covariate unit (categorical)
    
    
    // sub-units data
    array[I*R] int<lower=1, upper=I> iidm_;   // (modified) index units
    array[I*R] int<lower=1, upper=R> ridm;    // (modified) index sub-units
    vector[I*R] Yr_c;                         // covariate sub-unit (continuous)
    array[I*R] int<lower=1, upper=cYd> Yr_d;  // covariate sub-unit (categorical)
    
    
    // judges data
    array[K] int<lower=1, upper=K> kidm;      // (modified) index judges
    vector[K] Zk_c;                           // covariate sub-unit (continuous)
    array[K] int<lower=1, upper=cZd> Zk_d;    // covariate sub-unit (categorical)

}
parameters{
    
    // (fixed) covariate effects
    real bXc;             // unit (continuous)
    vector[cXd] bXd;      // unit (categorical)
    
    real bYc;             // sub-unit (continuous)
    vector[cYd] bYd;      // sub-unit (categorical)
    
    real log_bZc;         // judges (continuous)
    vector[cZd] log_bZd;  // judges (categorical)
    
    
    // random effects parameters 
    vector[I] z_ei;         // units random effects (non-centered)
    matrix[I,R] z_eir;      // sub-units random effects (non-centered)
    vector[K] z_log_ek;     // judges random effects (non-centered)
    
    // for one group
    real<lower=0> eir_s1;   // sub-units sigma
    
    // for multiple groups
    simplex[cXd] ei_s;      // units sigma
    simplex[cYd] eir_s2;    // sub-units sigma
    simplex[cZd] log_ek_s;  // judges sigma
    
}
transformed parameters{
    
    // to track
    vector[I] ei;       // unit random effects 
    matrix[I,R] eir;    // sub-units random effects
    vector[K] log_ek;   // judges random effects
    
    vector[I] Ti;       // units' latent variable
    matrix[I,R] Tir;    // sub-units' latent variable
    vector[K] Gk;       // judges' latent variable
    
    
    // units' latent variable
    if( equal_ei_s==1 ){
      ei = z_ei;
    } else{
      ei = ei_s[ Xi_d ] .* z_ei;
    }
    Ti = bXc*Xi_c + bXd[ Xi_d ] + ei;
    
    
    // sub-units' latent variable
    if( equal_eir_s==1 ){
      eir = eir_s1*z_eir;
      for( ri in 1:(I*R) ){   // cXd variabilities
        Tir[ iidm_[ri], ridm[ri] ] = Ti[ iidm_[ri] ] + bYc*Yr_c[ri] + bYd[ Yr_d[ri] ] + eir[ iidm_[ri], ridm[ri] ];
      }
    } else{
      for( ri in 1:(I*R) ){   // cXd variabilities
        eir[ iidm_[ri], ridm[ri] ] = eir_s2[ Yr_d[ri] ] * z_eir[ iidm_[ri], ridm[ri] ]; 
        Tir[ iidm_[ri], ridm[ri] ] = Ti[ iidm_[ri] ] + bYc*Yr_c[ri] + bYd[ Yr_d[ri] ] + eir[ iidm_[ri], ridm[ri] ];
      }
    }
    
    
    // judges' latent variable
    if( equal_eir_s==1 ){
      log_ek = z_log_ek;
    } else{
      log_ek = log_ek_s[ Zk_d ] .* z_log_ek;  // cZd variabilities
    }
    Gk = exp( log_bZc*Zk_c + log_bZd[ Zk_d ] + log_ek );

}
model{
    
    // no track
    vector[N] Dkijr;    // perception of difference
    
    
    // (Hyper)Priors
    bXc ~ normal( 0, 0.2 );
    bXd ~ normal( 0, 0.2 );
    
    bYc ~ normal( 0, 0.2 );
    bYd ~ normal( 0, 0.2 );
    
    log_bZc ~ normal( 0, 0.2 );
    log_bZd ~ normal( 0, 0.2 );
    
    z_ei ~ std_normal();
    to_vector( z_eir ) ~ std_normal();
    z_log_ek ~ std_normal();
    
    // for multiple groups
    ei_s ~ dirichlet( rep_vector(5, cXd) );
    eir_s1 ~ exponential(3);
    eir_s2 ~ dirichlet( rep_vector(5, cYd) );
    log_ek_s ~ dirichlet( rep_vector(5, cZd) );


    // likelihood
    for( n in 1:N ){
      Dkijr[n] = Gk[ kidm_[n] ]*( Tir[ iidm_1[n], ridm_1[n] ] - Tir[ iidm_2[n], ridm_2[n] ] );
    }
    Okijr ~ bernoulli_logit( Dkijr );
    
}

