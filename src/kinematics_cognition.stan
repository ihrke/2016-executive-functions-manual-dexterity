// include all the repeated measures, once again but contrained by hierarchical t-prior
//
//
data{
  int<lower=0> n; // total number of data points in kinematics
  int<lower=0> nsubj; // number of subjects
  int<lower=0> ntasks; // number of tasks
  int<lower=0> nact;  // number of actions
  int<lower=0> nobjects; // number of objects
  int<lower=0,upper=nsubj> subj[n]; // indicates which subj produced each datapoint
  int<lower=0,upper=nact> action[n]; // indicates which action produced each datapoint
  int<lower=0,upper=nobjects> object[n]; // which object?
  int<lower=0,upper=ntasks> task[n]; // which task
  int<lower=0,upper=1> group[nsubj]; // which group is each subject in?
  int<lower=0> nkin; // number of kinematics variables
  vector[nkin] kinematics[n]; // the nkin kinematics variables for each data point
  vector[nsubj] tallforw;    // data for "Tallspenn_forlengs"
  vector[nsubj] tallbackw;   // data for "Tallspenn_baklengs", 
  vector[nsubj] stroop;      // data for "Stroop_WC"
  vector[nsubj] trail;       // data for "Trail_M_B"
}

parameters{
  cov_matrix[nkin] Sigma;    // kinematics covariance
  
  vector[nkin] bsubj[nsubj]; // subj-level intercept
  vector[nkin] baction_raw[nact-1];// action effect
  vector[nkin] btask_raw[ntasks-1];  // task effect
  vector[nkin] bobject_raw[nobjects-1];  // object effect
  
  real<lower=0> coef_subj_sigma; // std for subj-level model
  //real coef_subj_nu;    // df for that part
  
  vector[nkin] bgroup;       // group-effect
  vector[nkin] btallforw;    // coefficients for "Tallspenn_forlengs"
  vector[nkin] btallbackw;   // coefficients for "Tallspenn_baklengs", 
  vector[nkin] bstroop;      // coefficients for "Stroop_WC"
  vector[nkin] btrail;       // coefficients for "Trail_M_B"
  
  // interaction coefficients with group
  vector[nkin] bgtallforw;    // coefficients for "Tallspenn_forlengs"
  vector[nkin] bgtallbackw;   // coefficients for "Tallspenn_baklengs", 
  vector[nkin] bgstroop;      // coefficients for "Stroop_WC"
  vector[nkin] bgtrail;       // coefficients for "Trail_M_B"
  
  //real coef_nu;    // df parameter for overarching coefficient model
  real mu_subj;
  //real<lower=0> coef_sigma; // std for the coefficient model
  //real<lower=0> coef_cog_sigma; // std for the cognitive variables coefficient model
}

transformed parameters {
  // include baseline reference category
  vector[nkin] baction[nact];// action effect
  vector[nkin] btask[ntasks];  // task effect
  vector[nkin] bobject[nobjects];  // object effect
  for(i in 1:nkin){
    baction[1][i] <- 0.0;
    btask[1][i] <- 0.0;
    bobject[1][i] <- 0.0;
  }
  for( i in 2:nact){
    baction[i] <- baction_raw[i-1];
  }
  for( i in 2:ntasks){
    btask[i]   <- btask_raw[i-1];
  }
  for(i in 2:nobjects){
    bobject[i] <- bobject_raw[i-1];
  }
}

model{
  //real coef_nu;
  real coef_subj_nu;
  
  //coef_nu <- 1.0;
  coef_subj_nu <- 1.0;
  //coef_sigma ~ normal(0, 1); // half-normal because coef_sigma>0
  //coef_cog_sigma ~ normal(0, 1); 
  coef_subj_sigma ~ normal(0, 1);
  mu_subj ~ normal(0,1);
  
  for(i in 1:(nact-1))  {
    baction_raw[i] ~ cauchy(0,1);
  }
  
  for(i in 1:(ntasks-1)){
    btask_raw[i] ~ cauchy(0,1);
  }

  for(i in 1:(nobjects-1)){
    bobject_raw[i] ~ cauchy(0,1);
  }

  // bsubj prior
  for( i in 1:nsubj) {
    bsubj[i] ~ student_t(coef_subj_nu, mu_subj, coef_subj_sigma);
  }  
  
  // COGNITIVE
  bgroup ~ cauchy(0,1);
  
  btallforw ~ cauchy(0,1);
  btallbackw ~ cauchy(0,1);
  bstroop ~ cauchy(0,1);
  btrail ~ cauchy(0,1);

  bgtallforw ~ cauchy(0,1);
  bgtallbackw ~ cauchy(0,1);
  bgstroop ~ cauchy(0,1);
  bgtrail ~ cauchy(0,1);
  
  // LIKELIHOOD
  for( i in 1:n ){
    kinematics[i] ~ multi_normal( bsubj[subj[i]] + baction[action[i]] + 
                                  btask[task[i]] + bobject[object[i]] +
                                  bgroup*group[subj[i]] + btallforw*tallforw[subj[i]] + 
                                  btallbackw*tallbackw[subj[i]] + 
                                  bstroop*stroop[subj[i]] + btrail*trail[subj[i]] + 
                                  bgtallforw*tallforw[subj[i]]*group[subj[i]] +
                                  bgtallbackw*tallbackw[subj[i]]*group[subj[i]] + 
                                  bgstroop*stroop[subj[i]]*group[subj[i]] + 
                                  bgtrail*trail[subj[i]]*group[subj[i]]
                                  , Sigma );
  }  
}

generated quantities {
  //vector[nkin] y_rep[n];
  //for (i in 1:n){
//    y_rep[i] <- multi_normal_rng(bsubj[subj[i]] + baction[action[i]] + 
//                                 btask[task[i]] + bobject[object[i]], Sigma);
//  }
}