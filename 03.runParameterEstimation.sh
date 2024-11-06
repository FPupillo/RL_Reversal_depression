
models="RW_pav_alpha RW_pav_alpha_rho RW_pav_alpha_rho_gainloss RW_pav_alpha_gainloss RW_pav_alpha_gainloss_rho RW_pav_alpha_gainloss_rho_gainloss"

#models="RescorlaWagner_simple_min1_rs"

for model in $models

do

echo $model

Rscript parameter_estimation.R "$model"

wait

done