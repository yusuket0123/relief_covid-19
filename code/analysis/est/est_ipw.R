#"""
#irobustness chek
#"""

library(magrittr)


### construct.Rからデータ読み込み

path_merge = file.path("code", "cleaning", "merge.R")
source(path_merge)


estimate_error = function(data, comid = "yes", outcome, covariates){
  if(comid == "yes"){
    formula = paste(outcome, paste(paste(covariates, collapse = " + "), "comid", sep = " + "), sep = " ~ ")
  } else {
    formula = paste(outcome, paste(covariates, collapse = " + "), sep = " ~ ")
  }
  est = lfe::felm(as.formula(formula) , data = data)
  summary = list(formula = formula, estimate = broom::tidy(est), glance = broom::glance(est), augment = broom::augment(est))
  return(summary)
}



  
df_use = dataset_list$dataset_2018_all %>% 
  dplyr::left_join(., dataset_list$dataset_2015_all[c("hhid", "trnsfr_any_dummy")], by = "hhid") %>%
  dplyr::rename(hist_aid = trnsfr_any_dummy,y)

var_list = c( "elite_lc_gov", "elite_gov", "elite_con",
              "lpercapcons", "lhhsize","hhhead_female", "hhhead_age", "hh_head_literacy",
              "hh_head_educ_pri", "hh_head_educ_sec", "hh_head_educ_sec", "hh_head_educ_high", "hh_head_educ_vocation", 
              "hh_head_educ_col","hh_work_employee", "hh_work_farm", "hh_work_business"
                                ) # "nd_drought","nd_flood","nd_pest","nd_livestock","nd_epi"サンプルが落ちる

formula_weight = (paste("hist_aid", paste(var_list, collapse = "+"), sep = "~"))
res.weighting = glm(formula_weight, data = df_use, family = binomial("probit"))
df_use["ps_hist_aid"] = predict(res.weighting, type = "response")
df_use %<>% dplyr::mutate(
  hist_aid_ipw = dplyr::case_when(hist_aid == 0 ~ 1/(1-ps_hist_aid), hist_aid == 1~1/ps_hist_aid, TRUE ~ NA_real_)
)

list_est_error = list()

var_list =  unlist(append(var_list, "hist_aid_ipw"))
outcome = list("trnsfr_any_dummy", "trnsfr_food_dummy", "trnsfr_cash_dummy",
               "in_error_popbnfcry", "ex_error_popbnfcry",
               "in_error_popbnfcry_imp", "ex_error_popbnfcry_imp",
               "in_error_povline", "ex_error_povline"
)

for (i in outcome) {
  name = paste0(i, gsub("var_step_+", "s", j) ,"NoId", gsub("20+", "", y))
  summary = estimate_error(df_use, comid = "no", outcome = i, covariates = var_list[[j]])
  list_est_ipw[[name]] = summary
  print(paste0("done:", name))
  
  name = paste0(i, gsub("var_step_+", "s", j), "Id", gsub("20+", "", y))
  summary = estimate_error(df_use, comid = "yes", outcome = i, covariates = var_list[[j]])
  list_est_error[[name]] = summary
  print(paste0("done:", name))
}
  


