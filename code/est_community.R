#"""
#estimation_community level
#"""

### construct.Rからデータ読み込み

path_merge = file.path("code", "merge.R")
source(path_merge)


  
# var_list
outcome = c("trnsfr_any_dummy", "trnsfr_food_dummy", "trnsfr_cash_dummy",
                      "in_error_popbnfcry", "ex_error_popbnfcry","in_error_popbnfcry_imp", "ex_error_popbnfcry_imp",
                      "in_error_povline", "ex_error_povline"
                      )
outcome = paste("com", outcome, sep = "_")

var_step_1 = paste("com", c( "elite_lc_gov", "elite_gov", "elite_con"), sep = "_")

var_step_2_only = c("hist_aid", "lpercapcons", "lhhsize","hhhead_female", "hhhead_age", "hh_head_literacy",
                    "hh_head_educ_pri", "hh_head_educ_sec", "hh_head_educ_high", 
                    "hh_head_educ_vocation", "hh_head_educ_col", "hh_work_employee", "hh_work_farm", "hh_work_business", "capital"
)
var_step_2_only = paste("com", var_step_2_only, sep = "_")

var_step_2 = unlist(append(var_step_1,var_step_2_only))
var_step_3_only_2018 = paste("com", c("nd_drought","nd_flood","nd_pest","nd_livestock","nd_epi"), sep = "_") # サンプルが落ちる
var_step_3_2018 = unlist(append(var_step_2,var_step_3_only_2018))
var_step_3_only_2020 = paste("com", c("shock_sickness","shock_jobloss","shock_business","shock_theft","shock_agri","shock_input",
                         "shock_output","shock_inf"), sep = "_")

var_step_3_2020 = unlist(append(var_step_2,var_step_3_only_2020))

var_cap_inter = c("com_elite_lc_gov*com_capital", "com_elite_gov*com_capital", "com_elite_con*com_capital")

var_list_ipw = c(
  "lpercapcons", "lhhsize","hhhead_female", "hhhead_age", "hh_head_literacy",
  "hh_head_educ_pri", "hh_head_educ_sec", "hh_head_educ_sec", "hh_head_educ_high", "hh_head_educ_vocation", 
  "hh_head_educ_col","hh_work_employee", "hh_work_farm", "hh_work_business"
)
var_list_ipw = paste("com", var_list_ipw, sep = "_")


var_list_2018 = list(var_step_1 = var_step_1, var_step_2 = var_step_2, var_step_3 = var_step_3_2018)
var_list_2020 = list(var_step_1 = var_step_1, var_step_2 = var_step_2, var_step_3 = var_step_3_2020)




estimate_error = function(data, outcome, covariates){
  formula = paste(outcome, paste(covariates, collapse = " + "), sep = " ~ ")
  est = estimatr::lm_robust(as.formula(formula), clusters = comid, se_type = "stata", data = data)
  summary = list(formula = formula, estimate = broom::tidy(est), glance = broom::glance(est), augment = broom::augment(est))
  return(summary)
}


### community levelの推計

list_est_community_level = list()
year = c("2018", "2020")
for (y in year) {
  name_df_current = paste("df_community_level", y, sep = "_")
  var_list = eval(parse(text = paste("var_list", y, sep = "_")))
  
  # capital dummyこうさこうを入れるモデル
  var_step_2C = unlist(append(var_cap_inter, var_list$var_step_2))
  var_step_3C = unlist(append(var_cap_inter,var_list$var_step_3))
  
  
  if(y == "2018"){
    var_list = var_list_2018
    name_df_past = paste("df_community_level", "2015", sep = "_")
  } else if (y == "2020") {
    var_list = var_list_2020
    name_df_past = paste("df_community_level", "2018", sep = "_")
  }
  
  var_list[["var_step_2C"]] = var_step_2C
  var_list[["var_step_3C"]] = var_step_3C
  
  
  df_use = dataset_community_level[[name_df_current]] %>% 
    dplyr::left_join(., 
                     dataset_community_level[[name_df_past]] %>% 
                       dplyr::select(c("comid", "com_trnsfr_any_dummy")) %>%
                       dplyr::rename(com_hist_aid = com_trnsfr_any_dummy),
                     by = "comid"
    )
  
  if(y == "2018"){ # 2018のみhist_aidの欠損に対処
    formula_weight = paste("com_hist_aid", paste(var_list_ipw, collapse = "+"), sep = "~")
    res.weighting = glm(formula_weight, data = df_use, family = binomial("probit"))
    df_use["com_ps_hist_aid"] = predict(res.weighting, type = "response", newdata = df_use)
    df_use_ipw = df_use %>%
      dplyr::mutate(
        com_hist_aid_ipw = dplyr::case_when(com_hist_aid == 0 ~ 1/(1-com_ps_hist_aid), com_hist_aid == 1~1/com_ps_hist_aid, TRUE ~ NA_real_)
      )  
  }
  
  for (i in outcome) {
    for (j in names(var_list)) {
      name = paste0(gsub("com_+", "", i), gsub("var_step_+", "s", j) ,"NId", gsub("20+", "", y))
      summary = estimate_error(df_use, outcome = i, covariates = var_list[[j]])
      list_est_community_level[[name]] = summary
      print(paste0("done:", name))
    }
    if (year == "2018") {
      name = paste0(gsub("com_+", "", i), "s2", "NId", "ipw")
      summary = estimate_error(df_use_ipw, outcome = i, covariates = var_list_2018$var_step_2)
      list_est_community_level[[name]] = summary
      print(paste0("done:", name))
    }
  }
}

## Create a blank workbook
wb <- openxlsx::createWorkbook()

for (i in names(list_est_community_level)) {
  sheet = i
  openxlsx::addWorksheet(wb, paste0(sheet,"e"))
  openxlsx::addWorksheet(wb, paste0(sheet,"g"))
  openxlsx::writeData(wb, sheet = paste0(sheet,"e"), x = list_est_community_level[[i]]$estimate)
  openxlsx::writeData(wb, sheet = paste0(sheet,"g"), x = list_est_community_level[[i]]$glance)
}
## Save workbook to working directory
openxlsx::saveWorkbook(wb, file = "est_eeror_community_level.xlsx", overwrite = TRUE)



