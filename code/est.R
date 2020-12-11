#"""
#推計: 推計とその表
#"""

library(magrittr)


### construct.Rからデータ読み込み

path_merge = file.path("code", "merge.R")
source(path_merge)

dataset_list$dataset_2015_all %<>% dplyr::mutate(comid = stringr::str_sub(hhid, start = 1, end = -7))
dataset_list$dataset_2018_all %<>% dplyr::mutate(comid = stringr::str_sub(hhid, start = 1, end = -7))
View(psych::describe(dataset_list$dataset_2018_all))

year = list("2018", "2020")

outcome = list("trnsfr_any_dummy", "trnsfr_food_dummy", "trnsfr_cash_dummy",
               "in_error_popbnfcry", "ex_error_popbnfcry",
               "in_error_popbnfcry_imp", "ex_error_popbnfcry_imp",
               "in_error_povline", "ex_error_povline"
               )

var_step_1 = c( "elite_lc_gov", "elite_gov", "elite_con")
var_step_2_2018 = unlist(append(var_step_1,
                           c("hist_aid" ,"lpercapcons", "lhhsize","hhhead_female", "hhhead_age", "hh_head_literacy",
                           "hh_head_educ_pri", "hh_head_educ_sec", "hh_head_educ_sec", "hh_head_educ_high", "hh_head_educ_vocation", "hh_head_educ_col",
                           "hh_work_employee", "hh_work_farm", "hh_work_business"
                           ) # "nd_drought","nd_flood","nd_pest","nd_livestock","nd_epi"サンプルが落ちる
                           ))
var_step_2_2020 = unlist(append(var_step_1,
                                c("hist_aid", "lpercapcons", "lhhsize","hhhead_female", "hhhead_age", "hh_head_literacy",
                                  "hh_head_educ_pri", "hh_head_educ_sec", "hh_head_educ_sec", "hh_head_educ_high", "hh_head_educ_vocation", "hh_head_educ_col",
                                  "hh_work_employee", "hh_work_farm", "hh_work_business", 
                                  "shock_sickness","shock_jobloss","shock_business","shock_theft","shock_agri","shock_input","shock_output","shock_inf")
                                ))


var_list_2018 = list(var_step_1 = var_step_1, var_step_2 = var_step_2_2018)
var_list_2020 = list(var_step_1 = var_step_1, var_step_2 = var_step_2_2020)

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

list_est_error = list()
for (y in year) {
  name_df_current = paste("dataset", y, "all", sep = "_")
  if(y == "2018"){
    var_list = var_list_2018
    name_df_past = paste("dataset", "2015", "all", sep = "_")
  } else if (y == "2020") {
    var_list = var_list_2020
    name_df_past = paste("dataset", "2018", "all", sep = "_")
  }
  
  df_use = dataset_list[[name_df_current]] %>% 
    dplyr::left_join(., 
                     dataset_list[[name_df_past]] %>% 
                       dplyr::select(c("hhid", "trnsfr_any_dummy")) %>%
                       dplyr::rename(hist_aid = trnsfr_any_dummy),
                     by = "hhid"
                     )
  for (i in outcome) {
    for (j in names(var_list)) {
      name = paste0(i, gsub("var_step_+", "s", j) ,"NoId", gsub("20+", "", y))
      summary = estimate_error(df_use, comid = "no", outcome = i, covariates = var_list[[j]])
      list_est_error[[name]] = summary
      print(paste0("done:", name))
      
      name = paste0(i, gsub("var_step_+", "s", j), "Id", gsub("20+", "", y))
      summary = estimate_error(df_use, comid = "yes", outcome = i, covariates = var_list[[j]])
      list_est_error[[name]] = summary
      print(paste0("done:", name))
    }
  }
  
}




## Create a blank workbook
wb <- openxlsx::createWorkbook()
names(list_est_error)
for (i in names(list_est_error)) {
  sheet = i
  openxlsx::addWorksheet(wb, paste0(sheet,"e"))
  openxlsx::addWorksheet(wb, paste0(sheet,"g"))
  openxlsx::writeData(wb, sheet = paste0(sheet,"e"), x = list_est_error[[i]]$estimate)
  openxlsx::writeData(wb, sheet = paste0(sheet,"g"), x = list_est_error[[i]]$glance)
}
## Save workbook to working directory
openxlsx::saveWorkbook(wb, file = "est_eror.xlsx", overwrite = TRUE)

