#"""
#did
#"""

library(magrittr)

path_merge = file.path("code", "merge.R")
source(path_merge)
path_est_func = file.path("code", "est_func.R")
source(path_est_func)

### 推計関数
run_estimation = function(data, outcome, covariates){
  estimations = list()
  est_did = est$new(data = data)
  for (o in outcome) {
    for (i in names(covariates)) {
      result = est_did$est_did(year = "2020", outcome = o, covariates = covariates[[i]])
      name = paste0(o, gsub("var_step_+", "s", i), gsub("20+", "", "2020"))
      estimations[[name]] = result
      print(name)
    }
  }
  return(estimations)
}

### did at hh level
# model elite_gov*error = Post*Capital


outcomes = c("iner_elite_gov_popbn", "exer_elite_gov_popbn", "iner_elite_gov_povln", "exer_elite_gov_povln")
var_step_1 = c("capital*post")
var_step_2_origin = c("hist_aid", "lpercapcons", "lhhsize","hhhead_female", "hhhead_age", "hh_head_literacy",
                      "hh_head_educ_pri", "hh_head_educ_sec", "hh_head_educ_high", 
                      "hh_head_educ_vocation", "hh_head_educ_col","hh_work_employee", "hh_work_farm", "hh_work_business", "capital"
)
var_step_2 = unlist(c(var_step_1, var_step_2_origin))
var_step_3 = unlist(c(var_step_2, "as.factor(comid)"))
list_covariates = list(var_step_1 = var_step_1, var_step_2 = var_step_2, var_step_3 = var_step_3)

estimations = run_estimation(data = dataset_list_all_year, outcome = outcomes, covariates = list_covariates)
file_did_2020 = output.excelsheets$new(file_name = "est_did_hh")
file_did_2020$output_xlsxfile(list_est = estimations)

### did at community level
outcomes_com = c("com_iner_elite_gov_popbn", "com_exer_elite_gov_popbn", "com_iner_elite_gov_povln", "com_exer_elite_gov_povln")
var_step_1_com = c("com_capital*post")
var_step_2_origin_com = paste0("com_", var_step_2_origin)
var_step_2_com = unlist(c(var_step_1_com, var_step_2_origin_com))
list_covariates_com = list(var_step_1 = var_step_1_com, var_step_2 = var_step_2_com)
estimations_com = run_estimation(data = dataset_community_level_all_year, 
                                      outcome = outcomes_com, 
                                      covariates = list_covariates_com
                                      )
file_did = output.excelsheets$new(file_name = "est_did_com")
file_did$output_xlsxfile(list_est = estimations_com)

