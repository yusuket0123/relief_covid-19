#"""
#estimation_community level
#"""

# var_list
outcome_list = list(
  outcome_2015 = list("trnsfr_any_dummy","in_error_popbnfcry", "ex_error_popbnfcry",
                      "in_error_popbnfcry_imp", "ex_error_popbnfcry_imp","in_error_povline", "ex_error_povline"
                      ),
  outcome_2018 = list("trnsfr_any_dummy", "trnsfr_food_dummy", "trnsfr_cash_dummy",
                      "in_error_popbnfcry", "ex_error_popbnfcry","in_error_popbnfcry_imp", "ex_error_popbnfcry_imp",
                      "in_error_povline", "ex_error_povline"
                      ),
  outcome_2020 = list("trnsfr_any_dummy", "trnsfr_food_dummy", "trnsfr_cash_dummy",
                      "in_error_popbnfcry", "ex_error_popbnfcry","in_error_popbnfcry_imp", "ex_error_popbnfcry_imp",
                      "in_error_povline", "ex_error_povline"
                      )
  )


var_step_1 = c( "elite_lc_gov", "elite_gov", "elite_con")
var_step_2_only = c("hist_aid", "lpercapcons", "lhhsize","hhhead_female", "hhhead_age", "hh_head_literacy",
                    "hh_head_educ_pri", "hh_head_educ_sec", "hh_head_educ_high", 
                    "hh_head_educ_vocation", "hh_head_educ_col", "hh_work_employee", "hh_work_farm", "hh_work_business"
)
var_step_2 = unlist(append(var_step_1,var_step_2_only))
var_step_3_only_2018 = c("nd_drought","nd_flood","nd_pest","nd_livestock","nd_epi") # サンプルが落ちる
var_step_3_2018 = unlist(append(var_step_2,var_step_3_only_2018))
var_step_3_only_2020 = c("shock_sickness","shock_jobloss","shock_business","shock_theft","shock_agri","shock_input",
                         "shock_output","shock_inf")
var_step_3_2020 = unlist(append(var_step_2,var_step_3_only_2020))

var_list = list(
  var_list_2015 = list(var_step_1 = var_step_1, var_step_2 = var_step_2, var_step_3 = var_step_3_2018),
  var_list_2018 = list(var_step_1 = var_step_1, var_step_2 = var_step_2, var_step_3 = var_step_3_2018),
  var_list_2020 = list(var_step_1 = var_step_1, var_step_2 = var_step_2, var_step_3 = var_step_3_2020)
  )

year = c("2015","2018", "2020")

make_df_community_level = function(data, year){
  df_use = data
  var_list = var_list[[paste("var_list", 2018, sep = "_")]]
  var_list_convert = unlist(append(var_list$var_step_3, outcome)) %>% .[!stringi::stri_detect_regex(., "hist_aid")]

  cols_convert_from = unlist(var_list$var_step_3, outcome_15) %>% .[!stringi::stri_detect_regex(., "hist_aid")]
  cols_id = c("hhid", "comid")
  cols_df_use = unlist(append(cols_id, cols_convert_from))
  
  df_use_community_level =
    df_use %>%
    dplyr::select(cols_df_use) %>%
    dplyr::group_by(comid) %>%
    dplyr::group_split(., .keep = TRUE)
  
  df_use_community_level =  purrr::map(df_use_community_level,
               ~ .x %<>% dplyr::mutate_at(cols_convert_from, ~mean(., na.rm = TRUE)) %>% dplyr::distinct(comid, .keep_all = TRUE))
  df_use_community_level = dplyr::bind_rows(df_use_community_level)
    return(df_use_community_level)
}





