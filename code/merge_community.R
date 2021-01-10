#"""
#community levelのデータ
#"""

#path_merge = file.path("code", "merge.R")
#source(path_merge)


# var_list
outcome_list = list(
  outcome_2015 = list("trnsfr_any_dummy","in_error_popbnfcry", "ex_error_popbnfcry",
                      "in_error_povline", "ex_error_povline", 
                      "iner_elite_gov_popbn", "exer_elite_gov_popbn", "iner_elite_gov_povln", "exer_elite_gov_povln"
  ),
  outcome_2018 = list("trnsfr_any_dummy", "trnsfr_food_dummy", "trnsfr_cash_dummy",
                      "in_error_popbnfcry", "ex_error_popbnfcry",
                      "in_error_povline", "ex_error_povline",
                      "iner_elite_gov_popbn", "exer_elite_gov_popbn", "iner_elite_gov_povln", "exer_elite_gov_povln"
  ),
  outcome_2020 = list("trnsfr_any_dummy", "trnsfr_food_dummy", "trnsfr_cash_dummy",
                      "in_error_popbnfcry", "ex_error_popbnfcry",
                      "in_error_povline", "ex_error_povline",
                      "iner_elite_gov_popbn", "exer_elite_gov_popbn", "iner_elite_gov_povln", "exer_elite_gov_povln"
  )
)


var_step_1 = c( "elite_lc_gov", "elite_gov", "elite_con")
var_step_2_only = c("hist_aid", "lpercapcons", "lhhsize","hhhead_female", "hhhead_age", "hh_head_literacy",
                    "hh_head_educ_pri", "hh_head_educ_sec", "hh_head_educ_high", 
                    "hh_head_educ_vocation", "hh_head_educ_col", "hh_work_employee", "hh_work_farm", "hh_work_business", "capital"
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

### function
mutate_vars_with_progress = function(data, pb, cols_from){
  pb$tick()$print()
  data %<>% 
    dplyr::mutate_at(cols_from, ~mean(., na.rm = TRUE)) %>% 
    dplyr::distinct(comid, .keep_all = TRUE)
}

make_df_community_level = function(data, year){
  df_use = data 
  outcome = outcome_list[[paste("outcome", year, sep = "_")]] 
  var_list = var_list[[paste("var_list", year, sep = "_")]] 
  
  cols_convert_from = unlist(append(var_list$var_step_3, outcome)) 
  cols_id = c("comid")
  cols_df_use = unlist(append(cols_id, cols_convert_from))
  
  df_use_community_level =
    df_use %>%
    dplyr::select(cols_df_use) %>%
    dplyr::group_by(comid) %>%
    dplyr::group_split(., .keep = TRUE)
  # create the progress bar with a dplyr function. 
  pb = dplyr::progress_estimated(length(df_use_community_level))
  df_use_community_level = 
    purrr::map(df_use_community_level, ~.x %<>% mutate_vars_with_progress(., pb = pb, cols_from = cols_convert_from)) %>% 
    dplyr::bind_rows(.) %>%
    dplyr::rename_at(cols_convert_from, function(x) stringi::stri_paste("com", x, sep = "_"))
  return(df_use_community_level)
}


# データセットリスト
year = c("2015","2018", "2020")
dataset_community_level = list()
for (y in year) {
  print(y)
  df_name = paste("dataset", y, "all", sep = "_")
  data = dataset_list[[df_name]]
  df_community_level = make_df_community_level(data = data, year = y) 
  dataset_community_level[[paste("df_community_level", y,sep = "_")]] = df_community_level
  print("")
}

dataset_community_level_all_year = purrr::map2(dataset_community_level, names(dataset_community_level), ~ dplyr::mutate(.x, year = gsub("df_community_level_", "", .y))) %>% purrr::reduce(., dplyr::bind_rows)
write.csv(dataset_community_level_all_year, file.path("datas", "processed", "dataset_community_level_all_year.csv"))


cols = c("comid", "com_ex_error_povline", "com_capital")
make_df_use_ssid = function(name, cols){
  dataset_community_level[[name]] %>%
    dplyr::select(cols) %>%
    dplyr::filter(!is.na(.$com_ex_error_povline)) %>%
    dplyr::rename_at(dplyr::vars(contains("com_")), 
                     ~paste0(., "_", gsub(pattern="df_community_level_", replacement="", x = name))
                     )
}


write.csv(
  purrr::reduce(
    purrr::map(names(dataset_community_level),
               ~ make_df_use_ssid(name = as.character(.x), cols = cols)
    ), 
    dplyr::inner_join, by = "comid"),  
  file.path("datas", "processed", "df_use_sdid.csv")
)

print("done: merge_community.R")

