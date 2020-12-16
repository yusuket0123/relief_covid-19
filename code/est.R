#"""
#推計: 推計とその表
#"""

library(magrittr)


### construct.Rからデータ読み込み

path_merge = file.path("code", "merge.R")
source(path_merge)



year = list("2018", "2020")

outcome = list("trnsfr_any_dummy", "trnsfr_food_dummy", "trnsfr_cash_dummy",
               "in_error_popbnfcry", "ex_error_popbnfcry",
               "in_error_popbnfcry_imp", "ex_error_popbnfcry_imp",
               "in_error_povline", "ex_error_povline"
               )

var_step_1 = c( "elite_lc_gov", "elite_gov", "elite_con")
var_step_2_only = c("hist_aid", "lpercapcons", "lhhsize","hhhead_female", "hhhead_age", "hh_head_literacy",
                    "hh_head_educ_pri", "hh_head_educ_sec", "hh_head_educ_high", 
                    "hh_head_educ_vocation", "hh_head_educ_col","hh_work_employee", "hh_work_farm", "hh_work_business", "capital"
)
var_step_2 = unlist(append(var_step_1,var_step_2_only))

var_step_3_only_2018 = c("nd_drought","nd_flood","nd_pest","nd_livestock","nd_epi") # サンプルが落ちる
var_step_3_2018 = unlist(append(var_step_2,var_step_3_only_2018))



var_step_3_only_2020 = c("shock_sickness","shock_jobloss","shock_business","shock_theft","shock_agri","shock_input",
                         "shock_output","shock_inf")
var_step_3_2020 = unlist(append(var_step_2,var_step_3_only_2020))

var_list_2018 = list(var_step_1 = var_step_1, var_step_2 = var_step_2, var_step_3 = var_step_3_2018)
var_list_2020 = list(var_step_1 = var_step_1, var_step_2 = var_step_2, var_step_3 = var_step_3_2020)

var_cap_inter = c("elite_lc_gov*capital", "elite_gov*capital", "elite_con*capital")
var_lpcons_inter = c("elite_lc_gov*lpercapcons", "elite_gov*lpercapcons", "elite_con*lpercapcons")
var_com_lpcons_inter = c("elite_lc_gov*com_lpercapcons", "elite_gov*com_lpercapcons", "elite_con*com_lpercapcons")
var_elite_community = c("com_elite_lc_gov", "com_elite_gov", "com_elite_con")
var_hist_aid = c("elite_lc_gov*hist_aid", "elite_gov*hist_aid", "elite_con*hist_aid")

var_list_ipw = c(
  "lpercapcons", "lhhsize","hhhead_female", "hhhead_age", "hh_head_literacy",
  "hh_head_educ_pri", "hh_head_educ_sec", "hh_head_educ_sec", "hh_head_educ_high", "hh_head_educ_vocation", 
  "hh_head_educ_col","hh_work_employee", "hh_work_farm", "hh_work_business"
)
  
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
  var_list = eval(parse(text = paste("var_list", y, sep = "_")))
  
  # capital dummy interaction termsを入れるモデル
  var_step_1C = unlist(append(var_cap_inter, var_list$var_step_1))
  var_step_2C = unlist(append(var_cap_inter, var_list$var_step_2))
  var_step_3C = unlist(append(var_cap_inter, var_list$var_step_3))
  # Elite_community levelを入れるモデル
  var_step_1E = unlist(append(var_elite_community, var_list$var_step_1))
  var_step_2E = unlist(append(var_elite_community, var_list$var_step_2))
  var_step_3E = unlist(list(var_elite_community, var_list$var_step_3))
  # 2020attrition dummyを入れるモデル
  var_step_1A = unlist(append("atr", var_list$var_step_1))
  var_step_2A = unlist(append("atr", var_list$var_step_2))
  var_step_3A = unlist(append("atr", var_list$var_step_3))
  # lpercapconsのinteraction termsを入れるモデル
  var_step_1P = unlist(append(var_lpcons_inter, var_list$var_step_1))
  var_step_2P = unlist(append(var_lpcons_inter, var_list$var_step_2))
  var_step_3P = unlist(append(var_lpcons_inter, var_list$var_step_3))
  # community levelのlpercapconsのinteraction termsを入れるモデル
  var_step_1Q = unlist(append(var_com_lpcons_inter, var_list$var_step_1))
  var_step_2Q = unlist(append(var_com_lpcons_inter, var_list$var_step_2))
  var_step_3Q = unlist(append(var_com_lpcons_inter, var_list$var_step_3))
  # hist_aidの異質性
  var_step_1H = unlist(append(var_hist_aid, var_list$var_step_1))
  var_step_2H = unlist(append(var_hist_aid, var_list$var_step_2))
  var_step_3H = unlist(append(var_hist_aid, var_list$var_step_3))
  
  
  
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
  
  # var_listに追加的分析の共変量リストを格納
  for(i in c("C", "E", "P", "Q", "H")){
    for (j in c(1,2,3)) {
      var_list[paste0("var_step_", j, i)] = eval(parse(text = paste0("var_step_", j, i)))
    }
  }
  
  if(y == "2018"){ # 2018のみのロバストネスのための対応
    # dealing with attrition 
    df_use %<>% 
      dplyr::left_join(., dataset_list$dataset_2020_all %>% dplyr::mutate(atr = 0) %>% dplyr::select(c("hhid", "atr")), by = "hhid") %>%
      dplyr::mutate(atr = ifelse(is.na(atr), 1, atr))
    for (j in c(1,2,3)) {
      var_list[paste0("var_step_", j, "A")] = eval(parse(text = paste0("var_step_", j, "A")))
    }
    
    # ipw estimation
    formula_weight = paste("hist_aid", paste(var_list_ipw, collapse = "+"), sep = "~")
    res.weighting = glm(formula_weight, data = df_use, family = binomial("probit"))
    df_use["ps_hist_aid"] = predict(res.weighting, type = "response", newdata = df_use)
    df_use_ipw = df_use %>%
      dplyr::mutate(
        hist_aid_ipw = dplyr::case_when(hist_aid == 0 ~ 1/(1-ps_hist_aid), hist_aid == 1~1/ps_hist_aid, TRUE ~ NA_real_)
        )  
  }
  
  # regression
  for (i in outcome) {
    for (j in names(var_list)) {
      name = paste0(i, gsub("var_step_+", "s", j) ,"NId", gsub("20+", "", y))
      summary = estimate_error(df_use, comid = "no", outcome = i, covariates = var_list[[j]])
      list_est_error[[name]] = summary
      print(paste0("done:", name))

      name = paste0(i, gsub("var_step_+", "s", j), "Id", gsub("20+", "", y))
      summary = estimate_error(df_use, comid = "yes", outcome = i, covariates = var_list[[j]])
      list_est_error[[name]] = summary
      print(paste0("done:", name))
    }
    if (year == "2018") {
      name = paste0(i, "s2", "NId", "ipw")
      summary = estimate_error(df_use_ipw, comid = "no", outcome = i, covariates = var_list_2018$var_step_2)
      list_est_error[[name]] = summary
      print(paste0("done:", name))
      
      name = paste0(i, "s2", "Id","ipw")
      summary = estimate_error(df_use_ipw, comid = "yes", outcome = i, covariates = var_list_2018$var_step_2)
      list_est_error[[name]] = summary
      print(paste0("done:", name))
    }
  }
}

### informal sharingの推計
year = c("2015", "2020")
for (y in year) {
  
  if (y == "2015") {
    var_list = purrr::map(var_list_2018, ~ .x[!stringi::stri_detect_regex(.x, "hist_aid")])
    df_use = dataset_list$dataset_2015_all 
    }
  else if (y == "2020") {
    var_list = var_list_2020
    df_use = dataset_list$dataset_2020_all %>% 
      dplyr::left_join(., 
                       dataset_list$dataset_2018_all %>% 
                         dplyr::select(c("hhid", "trnsfr_any_dummy")) %>%
                         dplyr::rename(hist_aid = trnsfr_any_dummy),
                       by = "hhid"
      )
    }
  
  for (j in names(var_list)) {
    name = paste0("share", gsub("var_step_+", "s", j) ,"NId", gsub("20+", "", y))
    summary = estimate_error(df_use, comid = "no", outcome = "share", covariates = var_list[[j]])
    list_est_error[[name]] = summary
    print(paste0("done:", name))
    
    name = paste0("share", gsub("var_step_+", "s", j), "Id", gsub("20+", "", y))
    summary = estimate_error(df_use, comid = "yes", outcome = "share", covariates = var_list[[j]])
    list_est_error[[name]] = summary
    print(paste0("done:", name))
  }
}


## Create a blank workbook
wb <- openxlsx::createWorkbook()

for (i in names(list_est_error)[!stringi::stri_detect_regex(names(list_est_error), ".*imp.*")]) {
  sheet = i
  openxlsx::addWorksheet(wb, paste0(sheet,"e"))
  openxlsx::addWorksheet(wb, paste0(sheet,"g"))
  openxlsx::writeData(wb, sheet = paste0(sheet,"e"), x = list_est_error[[i]]$estimate)
  openxlsx::writeData(wb, sheet = paste0(sheet,"g"), x = list_est_error[[i]]$glance)
}
## Save workbook to working directory
openxlsx::saveWorkbook(wb, file = "est_eeror.xlsx", overwrite = TRUE)
