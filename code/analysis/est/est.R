#"""
#推計: 推計とその表
#"""

library(magrittr)


### construct.Rからデータ読み込み

path_merge = file.path("code", "merge.R")
source(path_merge)

year = list("2018", "2020")

outcome = c("trnsfr_any_dummy", "trnsfr_food_dummy", "trnsfr_cash_dummy",
               "in_error_popbnfcry", "ex_error_popbnfcry",
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
var_share_inter = c("elite_lc_gov*share", "elite_gov*share", "elite_con*share")
var_popbnfcry_inter = c("elite_lc_gov*in_error_popbnfcry", "elite_gov*in_error_popbnfcry", "elite_con*in_error_popbnfcry") # crowding outの推計
var_povline_inter = c("elite_lc_gov*in_error_povline", "elite_gov*in_error_povline", "elite_con*in_error_povline") # crowding outの推計

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
## Create a blank workbook
output.excelsheets = function(list_est, file_name){
  wb <- openxlsx::createWorkbook()
  
  for (i in names(list_est)[!stringi::stri_detect_regex(names(list_est), ".*imp.*")]) {
    sheet = i
    openxlsx::addWorksheet(wb, paste0(sheet,"e"))
    openxlsx::addWorksheet(wb, paste0(sheet,"g"))
    openxlsx::writeData(wb, sheet = paste0(sheet,"e"), x = list_est[[i]]$estimate)
    openxlsx::writeData(wb, sheet = paste0(sheet,"g"), x = list_est[[i]]$glance)
  }
  ## Save workbook to working directory
  openxlsx::saveWorkbook(wb, file = paste0(file_name, ".xlsx"), overwrite = TRUE)
}

# additional estimations
list_add = list(
  C = var_cap_inter,
  E = var_elite_community,
  A = "atr",
  P = var_lpcons_inter,
  Q = var_com_lpcons_inter,
  H = var_hist_aid,
  S = var_share_inter,
  PB = var_popbnfcry_inter,
  PL = var_povline_inter
)

for (y in year) {
  list_est_error = list()
  list_est_error_inter = list()
  name_df_current = paste("dataset", y, "all", sep = "_")
  var_list = eval(parse(text = paste("var_list", y, sep = "_")))
  
  # making variables' list in each equation
  for (initial in names(list_add)) {
    for (num in 1:3) {
      name = paste0("var_step_", num, initial)
      list = unlist(append(list_add[[initial]], var_list[[paste0("var_step_", num)]]))
      assign(name, list)
    }
  }
  
  # 下準備
  if(y == "2018"){
    var_list_simple = var_list_2018
    var_list_inter = var_list_2018
  } else if (y == "2020") {
    var_list_simple = var_list_2020
    var_list_inter = var_list_2020
  }
  df_use = dataset_list[[name_df_current]]
  
  # var_listに追加的分析の共変量リストを格納
  num = c(1,2,3)
  for (j in num) {
    var_list_simple[[paste0("var_step_", j, "E")]] = eval(parse(text = paste0("var_step_", j, "E")))
  }
  
  
  alpha_inter = c("C", "P", "Q", "H", "PB", "PL")
  for(i in alpha_inter){
    for (j in num) {
      var_list_inter[[paste0("var_step_", j, i)]] = eval(parse(text = paste0("var_step_", j, i)))
    }
  }

  if(y == "2018"){ # 2018のみのロバストネスのための対応
    for (j in num) {
      var_list_simple[[paste0("var_step_", j, "A")]] = eval(parse(text = paste0("var_step_", j, "A")))
    }
    # dealing with attrition 
    df_use %<>% 
      dplyr::left_join(., dataset_list$dataset_2020_all %>% dplyr::mutate(atr = 0) %>% dplyr::select(c("hhid", "atr")), by = "hhid") %>%
      dplyr::mutate(atr = ifelse(is.na(atr), 1, atr))
    
    # ipw estimation
    formula_weight = paste("hist_aid", paste(var_list_ipw, collapse = "+"), sep = "~")
    res.weighting = glm(formula_weight, data = df_use, family = binomial("probit"))
    df_use[["ps_hist_aid"]] = predict(res.weighting, type = "response", newdata = df_use)
    df_use_ipw = df_use %>%
      dplyr::mutate(
        hist_aid_ipw = dplyr::case_when(hist_aid == 0 ~ 1/(1-ps_hist_aid), hist_aid == 1~1/ps_hist_aid, TRUE ~ NA_real_)
        )  
  }
  
  if(y == "2020"){
    for (j in c(1,2,3)) {
      var_list_inter[[paste0("var_step_", j, "S")]] = eval(parse(text = paste0("var_step_", j, "S")))
    }
  }
  
  
  # regression
  for (i in outcome) {
    for (j in names(var_list_simple)) {
      name = paste0(i, gsub("var_step_+", "s", j) ,"NId", gsub("20+", "", y))
      summary = estimate_error(df_use, comid = "no", outcome = i, covariates = var_list_simple[[j]])
      list_est_error[[name]] = summary
      print(paste0("done:", name))

      name = paste0(i, gsub("var_step_+", "s", j), "Id", gsub("20+", "", y))
      summary = estimate_error(df_use, comid = "yes", outcome = i, covariates = var_list_simple[[j]])
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
  name_file = paste("est_error_simple",y,sep = "_")
  assign(name_file, list_est_error)
  output.excelsheets(list_est_error, name_file)
  
  # reggression including interaction terms
  for (i in outcome) {
    # regress elite * in_error only when Y = ex_error
    if (i != "ex_error_popbnfcry" | i != "ex_error_povline") {
      var_list_reg = var_list_inter[!stringi::stri_detect_regex(names(var_list_inter), ".*(PB|PL)")]
    } else {
      var_list_reg = var_list_inter
    }
    # regress each linear prob. model
    for (j in names(var_list_reg)) {
      name = paste0(i, gsub("var_step_+", "s", j) ,"NId", gsub("20+", "", y))
      summary = estimate_error(df_use, comid = "no", outcome = i, covariates = var_list_reg[[j]])
      list_est_error_inter[[name]] = summary
      print(paste0("done:", name))
      
      name = paste0(i, gsub("var_step_+", "s", j), "Id", gsub("20+", "", y))
      summary = estimate_error(df_use, comid = "yes", outcome = i, covariates = var_list_reg[[j]])
      list_est_error_inter[[name]] = summary
      print(paste0("done:", name))
    }
  }
  name_file = paste("est_error_inter",y,sep = "_")
  assign(name_file, list_est_error_inter)
  output.excelsheets(list_est_error_inter, name_file)
}
#mean(dataset_list$dataset_2020_all$in_error_povline, na.rm = TRUE)


### informal sharingの推計
year = c("2015", "2020")
list_est_error = list()

for (y in year) {

  if (y == "2015") {
    var_list = purrr::map(var_list_2018, ~ .x[!stringi::stri_detect_regex(.x, "hist_aid")])
    df_use = dataset_list$dataset_2015_all 
    }
  else if (y == "2020") {
    var_list = var_list_2020
    df_use = dataset_list$dataset_2020_all
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

output.excelsheets(list_est_error, "est_share")


### 2015年の推計(大統領選挙)
outcome_2015 = c("trnsfr_any_dummy",
                 "in_error_popbnfcry", "ex_error_popbnfcry",
                 "in_error_povline", "ex_error_povline"
)
list_est_error_2015 = list()
var_step_3_2015 = var_step_3_2018[!stringi::stri_detect_fixed(var_step_3_2018, "hist_aid")]
for (i in outcome_2015) {
  name = paste0(i, "3", "Id", "15")
  summary = estimate_error(data = dataset_list$dataset_2015_all, comid = "yes", outcome = i, covariates = var_step_3_2015)
  list_est_error_2015[[name]] = summary
  print(name)
  name = paste0(i, "3", "NId", "15")
  summary = estimate_error(data = dataset_list$dataset_2015_all, comid = "no", outcome = i, covariates = var_step_3_2015)
  list_est_error_2015[[name]] = summary
  print(name)
}
output.excelsheets(list_est = list_est_error_2015, file_name = "est_error_2015")

debug = function(x){
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
  #share
  var_step_1S = unlist(append(var_share_inter, var_list$var_step_1))
  var_step_2S = unlist(append(var_share_inter, var_list$var_step_2))
  var_step_3S = unlist(append(var_share_inter, var_list$var_step_3))
  
  if (i != ex_error_popbnfcry & (!stringi::stri_detect_regex("var_step_3PL", ".*(PB|PL)"))) {
    name = paste0(i, gsub("var_step_+", "s", j) ,"NId", gsub("20+", "", y))
    summary = estimate_error(df_use, comid = "no", outcome = i, covariates = var_list_inter[[j]])
    list_est_error_inter[[name]] = summary
    print(paste0("done:", name))
    name = paste0(i, gsub("var_step_+", "s", j), "Id", gsub("20+", "", y))
    print(paste0("done:", name))
  }
  
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
}
