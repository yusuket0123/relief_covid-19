#"""
#pmt推計後のmerge
#"""

### pmtの推計

### 変数のまーじ

library(magrittr)

### construct.Rからデータ読み込み

path_merge = file.path("code", "merge.R")
source(path_merge)


### pmtの推計
outcome = c("lpercapcons")

var_step_1 = c( "wall", "roof","floor","electricity","lrooms","pipewater")
var_step_2 = unlist(append(list(var_step_1, "lhhsize"), setdiff(colnames(dataset_list$dataset_2015_all %>% dplyr::select(., starts_with("hh"))), c("hhid", "hhhead", "hhhead_muslim", "hhsize"))))
var_step_3 = unlist(append(list(var_step_2,"houseowner"), setdiff(colnames(dataset_list$dataset_2015_all %>% dplyr::select(., starts_with("dummy_asset"))), c("dummy_asset_cd_332", "dummy_asset_cd_3342",'dummy_asset_cd_3343', "dummy_asset_cd_3344", "dummy_asset_cd_3345" , "dummy_asset_cd_3346", "dummy_asset_cd_3347", "dummy_asset_cd_3348"))))
var_step_4 = unlist(append(var_step_3, setdiff(colnames(dataset_list$dataset_2015_all %>% dplyr::select(., starts_with("dummy_animal"))), c("dummy_animal_cd_119", "dummy_animal_cd_121"))))

var_list = list(var_step_1 = var_step_1, var_step_2 = var_step_2, var_step_3 = var_step_3, var_step_4 = var_step_4)

estimate_pmt = function(data, comid = "yes", outcome, covariates, method = "lm"){
  if(comid == "yes"){
    formula = paste(outcome, paste(paste(covariates, collapse = " + "), "comid", sep = " + "), sep = " ~ ")
  } else if(comid == "no"){
    formula = paste(outcome, paste(covariates, collapse = " + "), sep = " ~ ")
  } else {
    print("specify yes or no as augment of comid")
  }
  
  if(method == "lm"){
    est = lfe::felm(as.formula(formula) , data = data)
    summary = list(formula = formula, estimate = broom::tidy(est), glance = broom::glance(est), augment = broom::augment(est))
  } else if (method == "qr") {
    est = quantreg::rq(as.formula(formula), data = data, tau = 0.5)
    summary = list(formula = formula, estimate = broom::tidy(est, se.type = "iid"), glance = broom::glance(est, se.type = "iid"), augment = broom::augment(est, se.type = "iid"))
  } else {
    print("specify qr (quantile regression) or lm (linear regression) as augment of method")
  }
  return(summary)
}

list_est_pmt = list()
for (i in outcome) {
  for (j in names(var_list)) {
    name = paste("lm",i, gsub("var_step_+", "s", j), "Id", sep = "_")
    summary = estimate_pmt(dataset_list$dataset_2015_all, comid = "yes", outcome = i, covariates = var_list[[j]], method = "lm")
    list_est_pmt[[name]] = summary
    print(paste0("done:", name))
    
    name = paste("lm", i, gsub("var_step_+", "s", j) ,"NoId", sep = "_")
    summary = estimate_pmt(dataset_list$dataset_2015_all, comid = "no", outcome = i, covariates = var_list[[j]], method = "lm")
    list_est_pmt[[name]] = summary
    print(paste0("done:", name))
  }
}

## 分位点回帰（試行錯誤）
summary = estimate_pmt(dataset_list$dataset_2015_all, comid = "no", outcome = "lpercapcons", covariates = var_list$var_step_3, method = "qr")
#summary$glance
var = unlist(stringi::stri_split_regex(summary$formula, pattern = "(.\\+.|.\\~.)"))
var[1] = "1"
pred = 0
for (num in 1:length(var)) {
  if(num == 1){
    elem = summary$estimate$estimate[num] * 1
  } else {
    elem = summary$estimate$estimate[num] * dataset_list$dataset_2015_all[var[num]]
  }
  pred = pred + elem
}

## Create a blank workbook
wb <- openxlsx::createWorkbook()
for (i in names(list_est_pmt)) {
  sheet = i
  openxlsx::addWorksheet(wb, paste(sheet,"est", sep = "_"))
  openxlsx::addWorksheet(wb, paste(sheet,"glc", sep = "_"))
  openxlsx::writeData(wb, sheet = paste(sheet,"est", sep = "_"), x = list_est_pmt[[i]]$estimate)
  openxlsx::writeData(wb, sheet = paste(sheet,"glc", sep = "_"), x = list_est_pmt[[i]]$glance)
}
## Save workbook to working directory
openxlsx::saveWorkbook(wb, file = "est_pmt.xlsx", overwrite = TRUE)
