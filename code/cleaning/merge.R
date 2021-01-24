#"""
#merge（ファイルまーじ：まーじデータの加工）
#"""


library(magrittr)

### construct.Rからデータ読み込み

path_clean = file.path("code", "cleaning", "clean.R")
source(path_clean)

SkipImputation = function(skip = "no"){
  if(skip == "no"){
    path_imp = file.path("code", "cleaning", "imp.R")
    source(path_imp)
    } else if (skip == "yes") {
    print("skip imputation of food price data")
    }
  }
SkipImputation("no")


### 2015年
##リスト
list_pid_2015 = unlist(purrr::map_if(names(dataset_2015), # pidないファイルを消してunlist
                                     ~ sum(stringi::stri_detect_regex(colnames(dataset_2015[[.x]]), "pid_use")) < 1,
                                     ""
)
)
list_hhid_2015 = unlist(purrr::map_if(names(dataset_2015), 
                                      ~ sum(stringi::stri_detect_regex(colnames(dataset_2015[[.x]]), "hhid_use")) < 1 | sum(stringi::stri_detect_regex(list_pid_2015, pattern = .x), na.rm = TRUE) > 0, # dataset_pidにに含まれないファイルを採用
                                      ""
)
)
list_comid_2015 = unlist(purrr::map_if(names(dataset_2015), 
                                       ~ sum(stringi::stri_detect_regex(list_hhid_2015, pattern = .x), na.rm = TRUE) > 0 | sum(stringi::stri_detect_regex(list_pid_2015, pattern = .x), na.rm = TRUE) > 0,
                                       "")
)
list_hhid_2015 = list_hhid_2015[list_hhid_2015 != "sect11a1_plantingw3"&list_hhid_2015 != "sect10b_harvestw3"]

## pidごとにmerge
dataset_2015_pid = purrr::reduce(dataset_2015[list_pid_2015], 
                                 dplyr::full_join, 
                                 by = "pid_use"
) %>%
  dplyr::select(., colnames(.)[stringi::stri_detect_regex(colnames(.), "(.*use$)|(.*use.x$)")])

dataset_2015_pid %<>% 
  #dplyr::select(-"hhid_use") %>%
  dplyr::rename(hhid_use = hhid_use.x) %>%
  dplyr::group_by(hhid_use) %>%
  dplyr::mutate(hhhead_female_use = dplyr::case_when(sum(hhhead_female_use == 1, na.rm = TRUE) > 0 ~ 1, TRUE ~ 0),
                hhhead_muslim_use = dplyr::case_when(sum(hhhead_muslim_use == 1, na.rm = TRUE) > 0 ~ 1, TRUE ~ 0),
                #sick_use = dplyr::case_when(sum(sick_use == 1, na.rm = TRUE) > 0 ~ 1, TRUE ~ 0)
) %>%
  dplyr::ungroup()

write.csv(dataset_2015_pid, "datas/processed/dataset_2015_pid.csv")


## hhidごとにmerge
dataset_2015_hhid = purrr::reduce(dataset_2015[list_hhid_2015], 
                                 dplyr::full_join, 
                                 by = "hhid_use") %>%
  dplyr::select(., colnames(.)[stringi::stri_detect_regex(colnames(.), "(.*use$)|(.*use.x$)")])

write.csv(dataset_2015_hhid, "datas/processed/dataset_2015_hhid.csv")


## comidごとにmerge
dataset_2015_comid = dplyr::full_join(dataset_2015[[list_comid_2015[1]]], dataset_2015[[list_comid_2015[2]]],
                                  by = "comid_use"
) %>%
  dplyr::select(., colnames(.)[stringi::stri_detect_regex(colnames(.), "(.*use$)|(.*use.x$)")])


write.csv(dataset_2015_comid, "datas/processed/dataset_2015_comid.csv")



### 2018年
## リスト
list_pid_2018 = unlist(purrr::map_if(names(dataset_2018), # pidないファイルを消してunlist
                                     ~ sum(stringi::stri_detect_regex(colnames(dataset_2018[[.x]]), "pid_use")) < 1,
                                     ""
)
)
list_hhid_2018 = unlist(purrr::map_if(names(dataset_2018), 
                                      ~ sum(stringi::stri_detect_regex(colnames(dataset_2018[[.x]]), "hhid_use")) < 1 | sum(stringi::stri_detect_regex(list_pid_2018, pattern = .x), na.rm = TRUE) > 0, # dataset_pidにに含まれないファイルを採用
                                      ""
)
)
list_comid_2018 = unlist(purrr::map_if(names(dataset_2018), 
                                       ~ sum(stringi::stri_detect_regex(list_hhid_2018, pattern = .x), na.rm = TRUE) > 0 | sum(stringi::stri_detect_regex(list_pid_2018, pattern = .x), na.rm = TRUE) > 0,
                                       "")
)
list_hhid_2018 = list_hhid_2018[list_hhid_2018 != "sect14b_harvestw4"]

## pidごと
dataset_2018_pid = purrr::reduce(dataset_2018[list_pid_2018], 
                                 dplyr::full_join, 
                                 by = "pid_use"
) %>%
  dplyr::select(., colnames(.)[stringi::stri_detect_regex(colnames(.), "(.*use$)|(.*use.x$)")])

dataset_2018_pid %<>%
  dplyr::rename(hhid_use = hhid_use.x) %>%
  dplyr::group_by(hhid_use) %>%
  dplyr::mutate(hhhead_female_use = dplyr::case_when(sum(hhhead_female_use == 1, na.rm = TRUE) > 0 ~ 1, TRUE ~ 0),
                hhhead_muslim_use = dplyr::case_when(sum(hhhead_muslim_use == 1, na.rm = TRUE) > 0 ~ 1, TRUE ~ 0)
                )

write.csv(dataset_2018_pid, "datas/processed/dataset_2018_pid.csv")

## hhidごと
dataset_2018_hhid = purrr::reduce(dataset_2018[list_hhid_2018], 
                                  dplyr::full_join, 
                                  by = "hhid_use") %>%
  dplyr::select(., colnames(.)[stringi::stri_detect_regex(colnames(.), "(.*use$)|(.*use.x$)")])
write.csv(dataset_2018_hhid, "datas/processed/dataset_2018_hhid.csv")

## comidごと
dataset_2018_comid = dplyr::full_join(dataset_2018[[list_comid_2018[1]]], dataset_2018[[list_comid_2018[2]]],
                                      by = "comid_use"
) %>%
  dplyr::select(., colnames(.)[stringi::stri_detect_regex(colnames(.), "(.*use$)|(.*use.x$)")])
write.csv(dataset_2018_comid, "datas/processed/dataset_2018_comid.csv")



## 2020年
## pid, comid: null; hhidのみ
list_r = list("r1", "r2", "r3", "r4")
dataset_2020_list = list()
for (i in list_r) {
  name_by_r = paste("dataset_2020_hhid", i, sep = "_")
  pattern = paste0("^", i, ".*")
  list_hhid_2020 = unlist(purrr::map_if(names(dataset_2020), # pidないファイルを消してunlist
                                           ~ !stringi::stri_detect_regex(.x, pattern),
                                           ""
  )
  )
  df = purrr::reduce(dataset_2020[list_hhid_2020], 
                                    dplyr::full_join, 
                                    by = "hhid_use") %>%
    dplyr::select(., colnames(.)[stringi::stri_detect_regex(colnames(.), "(.*use$)|(.*use.x$)")]) %>%
    dplyr::distinct(hhid_use, .keep_all = TRUE)
  filename = paste(name_by_r, "csv", sep = ".")
  write.csv(df, file.path("datas", "processed", filename))
  dataset_2020_list[[name_by_r]] = df
}

# merge= all
dataset_list = list(dataset_2015_comid = dataset_2015_comid, 
                    dataset_2015_hhid = dataset_2015_hhid,
                    dataset_2015_pid = dataset_2015_pid,
                    dataset_2018_comid = dataset_2018_comid,
                    dataset_2018_hhid = dataset_2018_hhid,
                    dataset_2018_pid = dataset_2018_pid,
                    dataset_2020_hhid_r1 = dataset_2020_list$dataset_2020_hhid_r1,
                    dataset_2020_hhid_r2 = dataset_2020_list$dataset_2020_hhid_r2,
                    dataset_2020_hhid_r3 = dataset_2020_list$dataset_2020_hhid_r3,
                    dataset_2020_hhid_r4 = dataset_2020_list$dataset_2020_hhid_r4
)
for (i in names(dataset_list)) {
  colnames(dataset_list[[i]]) = gsub("+.x$", "", colnames(dataset_list[[i]]))
}

### hhid, comid, pidごとのデータをまーじする関数
merge_by_year = function(year){
  name_dataset_hhid = paste("dataset", year, "hhid", sep = "_")
  name_dataset_pid = paste("dataset", year, "pid", sep = "_")
  name_dataset_comid = paste("dataset", year, "comid", sep = "_")
  # 重複するcolumnsを消す
  dataset_hhid = dataset_list[[name_dataset_hhid]] %>% .[,!duplicated(colnames(.))]
  dataset_comid = dataset_list[[name_dataset_comid]] %>% .[,!duplicated(colnames(.))]
  dataset_pid = dataset_list[[name_dataset_pid]] %>%  .[,!duplicated(colnames(.))]
  # 結合
  dataset_all = dataset_hhid %>%
    dplyr::left_join(., dataset_comid, by = "comid_use") %>%
    dplyr::left_join(., dataset_pid %>% dplyr::distinct(hhid_use, .keep_all = TRUE), by = "hhid_use") 
  return(dataset_all)
}
dataset_2015_all = merge_by_year(year = "2015")
dataset_2018_all = merge_by_year(year = "2018")


### 変数加工
mutate_mean = function(data, var_list){
  df = data
  for (i in var_list) {
    var_h = paste(i, "h_use", sep = "_")
    var_p = paste(i, "p_use", sep = "_")
    var_new = paste(i, "day_use", sep = "_")
    df %<>% dplyr::mutate(!!var_new := (eval(parse(text = var_h)) + eval(parse(text = var_p))) / 2)
  }
  return(df)
}

mutate_percapcons = function(data, var_list, name_to){
  data %<>% 
    dplyr::mutate(!!name_to := apply(dplyr::select(., var_list), MARGIN = 1, FUN = sum) / hhsize_use) 
  return(data)
}


mutate_logvar = function(data, var_list){
  for (i in var_list) {
    new_name = paste("l", i, sep = "")
    data %<>% dplyr::mutate(!!new_name := log10(eval(parse(text = i)) + 0.001))
  }
  return(data)
}

mutate_capital_dummy = function(data){
  df = data %>%
    dplyr::mutate(capital = dplyr::case_when(stringi::stri_detect_regex(hhid_use, "^.37.*")~ 1, TRUE ~ 0))
  return(df)
}


mutate_error = function(data, year){
  if(year == "2018"){
    cutoff = quantile(data[["lpercapcons_use"]], 0.092, na.rm = TRUE) 
  } else if (year == "2020"){
    cutoff = quantile(data[["lpercapcons_use"]], 0.159, na.rm = TRUE) 
  } else if (year == "2015"){
    cutoff = quantile(data[["lpercapcons_use"]], 0.021, na.rm = TRUE) 
  }
  
  df = data %>%
    dplyr::mutate(povrank_use = dplyr::min_rank(lpercapcons_use))
  
  for (i in c("popbnfcry", "povline")) {
    varname_use = paste0(i, "_use")
    if(i == "popbnfcry"){
      df %<>% dplyr::mutate(!!varname_use := dplyr::case_when(lpercapcons_use <= cutoff ~ 1, lpercapcons_use > cutoff ~ 0, TRUE ~ NA_real_))
    } else if (i == "povline") {
      df %<>% dplyr::mutate(!!varname_use := dplyr::case_when(percapcons_use <= (137430/265) ~ 1, percapcons_use > (137430/265) ~ 0, TRUE ~ NA_real_))
    }

    ex_error_varname = paste("ex_error", i, "use", sep = "_")
    in_error_varname = paste("in_error", i, "use", sep = "_")
    error_varname = paste("error", i, "use", sep = "_")
    
    df %<>% 
      dplyr::mutate(!!ex_error_varname := dplyr::case_when(.[varname_use] == 1 & trnsfr_any_dummy_use == 0 ~ 1, (.[varname_use] == 0 & trnsfr_any_dummy_use == 1)|(.[varname_use] == 1 & trnsfr_any_dummy_use == 1)|(.[varname_use] == 0 & trnsfr_any_dummy_use == 0) ~ 0, TRUE ~ NA_real_)) %>%
      dplyr::mutate(!!in_error_varname := dplyr::case_when(.[varname_use] == 0 & trnsfr_any_dummy_use == 1 ~ 1, (.[varname_use] == 1 & trnsfr_any_dummy_use == 0)|(.[varname_use] == 1 & trnsfr_any_dummy_use == 1)|(.[varname_use] == 0 & trnsfr_any_dummy_use == 0) ~ 0, TRUE ~ NA_real_)) %>%
      dplyr::mutate(!!error_varname := dplyr::case_when(as.numeric(.[[ex_error_varname]]) + as.numeric(.[[in_error_varname]]) > 0 ~ 1, TRUE ~ 0))
  }
  return(df)
}

mutate_elite_error = function(data){
  df = data
  elites = list(elite_gov_use = "elite_gov", elite_lc_gov_use = "elite_lc", elite_con_use = "elite_con")
  standards = list(popbnfcry_use = "popbn", povline_use = "povln")
  for (e in names(elites)) {
    for (s in names(standards)) {
      iner_varname = paste("iner", elites[[e]], standards[[s]], "use", sep = "_")
      exer_varname = paste("exer", elites[[e]], standards[[s]], "use", sep = "_")
      df %<>%  
        dplyr::mutate(!!iner_varname := .[[e]]*.[[paste0("in_error_", s)]],
                      !!exer_varname := .[[e]]*.[[paste0("ex_error_", s)]]
                      )
    }
  }
  return(df)
}

list_means = list("cons_eatout", "cons_food", "cons_food_imp", "cons_nonfood1", "cons_nonfood2", "cons_nonfood3")

list_logs_2015 = c("percapcons_use","trnsfr_cash_use","trnsfr_food_use","trnsfr_inkind_use","dist_admctr_use","tempDegC_use","precip_use","elev_use","popdens_use", "rooms_use", "hhsize_use")
list_logs_2018 = c("percapcons_use","trnsfr_cash_use","trnsfr_food_use","trnsfr_inkind_use", "rooms_use", "hhsize_use")

dataset_2015_all %<>% 
  mutate_mean(., list_means) %>% 
  mutate_percapcons(., setdiff(colnames(.)[stringi::stri_detect_regex(colnames(.), "^cons_.*day_use$")], "cons_food_day_use"), name_to = "percapcons_use") %>% # use imputed food cons.
  mutate_logvar(., list_logs_2015) %>%
  mutate_error(., year = "2015") %>%
  mutate_elite_error(.)

dataset_2018_all %<>% 
  mutate_mean(., list_means) %>% 
  mutate_percapcons(., setdiff(colnames(.)[stringi::stri_detect_regex(colnames(.), "^cons_.*day_use$")], "cons_food_day_use"), name_to = "percapcons_use") %>% # use imputed food cons.
  mutate_logvar(., list_logs_2018) %>% 
  mutate_error(., year = "2018") %>%
  mutate_elite_error(.)

list_logs_2020 = list("trnsfr_cash_use", "trnsfr_food_use", "trnsfr_inkind_use")

dataset_2020_r1_all = dataset_list$dataset_2020_hhid_r1 %>% 
  dplyr::left_join(., dataset_2018_all %>% dplyr::select(c("hhid_use", "percapcons_use", "lpercapcons_use", "hhhead_female_use",  "hhhead_age_use",  "hh_head_literacy_use","hh_head_educ_pri_use",
                                                           "hh_head_educ_sec_use","hh_head_educ_high_use","hh_head_educ_vocation_use","hh_head_educ_col_use",
                                                           "lhhsize_use", "hh_work_employee_use", "hh_work_farm_use", "hh_work_business_use", "elite_lc_gov_use", "elite_gov_use", "elite_con_use")), by = "hhid_use") %>% 
  mutate_logvar(., list_logs_2020) %>%
  mutate_error(., year = "2020") %>%
  mutate_elite_error(.)

dataset_list = list(dataset_2015_all = dataset_2015_all,
                    dataset_2018_all = dataset_2018_all,
                    dataset_2020_all = dataset_2020_r1_all
                    )

dataset_list$dataset_2015_all %<>% dplyr::mutate(comid_use = stringr::str_sub(hhid_use, start = 1, end = -7))
dataset_list$dataset_2018_all %<>% dplyr::mutate(comid_use = stringr::str_sub(hhid_use, start = 1, end = -7))

for (i in names(dataset_list)) {
  dataset_list[[i]] %<>% mutate_capital_dummy(.)
}
# "_use"の文字消去
for (i in names(dataset_list)) {
  colnames(dataset_list[[i]]) = gsub("+_use", "", colnames(dataset_list[[i]]))
}
dataset_list$dataset_2018_all
# hist_aid
year = c("2015", "2018", "2020")
for (y in year) {
  if(y == "2015"){
    dataset_list$dataset_2015_all %<>%
      dplyr::mutate(hist_aid = NA)
  } else {
    if(y == "2018"){
      name_df_current = paste("dataset", "2018", "all", sep = "_")
      name_df_past = paste("dataset", "2015", "all", sep = "_")
    } else if (y == "2020") {
      name_df_current = paste("dataset", "2020", "all", sep = "_")
      name_df_past = paste("dataset", "2018", "all", sep = "_")
    }
    dataset_list[[name_df_current]] %<>% 
      dplyr::left_join(., 
                       dataset_list[[name_df_past]] %>%
                         dplyr::select(c("hhid", "trnsfr_any_dummy")) %>%
                         dplyr::rename(hist_aid = trnsfr_any_dummy),
                       by = "hhid"
      ) 
  }
}


### 異常な値処理
dataset_list$dataset_2018_all %<>% dplyr::mutate(hhhead_age = dplyr::na_if(hhhead_age, 130))

Run.merge_community.R = function(run = "yes"){
  if(run == "yes"){
    path_merge_community = file.path("code", "cleaning", "merge_community.R")
    source(path_merge_community)
    year = c("2015", "2018", "2020")
    
    for (y in year) {
      dataset_list[[paste("dataset", y, "all", sep = "_")]] %<>% 
        dplyr::left_join(.,  dataset_community_level[[paste("df_community_level", y, sep = "_")]], by = "comid")
      print(paste0("merge_", y))
    }
    return(dataset_list)
  } else if (run == "no"){
    print("skip merge_community.R")
    return(dataset_list)
    }
}
dataset_list = Run.merge_community.R(run = "yes")

dataset_list_all_year = purrr::map2(dataset_list, names(dataset_list), ~ dplyr::mutate(.x, year = gsub("(dataset_)|(_all)", "", .y))) %>% purrr::reduce(., dplyr::bind_rows)
write.csv(dataset_list_all_year, file.path("datas", "processed", "dataset_list_all_year.csv"))


print("done: merge.R")

### デバッグ
debug_origin = function(){
  dataset_2015_all = dataset_list[["dataset_2015_hhid"]] %>%
    dplyr::left_join(., dataset_2015_comid, by = c("comid_use.x" = "comid_use")) %>%
    dplyr::left_join(., dataset_2015_pid %>% dplyr::distinct(hhid_use, .keep_all = TRUE), by = "hhid_use") %>%
    dplyr::mutate(lpercapcons = log10((sum(cons_eatout_day_use, 
                                           cons_food_day_use,
                                           cons_nonfood1_day_use, 
                                           cons_nonfood2_day_use,
                                           cons_nonfood3_day_use,
                                           cons_nonfood4_day_use,
                                           cons_nonfood5_day_use,
                                           na.rm = TRUE
    ) / hhsize_use)+0.01),
    ltrnsfr_cash_use = log10(trnsfr_cash_use+0.01),
    ltrnsfr_food_use = log10(.$trnsfr_food_use+0.01),
    ltrnsfr_inkind_use = log10(trnsfr_inkind_use+0.01),
    ldist_admctr_use = log10(dist_admctr_use),
    ltempDegC_use = log10(tempDegC_use+0.01),
    lprecip_use = log10(precip_use+0.01),
    lelev_use = log10(elev_use+0.01),
    lpopdens_use = log10(popdens_use)+0.01
    )
  dataset_2018_all = dataset_list[["dataset_2018_hhid"]] %>%
    dplyr::left_join(., dataset_2018_comid, by = "comid_use") %>%
    dplyr::left_join(., dataset_2018_pid %>% dplyr::distinct(hhid_use, .keep_all = TRUE), by = "hhid_use")%>%
    dplyr::mutate(lpercapcons = log10((sum(cons_eatout_day_use, 
                                           cons_food_day_use,
                                           cons_nonfood1_day_use, 
                                           cons_nonfood2_day_use,
                                           cons_nonfood3_day_use,
                                           cons_nonfood4_day_use,
                                           na.rm = TRUE
    ) / hhsize_use)+0.01),
    ltrnsfr_cash_use = dplyr::case_when(!is.na(trnsfr_cash_dummy_pre_use) & is.na(trnsfr_cash_use) ~ 0, TRUE ~ log10(trnsfr_cash_use+0.01)),
    ltrnsfr_food_use = dplyr::case_when(!is.na(trnsfr_food_dummy_pre_use) & is.na(trnsfr_food_use) ~ 0, TRUE ~ log10(trnsfr_food_use+0.01)),
    ltrnsfr_inkind_use = dplyr::case_when(!is.na(trnsfr_inkind_dummy_pre_use) & is.na(trnsfr_inkind_use) ~ 0, TRUE ~ log10(trnsfr_inkind_use+0.01))
    )

}



