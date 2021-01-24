#"""
#使用する変数を格納した年度毎のデータセット（クレンジング前）の作成
#"""

library(magrittr)

### read.Rからデータ読み込み

path_read = file.path("code", "loading", "read.R")
readdatas = source(path_read)

mutate_comid = function(data){
  data %<>% 
    dplyr::mutate(., state = formatC(as.numeric(.$"state"), width = 2, flag = "0")) %>%
    dplyr::mutate(., lga = formatC(as.numeric(.$"lga"), width = 4, flag = "0")) %>%
    dplyr::mutate(., ea = formatC(as.numeric(.$"ea"), width = 4, flag = "0")) %>%
    dplyr::mutate(comid_use = paste(.$"zone", .$"state", .$"lga", .$"ea", sep = ""))
  return(data)
}

mutate_hhid = function(data){ #16行のhhid
  data %<>% 
    dplyr::mutate(., state = formatC(as.numeric(.$"state"), width = 2, flag = "0")) %>%
    dplyr::mutate(., lga = formatC(as.numeric(.$"lga"), width = 4, flag = "0", format = "d")) %>%
    dplyr::mutate(., ea = formatC(as.numeric(.$"ea"), width = 4, flag = "0", format = "d")) %>%
    dplyr::mutate(., hhid = formatC(.$"hhid", width = 6, flag = "0", format = "d")) %>%
    dplyr::mutate(hhid_use = paste(.$"zone", .$"state", .$"lga", .$"ea", .$"hhid", sep = ""))
  return(data)
}

mutate_pid = function(data){ #19行
  data %<>% 
    dplyr::mutate(., state = formatC(as.numeric(.$"state"), width = 2, flag = "0")) %>%
    dplyr::mutate(., lga = formatC(as.numeric(.$"lga"), width = 4, flag = "0", format = "d")) %>%
    dplyr::mutate(., ea = formatC(as.numeric(.$"ea"), width = 4, flag = "0", format = "d")) %>%
    dplyr::mutate(., hhid = formatC(.$"hhid", width = 6, flag = "0",  format = "d")) %>%
    dplyr::mutate(., indiv = formatC(as.numeric(.$"indiv"), width = 2, flag = "0", format = "d")) %>%
    dplyr::mutate(., pid_use = paste(.$"zone", .$"state", .$"lga", .$"ea", .$"hhid", .$"indiv",sep = ""))
  return(data)
}

# 数字+地域名のようになっているデータを数字のみにする
convert_num = function(data, col_convert){
  for (i in col_convert) {
    data %<>%
      dplyr::mutate(., !!i := stringr::str_extract(.[[i]], "[[:digit:]]+"))
  }
  return(data)
}

# 2020年のでもデータを数字のみのデータにする
list_demo = list("zone", "state", "lga")
for (i in names(readdatas$value)) {
  if(stringi::stri_detect_regex(i, "^r") == TRUE){
    readdatas$value[[i]] %<>% 
      convert_num(., list_demo) 
  } else{
    next
  }
}


# comid作成
readdatas$value %<>% purrr::map(., ~mutate_comid(.x))

# hhidとpid作成
for (i in names(readdatas$value)) {
  if(sum(stringi::stri_detect_regex(colnames(readdatas$value[[i]]), ".*hhid.*")) > 0){
    if(sum (stringi::stri_detect_regex(colnames(readdatas$value[[i]]), ".*indiv.*")) > 0){
      readdatas$value[[i]] %<>% mutate_pid(.)
      print(paste("pid: ", i))
    }
    readdatas$value[[i]] %<>% mutate_hhid(.)
    print(paste("hhid: ", i))
  }
}

### データセット作成変数
mutate_dataset = function(list_dataset){
  usedata = list()
  
  for (i in names(list_dataset)) {
    file = paste(i, ".csv", sep = "")
    
    if(sum (stringi::stri_detect_regex(colnames(readdatas$value[[file]]), ".*pid_use*")) > 0){
      var_list = c("pid_use", "hhid_use", "comid_use", list_dataset[[i]], "zone", "state", "lga", "ea", "hhid", "indiv")
    } else if (sum(stringi::stri_detect_regex(colnames(readdatas$value[[file]]), ".*hhid_use.*")) > 0){
      var_list = c("hhid_use", "comid_use", list_dataset[[i]], "zone", "state", "lga","ea", "hhid")
    } else {
      var_list = c("comid_use",  list_dataset[[i]], "zone", "state", "lga", "ea")
    }
    df = readdatas$value[[file]] %>%
      dplyr::select(var_list)
    usedata[[i]] = df #df作成
  }
  
  return(usedata)
}

### 2015年
## 各ファイルで必要なデータと変数 2015 mutate_usedata func.のlist_dataset
list_dataset_2015 = list(sect1_harvestw3 = c("s1q2", "s1q3", "s1q4", "s1q4a","s1q17"), # stateの列がないため使わないharvestを使う
                         sect7a_plantingw3 = c("item_cd","s7aq1","s7aq2"),
                         sect7b_plantingw3 = c("item_cd", "s7bq1","s7bq2a", "s7bq2b","s7bq3a","s7bq3b","s7bq4"),
                         sect8a_plantingw3 = c("item_cd","s8q1","s8q2"),
                         sect8b_plantingw3 = c("item_cd","s8q3","s8q4"),
                         sect8c_plantingw3 = c("item_cd","s8q5","s8q6"),
                         sect3_plantingw3 = c("s3q4","s3q14","s3q15"),
                         sect5_plantingw3 = c("item_cd", "item_desc", "s5q1", "s5q4"), #  pmt estimation
                         sect11_plantingw3 = c("s11q1", "s11q6", "s11q7", "s11q8", "s11q9", "s11q10", "s11q11", "s11q17b", "s11q31", "s11q32", "s11q33a", "s11q33b", "s11q35", "s11q36"), # pmt estimation
                         sect11a_plantingw3 = c( "s11aq1"),
                         sect11a1_plantingw3 = c("s11aq4a1", "s11aq4c"),
                         sect11i_plantingw3 = c( "animal_cd", "animal_desc", "s11iq1", "s11iq2", "s11iq3"),
                         sectc_plantingw3 = c("sector"),
                         #sectc1_plantingw3 = c("pid", "c1q2", "c1q3", "c1q4a", "c1q4b", "c1q5", "c1q6"),
                         #sectc2a_plantingw3 = c("item_cd", "c2q1", "c2q3"),
                         #sect3_harvestw3 = c("s3q4","s3q14","s3q15","s3q21a","s3q23", "s3q24a", "s3q34a", "s3q36", "s3q37a"),
                         sect2_harvestw3 = c("s2aq5", "s2aq9", "s2aq23i"),
                         sect3_harvestw3 = c("s3q4", "s3q5", "s3q6"),
                         #sect4a_harvestw3 = c("s4aq1","s4aq3","s4aq9", "s4aq10", "s4aq13", "s4aq14", "s4aq15", "s4aq17", "s4aq18", "s4aq19"),
                         sect10a_harvestw3 = c("item_cd", "item_desc","s10aq1","s10aq2"),
                         sect10b_harvestw3 = c("item_cd","item_desc","s10bq1","s10bq2a", "s10bq2b","s10bq3a","s10bq3b","s10bq4"),
                         #sect10c_harvestw3 = c("item_cd", "item_desc","s10cq8"),
                         sect10ca_harvestw3 = c("s10cq9"),
                         sect11a_harvestw3 = c("item_cd", "item_desc", "s11aq1", "s11aq2"),
                         sect11b_harvestw3 = c("item_cd","item_desc","s11bq3","s11bq4"),
                         sect11c_harvestw3 = c("item_cd","item_desc","s11cq5","s11cq6"),
                         sect11d_harvestw3 = c("item_cd","item_desc","s11dq7","s11dq8"),
                         sect11e_harvestw3 = c("item_cd","item_desc","s11eq9","s11eq11"),
                         sect14_harvestw3 = c("snet_cd","snet_desc","s14q1","s14q2a","s14q2d","s14q2e"),
                         #sect15a_harvestw3 = c("shock_cd","shock_desc","s15aq1"),
                         sectc6_harvestw3 = c("event_seq","event_cd","event_desc","c6q2","c6q3"),
                         #sectc1_harvestw3 = c("serial_no","c1q2","c1q3","c1q4a","c1q4b","c1q5","c1q6"),
                         #sectc_harvestw3 = c("sector"),
                         #sectc3_harvestw3 = c("group_cd","c3q1","c3q2","c3q3","c3q4"),
                         #sectc8a_harvestw3 = c("item_cd","c8q1","c8q3"),
                         #food_conv_w3 = c("item_cd", "unit_cd", "conv_national", "conv_NC_1", "conv_NE_2","conv_NW_3","conv_SE_4","conv_SS_5","conv_SW_6"),
                         nga_householdgeovars_y3 = c("dist_popcenter2","dist_admctr","af_bio_1","af_bio_12","popdensity","srtm_nga")
                         #cons_agg_wave3_visit1 = c("totcons"),
                         #cons_agg_wave3_visit2 = c("totcons"),
                         #secta10_harvestw3 = c("sa10q2"),
                         #sect12_plantingw3 = c("s12q2")
)

## 2015年のデータセット
dataset_2015 = mutate_dataset(list_dataset = list_dataset_2015)
dataset_2015[["food_conv_w3"]] = readdatas$value$food_conv_w3
### 2018年
## 各ファイルで必要なデータと変数 2018
col_sect2_harvestw4 = colnames(readdatas$value$sect2_harvestw4.csv)
col_sect2_harvestw4 = col_sect2_harvestw4[stringi::stri_detect_regex(col_sect2_harvestw4, "s2aq23.*")]

list_dataset_2018 = list(sect1_harvestw4 = c("s1q2","s1q3","s1q4","s1q4a","s1q17"),
                         sect7a_plantingw4 = c("item_cd","s7aq1","s7aq2"),
                         sect7b_plantingw4 = c("item_cd", "s7bq1","s7bq2a", "s7bq2b", "s7bq2c", "s7bq2_cvn", "s7bq8", "s7bq9a", "s7bq9b", "s7bq9c", "s7bq9_cvn", "s7bq10"),
                         sect8a_plantingw4 = c("item_cd","s8q1","s8q2"),
                         sect8b_plantingw4 = c("item_cd","s8q3","s8q4"),
                         sect8c_plantingw4 = c("item_cd","s8q5","s8q6"),
                         #sect10c_harvestw4 = c("item_cd","s10cq8"),
                         sect11_plantingw4 = c("s11q1","s11q33b","s11q36","s11q40","s11q47","s11q6","s11q7","s11q8","s11q9"),
                         sect11a_harvestw4 = c("item_cd","s11aq1","s11aq2"),
                         #sect11a1_plantingw4 = c("s11aq4c"),
                         sect2_harvestw4 = c("s2aq5", "s2aq9", col_sect2_harvestw4),
                         sect3a_harvestw4 = c("s3q4", "s3q5", "s3q6"),
                         #sect4a_harvestw4 = c("s4aq1","s4aq3","s4aq9", "s4aq10", "s4aq13", "s4aq14", "s4aq15", "s4aq17"),
                         sect10a_harvestw4 = c("item_cd","s10aq1","s10aq2"),
                         sect10b_harvestw4 = c("item_cd","s10bq1","s10bq2a", "s10bq2b", "s10bq2c", "s10bq2_cvn", "s10bq8", "s10bq9a", "s10bq9b", "s10bq9c", "s10bq9_cvn", "s10bq10"),
                         sect11b_harvestw4 = c("item_cd","s11bq3","s11bq4"),
                         sect11c_harvestw4 = c("item_cd","s11cq5","s11cq6"),
                         sect11d_harvestw4 = c("item_cd","s11dq7","s11dq8"),
                         sect11i_plantingw4 = c("animal_cd","s11iq1","s11iq2a"),
                         sect14a_harvestw4 = c("s14q1a__1","s14q1a__2","s14q1a__3"),
                         sect14b_harvestw4 = c("s14q2a","s14q2d","s14q2f","snet_cd"),
                         #sect15a_harvestw4 = c("s15aq1","s15aq2","s15aq7_1","s15aq7_2","s15aq7_3","s15aq7_4","shock_cd"),
                         sect3_plantingw4 = c("s3q4","s3q14","s3q15","s3q21a","s3q21b","s3q24a","s3q24b"),
                         #sect3a_harvestw4 = c("s3q4","s3q14","s3q15","s3q21a", "s3q21b"),
                         sect5_plantingw4 = c("item_cd","s5q1a", "s5q1","s5q4"),
                         #sect9_plantingw4 = c("s9q8f","s9q8g","s9q8h"),
                         #sectc_harvestw4 = c("sector"),
                         sectc_plantingw4 = c("sector"),
                         #sectc1_harvestw4 = c("serial_id", "c1q2","c1q3","c1q4a","c1q4b","c1q5","c1q6"),
                         #sectc1_plantingw4 = c("c01q02","c01q03","c01q04a","c01q04b","c01q05","c01q06","serial_id"),
                         #sectc2_plantingw4 = c("item_cd","c2q1","c2q2", "c2q2c", "c2q3"),
                         #sectc3_harvestw4 = c("c3q1","c3q2","c3q3","c3q4","group_cd"),
                         sectc6_harvestw4 = c("c6q1","c6q2__2016","c6q2__2017","c6q2__2018","c6q2__2019","c6q3","event_cd")
                         #sectc8_harvestw4 = c("item_cd","c8q1","c8q2", "c8q2c", "c8q3")
)

## 2018のデータセット
dataset_2018 = mutate_dataset(list_dataset = list_dataset_2018)

### 2020年
## 各ファイルで必要なデータと変数 2020
list_dataset_2020 = list(r1_sect_1 = c("sector"),
                         r1_sect_10 = c("s10q1","s10q3__19","s10q3__7","shock_cd"),
                         r1_sect_11 = c("assistance_cd","s11q1","s11q2","s11q3"),
                         #r1_sect_2 = c("s2q5","s2q6","s2q7"),
                         r1_sect_a_3_4_5_6_8_9_12 = c("s6q1","s6q5","s8q4","s8q8"),
                         r2_sect_1 = c("sector"),
                         r2_sect_11 = c("assistance_cd","s11q1","s11q2","s11q3__1","s11q3__2","s11q3__3","s11q3__7"),
                         #r2_sect_2 = c("s2q5","s2q6","s2q7"),
                         r2_sect_a_2_5_6_8_12 = c("s6q1","s6q5", "s8q4","s8q7","s8q8"),
                         r3_sect_1 = c("sector"),
                         r3_sect_10 = c("s10q1","s10q3__19","s10q3__7","shock_cd"),
                         r3_sect_11 = c("assistance_cd","s11q1","s11q2","s11q3__1","s11q3__2","s11q3__3","s11q3__7"),
                         #r3_sect_2 = c("s2q5","s2q6","s2q7"),
                         r3_sect_a_2_5_5a_6_12 = c("s6q1", "s6q5"),
                         r4_sect_1 = c("sector"),
                         r4_sect_11 = c("assistance_cd","s11q1","s11q2","s11q3__1","s11q3__2","s11q3__3","s11q3__7"),
                         #r4_sect_2 = c("s2q5","s2q6","s2q7"),
                         r4_sect_a_2_5_5b_6_8_9_12 = c("s6q1","s6q5", "s8q4","s8q7","s8q8")
)

dataset_2020 = mutate_dataset(list_dataset = list_dataset_2020)

rm(readdatas)
print("done: construct.R")
