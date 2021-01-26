#"""
#クレンジング: ファイルごとの処理
#"""
library(magrittr)

### construct.Rからデータ読み込み

path_construct = file.path("code","cleaning", "construct.R")
source(path_construct)




### 2015年のクレンジング
# 変数名一斉変更関数
rename_var =function(data, list){
  for (i in names(list)) {
    data %<>% dplyr::rename(., !!i := list[[i]])
  }
  return(data)
}

dataset_2015$sect1_harvestw3 %<>% 
  dplyr::mutate(., 
                christian_use = dplyr::case_when(.$s1q17 == 1 ~ 1, (.$s1q17 == 2)| (.$s1q17 == 3)| (.$s1q17 == 4) ~ 0, TRUE ~ NA_real_),
                muslim_use = dplyr::case_when(.$s1q17 == 2 ~ 1, (.$s1q17 == 1)| (.$s1q17 == 3)| (.$s1q17 == 4) ~ 0, TRUE ~ NA_real_),
                male_use = dplyr::case_when(.$s1q2 == 1 ~ 1, .$s1q2 == 2 ~ 0, TRUE ~ NA_real_),
                hhhead_use = dplyr::case_when(.$s1q3 == 1 ~ 1,  TRUE ~ 0), 
                age = .$s1q4
                ) %>%
  dplyr::group_by(hhid_use) %>%
  dplyr::mutate(hhhead_female_use = dplyr::case_when(sum(male_use == 0 & hhhead_use == 1, na.rm = TRUE) > 0 ~ 1, TRUE ~ 0)) %>%
  dplyr::mutate(hhhead_muslim_use = dplyr::case_when(sum(muslim_use == 0 & hhhead_use == 1, na.rm = TRUE) > 0 ~ 1, TRUE ~ 0)) %>%
  dplyr::mutate(hhhead_age_use = sum(dplyr::case_when(hhhead_use == 1 ~ age, TRUE ~ 0), na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(., dataset_2015$sect1_harvestw3 %>% dplyr::filter(s1q4a == 1) %>% dplyr::count(hhid_use), by = "hhid_use") %>%
  dplyr::rename(hhsize_use = n)

# education
dataset_2015$sect2_harvestw3 %<>%
  # 学歴変数
  dplyr::mutate(literacy = dplyr::case_when(s2aq5 == 1 ~ 1, s2aq5 == 2 ~ 0, TRUE ~ NA_real_),
                educ_pri = dplyr::case_when((s2aq9 == 16)|(s2aq9 == 21)|(s2aq9 == 22) ~ 1,  (s2aq9 == 51)|(s2aq9 == 52)|(s2aq9 == 27)|(s2aq9 == 28)  ~ NA_real_, is.na(s2aq9) ~ NA_real_, TRUE ~ 0),
                educ_sec = dplyr::case_when((s2aq9 == 23)|(s2aq9 == 24)|(s2aq9 == 25) ~ 1,  (s2aq9 == 51)|(s2aq9 == 52)|(s2aq9 == 27)|(s2aq9 == 28) ~ NA_real_, is.na(s2aq9) ~ NA_real_, TRUE ~ 0),
                educ_high = dplyr::case_when(s2aq9 == 26 ~ 1, (s2aq9 == 51)|(s2aq9 == 52)|(s2aq9 == 27)|(s2aq9 == 28) ~ NA_real_,  is.na(s2aq9) ~ NA_real_, TRUE ~ 0),
                educ_vocation = dplyr::case_when((s2aq9 == 31)|(s2aq9 == 32)|(s2aq9 == 33)|(s2aq9 == 34) ~ 1, is.na(s2aq9) ~ NA_real_, (s2aq9 == 51)|(s2aq9 == 52)|(s2aq9 == 27)|(s2aq9 == 28) ~ NA_real_, TRUE ~ 0),
                educ_col = dplyr::case_when((s2aq9 == 41)|(s2aq9 == 42)|(s2aq9 == 43) ~ 1, is.na(s2aq9) ~ NA_real_, (s2aq9 == 51)|(s2aq9 == 52)|(s2aq9 == 27)|(s2aq9 == 28) ~ NA_real_, TRUE ~ 0),
  ) 
  
  
# educをまーじ
dataset_2015$sect1_harvestw3 %<>% 
  dplyr::left_join(., dataset_2015$sect2_harvestw3 %>% dplyr::select(pid_use, literacy, educ_pri, educ_sec, educ_high, educ_vocation, educ_col, s2aq23i), by = "pid_use") %>%
  dplyr::group_by(hhid_use) %>%
  dplyr::mutate(hh_head_literacy_use = sum(dplyr::case_when(hhhead_use == 1 ~ literacy, hhhead_use != 1  ~ 0, TRUE ~ NA_real_), na.rm = TRUE),
                hh_head_educ_pri_use = sum(dplyr::case_when(hhhead_use == 1 ~ educ_pri, hhhead_use != 1  ~ 0, TRUE ~ NA_real_), na.rm = TRUE),
                hh_head_educ_sec_use = sum(dplyr::case_when(hhhead_use == 1 ~ educ_sec, hhhead_use != 1  ~ 0, TRUE ~ NA_real_), na.rm = TRUE),
                hh_head_educ_high_use = sum(dplyr::case_when(hhhead_use == 1 ~ educ_high, hhhead_use != 1  ~ 0, TRUE ~ NA_real_), na.rm = TRUE),
                hh_head_educ_vocation_use = sum(dplyr::case_when(hhhead_use == 1 ~ educ_vocation, hhhead_use != 1  ~ 0, TRUE ~ NA_real_), na.rm = TRUE),
                hh_head_educ_col_use = sum(dplyr::case_when(hhhead_use == 1 ~ educ_col, hhhead_use != 1  ~ 0, TRUE ~ NA_real_), na.rm = TRUE),
                cons_toteduc_day_use = sum(s2aq23i, na.rm = TRUE) / 365 # 教育支出
                ) %>%
  dplyr::ungroup()

# labor dataset_2015$sect3_harvestw3
dataset_2015$sect3_harvestw3 %<>%
  dplyr::group_by(hhid_use) %>%
  dplyr::mutate(hh_work_employee_use = sum(dplyr::case_when(s3q4 == 1 ~ 1, s3q4 == 2 ~ 0, TRUE ~ NA_real_), na.rm = TRUE),
                hh_work_farm_use = sum(dplyr::case_when(s3q5 == 1 ~ 1, s3q5 == 2 ~ 0, TRUE ~ NA_real_), na.rm = TRUE),
                hh_work_business_use = sum(dplyr::case_when(s3q6 == 1 ~ 1, s3q6 == 2 ~ 0, TRUE ~ NA_real_), na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(hhid_use, .keep_all = TRUE)

# elite
dataset_2015$sect3_plantingw3 %<>%
  dplyr::mutate(., 
                elite_public_use = dplyr::case_when((.$s3q4 == 1) & (.$s3q14 == 13) ~ 1, is.na(s3q4) ~ s3q4, TRUE ~ 0),
                elite_lc_gov_use  =  dplyr::case_when((.$s3q4 == 1) & (.$s3q15 == 3) ~ 1, is.na(s3q4) ~ s3q4, TRUE ~ 0),
                elite_gov_use  = dplyr::case_when((.$s3q4 == 1) & ((.$s3q15 == 3)|(.$s3q15 == 1)|(.$s3q15 == 2)) ~ 1, is.na(s3q4) ~ s3q4, TRUE ~ 0),
                ) %>%
    dplyr::mutate(., elite_con_use  = dplyr::case_when((.$elite_public_use == 0) & (.$elite_gov_use == 1) ~ 1, is.na(.$elite_gov_use) ~ .$elite_gov_use, TRUE ~ 0)) %>%
  dplyr::group_by(hhid_use) %>%
  dplyr::mutate(elite_public_use = sum(elite_public_use, na.rm = TRUE),
                elite_lc_gov_use = sum(elite_lc_gov_use, na.rm = TRUE),
                elite_gov_use = sum(elite_gov_use, na.rm = TRUE),
                elite_con_use = sum(elite_con_use, na.rm = TRUE)
  ) %>%
  dplyr::ungroup() 

# asset
asset_list = as.character(unique(dataset_2015$sect5_plantingw3$item_cd))
names(asset_list) = paste("asset_cd", unique(dataset_2015$sect5_plantingw3$item_cd), "use",sep = "_")

dataset_2015$sect5_plantingw3 %<>%
  dplyr::select(., c(hhid_use, comid_use, item_cd, s5q1)) %>%
  tidyr::pivot_wider(., names_from = item_cd, values_from = s5q1) %>%
  rename_var(., asset_list)

for (i in names(asset_list)) {
  dataset_2015$sect5_plantingw3  %<>% 
    dplyr::mutate(!!paste("dummy", i, sep = "_") := dplyr::case_when(.[[i]] > 0 ~ 1, .[[i]] == 0 ~ 0, TRUE ~ NA_real_),
                  !!paste("std", i, sep = "_") := (.[[i]] - mean(.[[i]], na.rm = TRUE)) / sd(.[[i]], na.rm = TRUE)
    )
}



dataset_2015$sect11_plantingw3 %<>%
  dplyr::mutate(.,
                houseowner_use = dplyr::case_when((.$s11q1 == 1) ~ 1, is.na(.$s11q1) ~ .$s11q1, TRUE ~ 0),
                wall_use =  dplyr::case_when(((.$s11q6 == 4)|(.$s11q6 == 5)|(.$s11q6 == 6)|(.$s11q6 == 9)) ~ 1, is.na(.$s11q6) ~ .$s11q6, TRUE ~ 0),
                roof_use = dplyr::case_when((.$s11q7 == 2) ~ 1, is.na(.$s11q7) ~ .$s11q7, TRUE ~ 0), #ironsheetが76%
                floor_use = dplyr::case_when((.$s11q8 == 3) ~ 1, is.na(.$s11q8) ~ .$s11q8, TRUE ~ 0), #3cementが65%, 2mudが21％
                electricity_use = dplyr::case_when(.$s11q17b == 1 ~ 1, .$s11q17b == 2 ~ 0, TRUE ~ NA_real_), 
                phone_use = dplyr::case_when(.$s11q31 == 1 ~ 1, .$s11q31 == 2 ~ 0, TRUE ~ NA_real_), 
                rooms_use = dplyr::case_when(!is.na(.$s11q9) ~ .$s11q9, TRUE ~ NA_real_), 
                pipewater_use = dplyr::case_when(((.$s11q33a == 1)|(.$s11q33a == 2)) ~ 1, is.na(.$s11q33a) ~ .$s11q33a, TRUE ~ 0), # pip water
                )

dataset_2015$sect11a_plantingw3 %<>%
  dplyr::mutate(., landowner_use = dplyr::case_when(.$s11aq1 == 1 ~ 1, .$s11aq1 == 2 ~ 0, TRUE ~ NA_real_))

dataset_2015$sect11a1_plantingw3 %<>%
  dplyr::mutate(.,
                plot_use = dplyr::case_when(.$s11aq4a1 == 1 ~ .$s11aq4c, TRUE ~ NA_real_)
                ) %>%
  dplyr::group_by(., hhid_use) %>%
  dplyr::mutate(., plot_use = sum(s11aq4c, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(hhid_use, .keep_all = TRUE)

# sect11a1_plantingw3とsect11a_plantingw3を結合
dataset_2015$sect11a_plantingw3 %<>% 
  dplyr::left_join(., dataset_2015$sect11a1_plantingw3[c("hhid_use", "plot_use")], by = "hhid_use") %>%
  dplyr::mutate(plot_use = dplyr::case_when(s11aq1 == 1 ~ plot_use, s11aq1 == 2 ~ 0, TRUE ~ NA_real_))

# animal
animal_list = as.character(unique(dataset_2015$sect11i_plantingw3$animal_cd))
names(animal_list) = paste("animal_cd", unique(dataset_2015$sect11i_plantingw3$animal_cd), "use", sep = "_")

dataset_2015$sect11i_plantingw3 %<>%
  dplyr::mutate(., s11iq2 = ifelse(.$s11iq1 == 2, 0, .$s11iq2)) %>%
  dplyr::select(., c(hhid_use, comid_use, animal_cd, s11iq2)) %>%
  tidyr::pivot_wider(., names_from = animal_cd, values_from = s11iq2) %>%
  rename_var(., animal_list)

for (i in names(animal_list)) {
  dataset_2015$sect11i_plantingw3  %<>% 
    dplyr::mutate(!!paste("dummy", i, sep = "_") := dplyr::case_when(.[[i]] > 0 ~ 1, .[[i]] == 0 ~ 0, TRUE ~ NA_real_),
                  !!paste("std", i, sep = "_") := (.[[i]] - mean(.[[i]], na.rm = TRUE)) / sd(.[[i]], na.rm = TRUE)
    )
}


### food price
#dataset_2015$sectc2a_plantingw3 %<>%
#  dplyr::select(., c(comid_use, item_cd, c2q3)) %>%
#  dplyr::mutate(., food_cd_use = paste(comid_use, item_cd, sep = "_")) %>%
#  dplyr::rename(., pfood_use = c2q3) %>%
#  tidyr::pivot_wider(names_prefix = "price_item_", names_from = item_cd, values_from = pfood_use)

### health
#dataset_2015$sect4a_harvestw3 %<>%
#  dplyr::mutate(sick = dplyr::case_when(s4aq3 == 1 ~ 1, s4aq3 == 2 ~ 0, TRUE ~ NA_real_),
#                cons_health_consul = dplyr::case_when(s4aq3 == 2 ~ 0, s4aq3 == 1 ~ s4aq9, TRUE ~ NA_real_),
#                cons_health_trip = dplyr::case_when(s4aq3 == 2 ~ 0, s4aq3 == 1 ~ s4aq10, TRUE ~ NA_real_),
#                cons_health_drug = dplyr::case_when(s4aq13 == 2 ~ 0, s4aq13 == 1 ~ s4aq14, TRUE ~ NA_real_),
#                cons_health_hospital = dplyr::case_when(s4aq15 == 2 ~ 0, s4aq15 == 1 ~ s4aq17, TRUE ~ NA_real_),
#                cons_health_medic = dplyr::case_when(s4aq18 == 2 ~ 0, s4aq18 == 1 ~ s4aq19, TRUE ~ NA_real_)
#                ) %>%
#  dplyr::group_by(hhid_use) %>%
#  dplyr::mutate(hh_sick_use = sum(sick, na.rm = TRUE),
#                cons_health_consul_day_use = sum(cons_health_consul, na.rm = TRUE) / (7*4),
#                cons_health_trip_day_use = sum(cons_health_trip, na.rm = TRUE) / (7*4),
#                cons_health_drug_day_use = sum(cons_health_drug, na.rm = TRUE) / (7*4),
#                cons_health_hospital_day_use = sum(cons_health_hospital, na.rm = TRUE) / 365,
#                cons_health_medic_day_use = sum(cons_health_medic, na.rm = TRUE) / 365,
#  ) %>%
#  dplyr::ungroup() %>%
#  dplyr::distinct(hhid_use, .keep_all = TRUE)

# eatoutの消費
dataset_2015$sect10a_harvestw3 %<>%
  dplyr::mutate(s10aq2 = dplyr::case_when(s10aq1 == 1 ~ s10aq2, s10aq1 == 2 ~ 0, TRUE ~ NA_real_)) %>%
  dplyr::group_by(hhid_use) %>%
  dplyr::mutate(cons_eatout_h_use = sum(s10aq2, na.rm = TRUE) / 7 )%>%
  dplyr::ungroup() %>%
  dplyr::distinct(hhid_use, .keep_all = TRUE)
dataset_2015$sect7a_plantingw3 %<>%
  dplyr::mutate(s7aq2 = dplyr::case_when(s7aq1 == 1 ~ s7aq2, s7aq1 == 2 ~ 0, TRUE ~ NA_real_)) %>%
  dplyr::group_by(hhid_use) %>%
  dplyr::mutate(cons_eatout_p_use = sum(s7aq2, na.rm = TRUE) / 7) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(hhid_use, .keep_all = TRUE)

# food消費
dataset_2015$food_conv_w3 %<>% 
  dplyr::select(c("item_cd", "unit_cd", "conv_national", "conv_NC_1", "conv_NE_2","conv_NW_3","conv_SE_4","conv_SS_5","conv_SW_6")) %>%
  tidyr::pivot_longer(cols = -c(1:2), values_to = "cvn", names_to = "var") %>%
  dplyr::mutate(zone = dplyr::case_when(var == "conv_NC_1" ~ "1", 
                                                          var == "conv_NE_2" ~ "2",
                                                          var == "conv_NW_3" ~ "3",
                                                          var == "conv_SE_4" ~ "4",
                                                          var == "conv_SS_5" ~ "5",
                                                          var == "conv_SW_6" ~ "6",
                                                          TRUE ~ NA_character_
                                                          ),
                unit_cd = formatC(unit_cd , width = 3, flag = "0")
                ) %>%
  dplyr::mutate(cvn_cd = paste0(formatC(item_cd , width = 3, flag = "0"), unit_cd, zone))

dataset_2015$sect7b_plantingw3 %<>%
  dplyr::mutate(cvn_cd = dplyr::case_when(s7bq1 == 1 ~ paste0(formatC(item_cd , width = 3, flag = "0"), formatC(s7bq2b , width = 3, flag = "0"), zone),
                                          TRUE ~ NA_character_)
                ) %>% 
  dplyr::left_join(., dataset_2015$food_conv_w3, by = c("cvn_cd", "item_cd")) %>%
  dplyr::mutate(p0 = s7bq4 / (cvn*s7bq3a),
                q0 = s7bq2a*cvn
                )
dataset_2015$sect10b_harvestw3 %<>%  
  plyr::mutate(cvn_cd = dplyr::case_when(s10bq1 == 1 ~ paste0(formatC(item_cd , width = 3, flag = "0"), formatC(s10bq2b , width = 3, flag = "0"), zone),
                                         TRUE ~ NA_character_)
  ) %>% 
  dplyr::left_join(., dataset_2015$food_conv_w3, by = c("cvn_cd", "item_cd")) %>%
  dplyr::mutate(p1 = s10bq4 / (cvn*s10bq3a),
                q1 = s10bq2a*cvn
  )
dataset_2015$sect7b_plantingw3 %<>% 
  dplyr::full_join(., dataset_2015$sect10b_harvestw3, by = c("hhid_use", "item_cd")) %>%
  dplyr::group_by(hhid_use) %>%
  dplyr::mutate(h_idx = sum(p1 * q0, na.rm = TRUE) / sum(p0 * q0, na.rm = TRUE),
                cons_food_p_use = sum(p0*q0, na.rm = TRUE) / 7,
                cons_food_h = sum(p1*q1, na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(h_idx_use = dplyr::case_when((h_idx > 0) & (!is.na(h_idx)) & (!is.infinite(h_idx)) & (!is.nan(h_idx)) ~ h_idx,
                                             TRUE ~ NA_real_
  )
  ) %>%
  dplyr::mutate(cons_food_h_use = cons_food_h / h_idx_use / 7) %>%
  dplyr::mutate(category = dplyr::case_when(item_cd>=10 & item_cd <= 23 ~ "grain_flour",
                                            item_cd >= 25 & item_cd<=29 ~ "processed",
                                            item_cd >= 30 & item_cd<=38 ~ "plantain",
                                            item_cd >= 40 & item_cd<=48 ~ "seed",
                                            item_cd >= 50 & item_cd<=56 ~ "oil",
                                            (item_cd >= 60 & item_cd<=69)|(item_cd == 601) ~ "fruit",
                                            item_cd >= 70 & item_cd<=79 ~ "vegetable",
                                            item_cd >= 80 & item_cd<=85 ~ "poultry",
                                            item_cd >= 90 & item_cd<=96 ~ "meat",
                                            item_cd >= 100 & item_cd<=107 ~ "seafood",
                                            item_cd >= 110 & item_cd<=115 ~ "dairy",
                                            item_cd >= 120 & item_cd<=122 ~ "like_beverage",
                                            item_cd >= 130 & item_cd<=133 ~ "confectionary",
                                            item_cd >= 140 & item_cd<=147 ~ "others",
                                            item_cd >= 150 & item_cd<=155 ~ "non-alco",
                                            item_cd >= 160 & item_cd<=164 ~ "alco"
  ))

# dataset_2015$sect10c_harvestw3 他の項目で聞いているfoodなので不要
  
dataset_2015$sect10ca_harvestw3 %<>%
  dplyr::mutate(share_use = dplyr::case_when(s10cq9 == 1 ~ 1, s10cq9 == 2 ~ 0, TRUE ~ NA_real_))

# non-food expenditure within the household (in the past 7 days)
dataset_2015$sect11a_harvestw3 %<>% 
  dplyr::mutate(., 
                cons = dplyr::case_when(s11aq1 == 2 ~ 0, TRUE ~ s11aq2)
  ) %>%
  dplyr::group_by(hhid_use) %>%
  dplyr::mutate(cons_nonfood1_h_use = sum(cons, na.rm = TRUE) / 7) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(hhid_use, .keep_all = TRUE)
dataset_2015$sect8a_plantingw3 %<>% 
  dplyr::mutate(., 
                cons = dplyr::case_when(s8q1 == 2 ~ 0, TRUE ~ s8q2)
  ) %>%
  dplyr::group_by(hhid_use) %>%
  dplyr::mutate(cons_nonfood1_p_use = sum(cons, na.rm = TRUE) / 7) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(hhid_use, .keep_all = TRUE)

# non-food expenditure within the household (one month recall)
dataset_2015$sect11b_harvestw3 %<>% 
  dplyr::mutate(., 
                cons = dplyr::case_when(s11bq3 == 2 ~ 0, TRUE ~ s11bq4)
  ) %>%
  dplyr::group_by(hhid_use) %>%
  dplyr::mutate(cons_nonfood2_h_use = sum(cons, na.rm = TRUE) / 30) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(hhid_use, .keep_all = TRUE)
dataset_2015$sect8b_plantingw3 %<>% 
  dplyr::mutate(., 
                cons = dplyr::case_when(s8q3 == 2 ~ 0, TRUE ~ s8q4)
  ) %>%
  dplyr::group_by(hhid_use) %>%
  dplyr::mutate(cons_nonfood2_p_use = sum(cons, na.rm = TRUE) / 30) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(hhid_use, .keep_all = TRUE)

# non-food expenditure within the household (six month recall)***
dataset_2015$sect11c_harvestw3 %<>% 
  dplyr::mutate(., 
                cons = dplyr::case_when(s11cq5 == 2 ~ 0, TRUE ~ s11cq6)
  ) %>%
  dplyr::group_by(hhid_use) %>%
  dplyr::mutate(cons_nonfood3_h_use = sum(cons, na.rm = TRUE) / (30*6)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(hhid_use, .keep_all = TRUE)
dataset_2015$sect8c_plantingw3 %<>% 
  dplyr::mutate(., 
                cons = dplyr::case_when(s8q5 == 2 ~ 0, TRUE ~ s8q6)
  ) %>%
  dplyr::group_by(hhid_use) %>%
  dplyr::mutate(cons_nonfood3_p_use = sum(cons, na.rm = TRUE) / (30*6)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(hhid_use, .keep_all = TRUE)

# non-food expenditure within the household (twelve month recall)
dataset_2015$sect11d_harvestw3 %<>% 
  dplyr::mutate(., 
                cons = dplyr::case_when(s11dq7 == 2 ~ 0, TRUE ~ s11dq8)
  ) %>%
  dplyr::group_by(hhid_use) %>%
  dplyr::mutate(cons_nonfood4_day_use = sum(cons, na.rm = TRUE) / 365) %>%
  dplyr::ungroup() %>%  
  dplyr::distinct(hhid_use, .keep_all=TRUE)

# non-food expenditure within the household (twelve month recall)
dataset_2015$sect11e_harvestw3%<>% 
  dplyr::mutate(., 
                cons = dplyr::case_when(s11eq9 == 2 ~ 0, TRUE ~ s11eq11)
  ) %>%
  dplyr::group_by(hhid_use) %>%
  dplyr::mutate(cons_nonfood5_day_use = sum(cons, na.rm = TRUE) / 365) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(hhid_use, .keep_all = TRUE)

# snet in the last 12 months (dummyも)
dataset_2015$sect14_harvestw3 %<>%
  dplyr::mutate(.,
                trnsfr_cash_use = dplyr::case_when(s14q1 == 2 ~ 0, TRUE ~ s14q2a),
                trnsfr_food_use = dplyr::case_when(s14q1 == 2 ~ 0, TRUE ~ s14q2d),
                trnsfr_inkind_use = dplyr::case_when(s14q1 == 2 ~ 0, TRUE ~ s14q2e)
                ) %>%
  dplyr::group_by(hhid_use) %>%
  dplyr::mutate(trnsfr_any_dummy_use = dplyr::case_when(sum(stringi::stri_detect_regex(s14q1, 1), na.rm = TRUE) > 0 ~ 1, TRUE ~ 0)
                ) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(hhid_use, .keep_all = TRUE)

# disaster sectc6_harvestのdisaster項目を使用
#dataset_2015$sect15a_harvestw3%<>%
#  dplyr::group_by(hhid_use) %>%
#  dplyr::mutate(nd_flood_use = dplyr::case_when(sum(shock_cd == 13 & s15aq1 == 1, na.rm = TRUE) > 0 ~ 1, TRUE ~ 0),
#                nd_pest_use = dplyr::case_when(sum(shock_cd == 14 & s15aq1 == 1, na.rm = TRUE) > 0 ~ 1, TRUE ~ 0),
#                nd_drought_use = dplyr::case_when(sum(shock_cd == 12 & s15aq1 == 1, na.rm = TRUE) > 0 ~ 1, TRUE ~ 0)
#  ) %>%
#  dplyr::ungroup()
           
# urban
dataset_2015$sectc_plantingw3 %<>%
  dplyr::mutate(urban_use = dplyr::case_when(sector == 1 ~ 1, sector == 2 ~ 0, TRUE ~ NA_real_))

# 
     
# disaster
dataset_2015$sectc6_harvestw3 %<>%
  dplyr::group_by(comid_use) %>%
  dplyr::mutate(nd_drought_use = dplyr::case_when(sum(event_cd == 1 & c6q2 >= 2013, na.rm = TRUE) > 0 ~ 1, TRUE ~ 0),
                nd_flood_use = dplyr::case_when(sum(event_cd == 2& c6q2>=2013, na.rm = TRUE) > 0 ~ 1, TRUE ~ 0),
                nd_pest_use = dplyr::case_when(sum(event_cd == 3& c6q2>=2013, na.rm = TRUE) > 0 ~ 1, TRUE ~ 0),#crop disease / pest
                nd_livestock_use = dplyr::case_when(sum(event_cd == 4& c6q2>=2013, na.rm = TRUE) > 0 ~ 1, TRUE ~ 0),
                nd_epi_use = dplyr::case_when(sum(event_cd == 5& c6q2 > 2013, na.rm = TRUE) > 0 ~ 1, TRUE ~ 0),
                ) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(comid_use, .keep_all = TRUE)

# dataset_2015$sectc3_harvestw3: social group個人と紐付けできない, sectc1_harvestw3: headman個人と紐付けできない

# dataset_2015$sectc8a_harvestw3: food priceなくてもcons計算できる

# その土地のdemographic info.
dataset_2015$nga_householdgeovars_y3 %<>%
  dplyr::rename(dist_popcenter2_use = dist_popcenter2, # HH Distance in (KMs) to Nearest Population Center with +20,000
                dist_admctr_use = dist_admctr,
                tempDegC_use = af_bio_1, # anual ave. temperature
                precip_use = af_bio_12, #annunal precipitation (mm)
                popdens_use = popdensity,
                elev_use = srtm_nga # elevation(m)
                
  )
# totocons算出するので不要: dataset_2015$cons_agg_wave3_visit1, visit2

#dataset_2015$secta10_harvestw3 %<>%
#  dplyr::group_by(hhid_use) %>%
#  dplyr::mutate(elite_con_use = dplyr::case_when(sum(sa10q2 == 3|sa10q2 == 5|sa10q2 == 17|sa10q2 == 18 ,na.rm = TRUE) > 0 ~ 1, TRUE ~ 0), # 3: VILLAGE HEADMAN; 5: POLITICAL LEADER; 17:GOVERNMENT; 18: PARLIAMENT MEMBER
#                share_use = dplyr::case_when(sum(sa10q2 == 1|sa10q2 == 2 ,na.rm = TRUE) > 0 ~ 1, TRUE ~ 0), # 1: relative; 2: FRIEND/NEIGHBOUR
#                ) %>%
#  dplyr::ungroup()
# dataset_2015$sect12_plantingw3: dataset_2015$secta10_harvestw3の方がnetworkのデータ多いのでそちらを採用







########### 2018年
dataset_2018$sect1_harvestw4 %<>% 
  dplyr::mutate(., 
                christian_use = dplyr::case_when(.$s1q17 == 1 ~ 1, (.$s1q17 == 2)| (.$s1q17 == 3)| (.$s1q17 == 4) ~ 0, TRUE ~ NA_real_),
                muslim_use = dplyr::case_when(.$s1q17 == 2 ~ 1, (.$s1q17 == 1)| (.$s1q17 == 3)| (.$s1q17 == 4) ~ 0, TRUE ~ NA_real_),
                male_use = dplyr::case_when(.$s1q2 == 1 ~ 1, .$s1q2 == 2 ~ 0, TRUE ~ NA_real_),
                hhhead_use = dplyr::case_when(.$s1q3 == 1 ~ 1, is.na(.$s1q3) ~ NA_real_, TRUE ~ 0)
  ) %>%
  dplyr::group_by(hhid_use) %>%
  dplyr::mutate(hhhead_female_use = dplyr::case_when(sum(male_use == 0 & hhhead_use == 1, na.rm = TRUE) > 0 ~ 1, TRUE ~ 0)) %>%
  dplyr::mutate(hhhead_muslim_use = dplyr::case_when(sum(muslim_use == 0 & hhhead_use == 1, na.rm = TRUE) > 0 ~ 1, TRUE ~ 0)) %>%
  dplyr::mutate(hhhead_age_use = sum(dplyr::case_when(hhhead_use == 1 ~ s1q4, TRUE ~ 0), na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(., dataset_2018$sect1_harvestw4 %>% dplyr::filter(s1q4a == 1) %>% dplyr::count(hhid_use), by = "hhid_use") %>%
  dplyr::rename(hhsize_use = n)

# education
dataset_2018$sect2_harvestw4 %<>%
  dplyr::mutate(literacy = dplyr::case_when(s2aq5 == 1 ~ 1, s2aq5 == 2 ~ 0, TRUE ~ NA_real_),
                educ_pri = dplyr::case_when((s2aq9 == 16)|(s2aq9 == 21)|(s2aq9 == 22) ~ 1,  (s2aq9 == 51)|(s2aq9 == 52)|(s2aq9 == 27)|(s2aq9 == 28) ~ NA_real_, is.na(s2aq9) ~ NA_real_, TRUE ~ 0),
                educ_sec = dplyr::case_when((s2aq9 == 23)|(s2aq9 == 24)|(s2aq9 == 25) ~ 1,  (s2aq9 == 51)|(s2aq9 == 52)|(s2aq9 == 27)|(s2aq9 == 28) ~ NA_real_, is.na(s2aq9) ~ NA_real_, TRUE ~ 0),
                educ_high = dplyr::case_when(s2aq9 == 26 ~ 1, (s2aq9 == 51)|(s2aq9 == 52)|(s2aq9 == 27)|(s2aq9 == 28) ~ NA_real_,  is.na(s2aq9) ~ NA_real_, TRUE ~ 0),
                educ_vocation = dplyr::case_when((s2aq9 == 31)|(s2aq9 == 321)|(s2aq9 == 322)|(s2aq9 == 33)|(s2aq9 == 34)|(s2aq9 == 35) ~ 1, is.na(s2aq9) ~ NA_real_, (s2aq9 == 51)|(s2aq9 == 52)|(s2aq9 == 27)|(s2aq9 == 28) ~ NA_real_, TRUE ~ 0),
                educ_col = dplyr::case_when((s2aq9 == 41)|(s2aq9 == 411)|(s2aq9 == 412)|(s2aq9 == 421)|(s2aq9 == 422)|(s2aq9 == 423)|(s2aq9 == 424)|(s2aq9 == 43) ~ 1, is.na(s2aq9) ~ NA_real_, (s2aq9 == 51)|(s2aq9 == 52)|(s2aq9 == 27)|(s2aq9 == 28) ~ NA_real_, TRUE ~ 0),
                cons_toteduc_day = dplyr::case_when(s2aq23 == 2 ~ s2aq23bt / 365, 
                                                s2aq23 == 1 ~ apply(dataset_2018$sect2_harvestw4[stringi::stri_detect_regex(colnames(dataset_2018$sect2_harvestw4), "s2aq23a.*")], MARGIN = 1, FUN = function(x) sum(x, na.rm = TRUE)) / 30 +
                                                                  apply(dataset_2018$sect2_harvestw4[stringi::stri_detect_regex(colnames(dataset_2018$sect2_harvestw4), "s2aq23b.*")], MARGIN = 1, FUN = function(x) sum(x, na.rm = TRUE)) / 365,
                                                TRUE ~ NA_real_
                                                )
  )
# educをまーじ
dataset_2018$sect1_harvestw4 %<>% 
  dplyr::left_join(., dataset_2018$sect2_harvestw4 %>% dplyr::select(pid_use, literacy, educ_pri, educ_sec, educ_high, educ_vocation, educ_col, cons_toteduc_day), by = "pid_use") %>%
  dplyr::group_by(hhid_use) %>%
  dplyr::mutate(hh_head_literacy_use = sum(dplyr::case_when(hhhead_use == 1 ~ literacy, hhhead_use != 1  ~ 0, TRUE ~ NA_real_), na.rm = TRUE),
                hh_head_educ_pri_use = sum(dplyr::case_when(hhhead_use == 1 ~ educ_pri, hhhead_use != 1  ~ 0, TRUE ~ NA_real_), na.rm = TRUE),
                hh_head_educ_sec_use = sum(dplyr::case_when(hhhead_use == 1 ~ educ_sec, hhhead_use != 1  ~ 0, TRUE ~ NA_real_), na.rm = TRUE),
                hh_head_educ_high_use = sum(dplyr::case_when(hhhead_use == 1 ~ educ_high, hhhead_use != 1  ~ 0, TRUE ~ NA_real_), na.rm = TRUE),
                hh_head_educ_vocation_use = sum(dplyr::case_when(hhhead_use == 1 ~ educ_vocation, hhhead_use != 1  ~ 0, TRUE ~ NA_real_), na.rm = TRUE),
                hh_head_educ_col_use = sum(dplyr::case_when(hhhead_use == 1 ~ educ_col, hhhead_use != 1  ~ 0, TRUE ~ NA_real_), na.rm = TRUE),
                cons_totoeduc_day_use = sum(cons_toteduc_day, na.rm = TRUE)
                ) %>%
    dplyr::ungroup()

# labor dataset_2015$sect3a_harvestw3
dataset_2018$sect3a_harvestw4 %<>%
  dplyr::group_by(hhid_use) %>%
  dplyr::mutate(hh_work_employee_use = sum(dplyr::case_when(s3q4 == 1 ~ 1, s3q4 == 2 ~ 0, TRUE ~ NA_real_), na.rm = TRUE),
                hh_work_farm_use = sum(dplyr::case_when(s3q5 == 1 ~ 1, s3q5 == 2 ~ 0, TRUE ~ NA_real_), na.rm = TRUE),
                hh_work_business_use = sum(dplyr::case_when(s3q6 == 1 ~ 1, s3q6 == 2 ~ 0, TRUE ~ NA_real_), na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(hhid_use, .keep_all = TRUE)


# eatout消費
dataset_2018$sect10a_harvestw4 %<>%
  dplyr::mutate(s10aq2 = dplyr::case_when(s10aq1 == 2 ~ 0, s10aq1 == 1 ~ s10aq2, TRUE ~ NA_real_)) %>%
  dplyr::group_by(hhid_use) %>%
  dplyr::mutate(cons_eatout_h_use = sum(s10aq2, na.rm = TRUE) /7) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(hhid_use, .keep_all = TRUE)
dataset_2018$sect7a_plantingw4 %<>%
  dplyr::mutate(s7aq2 = dplyr::case_when(s7aq1 == 2 ~ 0, s7aq1 == 1 ~ s7aq2, TRUE ~ NA_real_)) %>%
  dplyr::group_by(hhid_use) %>%
  dplyr::mutate(cons_eatout_p_use = sum(s7aq2, na.rm = TRUE)  / 7) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(hhid_use, .keep_all = TRUE)


# food消費
dataset_2018$sect7b_plantingw4 %<>%
  dplyr::full_join(.,dataset_2018$sect10b_harvestw4, by = c("hhid_use", "item_cd", "zone", "state", "lga", "ea") ) %>%
  dplyr::mutate(p0 = s7bq10 / (s7bq9_cvn*s7bq9a),
                p1 = s10bq10 / (s10bq9_cvn*s10bq9a),
                q0 = s10bq2a*s10bq2_cvn,
                q1 = s7bq2a*s7bq2_cvn
                ) %>%
  dplyr::mutate(relative_price = p1 / p0) %>%
  dplyr::group_by(hhid_use) %>%
  dplyr::mutate(h_idx = sum(p1 * q0, na.rm = TRUE) / sum(p0 * q0, na.rm = TRUE),
                cons_food_p_use = sum(p0*q0, na.rm = TRUE) / 7,
                cons_food_h = sum(p1*q1, na.rm = TRUE)
                ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(h_idx_use = dplyr::case_when((h_idx > 0) & (!is.na(h_idx)) & (!is.infinite(h_idx)) & (!is.nan(h_idx)) ~ h_idx,
                                               TRUE ~ NA_real_
                                               )
                ) %>%
  dplyr::mutate(cons_food_h_use = cons_food_h / h_idx_use / 7) %>%
  dplyr::mutate(category = dplyr::case_when(item_cd>=10 & item_cd <= 23 ~ "grain_flour",
                                            item_cd >= 25 & item_cd<=29 ~ "processed",
                                            item_cd >= 30 & item_cd<=38 ~ "plantain",
                                            item_cd >= 40 & item_cd<=48 ~ "seed",
                                            item_cd >= 50 & item_cd<=56 ~ "oil",
                                            (item_cd >= 60 & item_cd<=69)|(item_cd == 601) ~ "fruit",
                                            item_cd >= 70 & item_cd<=79 ~ "vegetable",
                                            item_cd >= 80 & item_cd<=85 ~ "poultry",
                                            item_cd >= 90 & item_cd<=96 ~ "meat",
                                            item_cd >= 100 & item_cd<=107 ~ "seafood",
                                            item_cd >= 110 & item_cd<=115 ~ "dairy",
                                            item_cd >= 120 & item_cd<=122 ~ "like_beverage",
                                            item_cd >= 130 & item_cd<=133 ~ "confectionary",
                                            item_cd >= 141 & item_cd<=148 ~ "others",
                                            item_cd >= 150 & item_cd<=155 ~ "non-alco",
                                            item_cd >= 160 & item_cd<=174 ~ "alco"
  ))
dataset_2018 = dataset_2018[!stringi::stri_detect_regex(names(dataset_2018), "sect10b_harvestw4")]



# dataset_2018$sect10c_harvestw4: 他の項目で聞いているfoodなので不要
# dataset_2018$sect10ca_harvestw3: w3のdatasetなのにw4に混ざっている

dataset_2018$sect11_plantingw4 %<>%
  dplyr::mutate(.,
                houseowner_use = dplyr::case_when((.$s11q1 == 1) ~ 1, is.na(.$s11q1) ~ .$s11q1, TRUE ~ 0),
                wall_use =  dplyr::case_when(.$s11q6 == 5 ~ 1, is.na(.$s11q6) ~ .$s11q6, TRUE ~ 0), #cement or concrete
                roof_use = dplyr::case_when((.$s11q7 == 2) ~ 1, is.na(.$s11q7) ~ .$s11q7, TRUE ~ 0), #ironsheetが74%
                floor_use = dplyr::case_when((.$s11q8 == 3) ~ 1, is.na(.$s11q8) ~ .$s11q8, TRUE ~ 0), #3cementが68%, 2mudが14％
                electricity_use = dplyr::case_when(.$s11q47 == 1 ~ 1, .$s11q47 == 2 ~ 0, TRUE ~ NA_real_), 
                rooms_use = dplyr::case_when(!is.na(.$s11q9) ~ .$s11q9, TRUE ~ NA_real_), 
                pipewater_use = dplyr::case_when(((.$s11q33b == 1)|(.$s11q33b == 2)) ~ 1, is.na(.$s11q33b) ~ .$s11q33b, TRUE ~ 0), # pip water
  ) 

# Non-Food Expenditures - past 7 days
dataset_2018$sect11a_harvestw4 %<>%
  dplyr::mutate(., cons = dplyr::case_when(s11aq1 == 2 ~ 0, s11aq1 == 1 ~ s11aq2, TRUE ~ NA_real_)) %>%
  dplyr::group_by(hhid_use) %>%
  dplyr::mutate(cons_nonfood1_h_use = sum(cons, na.rm = TRUE)/ 7) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(hhid_use, .keep_all = TRUE)

dataset_2018$sect8a_plantingw4 %<>%
  dplyr::mutate(., cons = dplyr::case_when(s8q1 == 2 ~ 0, s8q1 == 1 ~ s8q2, TRUE ~ NA_real_)) %>%
  dplyr::group_by(hhid_use) %>%
  dplyr::mutate(cons_nonfood1_p_use = sum(cons, na.rm = TRUE) / 7) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(hhid_use, .keep_all = TRUE)

# Non-Food Expenditures - past 30 days
dataset_2018$sect11b_harvestw4 %<>%
  dplyr::mutate(., cons = dplyr::case_when(s11bq3 == 2 ~ 0, s11bq3 == 1 ~ s11bq4, TRUE ~ NA_real_)) %>%
  dplyr::group_by(hhid_use) %>%
  dplyr::mutate(cons_nonfood2_h_use = sum(cons, na.rm = TRUE) / 30) %>%
  dplyr::ungroup()%>%
  dplyr::distinct(hhid_use, .keep_all = TRUE)
dataset_2018$sect8b_plantingw4 %<>%
  dplyr::mutate(., cons = dplyr::case_when(s8q3 == 2 ~ 0, s8q3 == 1 ~ s8q4, TRUE ~ NA_real_)) %>%
  dplyr::group_by(hhid_use) %>%
  dplyr::mutate(cons_nonfood2_p_use = sum(cons, na.rm = TRUE) / 30) %>%
  dplyr::ungroup()%>%
  dplyr::distinct(hhid_use, .keep_all = TRUE)

# Non-Food Expenditures - 6 months
dataset_2018$sect11c_harvestw4 %<>% 
  dplyr::mutate(., cons = dplyr::case_when(s11cq5 == 2 ~ 0, s11cq5 == 1 ~ s11cq6, TRUE ~ NA_real_)) %>%
  dplyr::group_by(hhid_use) %>%
  dplyr::mutate(cons_nonfood3_h_use = sum(cons, na.rm = TRUE) / (6*30)) %>%
  dplyr::ungroup()%>%
  dplyr::distinct(hhid_use, .keep_all = TRUE)
dataset_2018$sect8c_plantingw4 %<>% 
  dplyr::mutate(., cons = dplyr::case_when(s8q5 == 2 ~ 0, s8q5 == 1 ~ s8q6, TRUE ~ NA_real_)) %>%
  dplyr::group_by(hhid_use) %>%
  dplyr::mutate(cons_nonfood3_p_use = sum(cons, na.rm = TRUE) / (6*30)) %>%
  dplyr::ungroup()%>%
  dplyr::distinct(hhid_use, .keep_all = TRUE)

# Non-Food Expenditures - 12 months
dataset_2018$sect11d_harvestw4 %<>%
  dplyr::mutate(., 
                cons = dplyr::case_when(s11dq7 == 2 ~ 0, TRUE ~ s11dq8)
  ) %>%
  dplyr::group_by(hhid_use) %>%
  dplyr::mutate(cons_nonfood4_day_use = sum(cons, na.rm = TRUE) / 365) %>%
  dplyr::ungroup()%>%
  dplyr::distinct(hhid_use, .keep_all = TRUE)



# animal
animal_list = as.character(unique(dataset_2018$sect11i_plantingw4$animal_cd))
names(animal_list) = paste("animal_cd", unique(dataset_2018$sect11i_plantingw4$animal_cd), "use",sep = "_")

dataset_2018$sect11i_plantingw4 %<>%
  dplyr::mutate(., s11iq2a = ifelse(.$s11iq1 == 2, 0, .$s11iq2a)) %>%
  dplyr::select(., c(hhid_use, comid_use, animal_cd, s11iq2a)) %>%
  tidyr::pivot_wider(., names_from = animal_cd, values_from = s11iq2a) %>%
  rename_var(., animal_list)

for (i in names(animal_list)) {
  dataset_2018$sect11i_plantingw4  %<>% 
    dplyr::mutate(!!paste("dummy", i, sep = "_") := dplyr::case_when(.[[i]] > 0 ~ 1, .[[i]] == 0 ~ 0, TRUE ~ NA_real_),
                  !!paste("std", i, sep = "_") := (.[[i]] - mean(.[[i]], na.rm = TRUE)) / sd(.[[i]], na.rm = TRUE)
    )
}

### health
#dataset_2018$sect4a_harvestw4 %<>%
#  dplyr::mutate(sick = dplyr::case_when(s4aq3 == 1 ~ 1, s4aq3 == 2 ~ 0, TRUE ~ NA_real_),
#                cons_health_consul = dplyr::case_when(s4aq3 == 2 ~ 0, s4aq3 == 1 ~ s4aq9, TRUE ~ NA_real_),
#                cons_health_trip = dplyr::case_when(s4aq3 == 2 ~ 0, s4aq3 == 1 ~ s4aq10, TRUE ~ NA_real_),
#                cons_health_drug = dplyr::case_when(s4aq13 == 2 ~ 0, s4aq13 == 1 ~ s4aq14, TRUE ~ NA_real_),
#                cons_health_hospital = dplyr::case_when(s4aq15 == 2 ~ 0, s4aq15 == 1 ~ s4aq17, TRUE ~ NA_real_)
#  ) %>%
#  dplyr::group_by(hhid_use) %>%
#  dplyr::mutate(hh_sick_use = sum(sick, na.rm = TRUE),
#                cons_health_consul_day_use = sum(cons_health_consul, na.rm = TRUE) / (7*4),
#                cons_health_trip_day_use = sum(cons_health_trip, na.rm = TRUE) / (7*4),
#                cons_health_drug_day_use = sum(cons_health_drug, na.rm = TRUE) / (7*4),
#                cons_health_hospital_day_use = sum(cons_health_hospital, na.rm = TRUE) / 365
#  ) %>%
#  dplyr::ungroup() %>%
#  dplyr::distinct(hhid_use, .keep_all = TRUE)

#snet: sect14b_harvestw4のsnet infoを使う
dataset_2018$sect14a_harvestw4 %<>%
  dplyr::mutate(.,
                trnsfr_cash_dummy_use = dplyr::case_when(s14q1a__1 == 1 ~ 1, s14q1a__1 == 2 ~ 0, TRUE ~ NA_real_),
                trnsfr_food_dummy_use = dplyr::case_when(s14q1a__2 == 1 ~ 1, s14q1a__2 == 2 ~ 0, TRUE ~ NA_real_),
                trnsfr_inkind_dummy_use = dplyr::case_when(s14q1a__3 == 1 ~ 1, s14q1a__3 == 2 ~ 0, TRUE ~ NA_real_)
  ) %>%
  dplyr::group_by(hhid_use) %>%
  dplyr::mutate(trnsfr_any_dummy_use = dplyr::case_when((trnsfr_cash_dummy_use > 0) |(trnsfr_food_dummy_use > 0 )| (trnsfr_inkind_dummy_use > 0) ~ 1, TRUE ~ 0)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(hhid_use, .keep_all =TRUE)

  
# snet amount
dataset_2018$sect14b_harvestw4 %<>%
       dplyr::mutate(.,
                     trnsfr_cash_pre_use = dplyr::case_when(s14q2a == 2 ~ 0, TRUE ~ s14q2a),
                     trnsfr_food_pre_use = dplyr::case_when(s14q2d == 2 ~ 0, TRUE ~ s14q2d),
                     trnsfr_inkind_pre_use = dplyr::case_when(s14q2f == 2 ~ 0, TRUE ~ s14q2f)
       ) %>%
  dplyr::group_by(hhid_use) %>%
    dplyr::mutate(.,
                  trnsfr_cash_pre_use = sum(trnsfr_cash_pre_use, na.rm = TRUE),
                  trnsfr_food_pre_use = sum(trnsfr_food_pre_use, na.rm = TRUE),
                  trnsfr_inkind_pre_use = sum(trnsfr_inkind_pre_use, na.rm = TRUE)
      ) %>%
  dplyr::mutate(trnsfr_cash_dummy_pre_use = dplyr::case_when(sum(trnsfr_cash_pre_use, na.rm = TRUE) > 0 ~ 1, TRUE ~ 0),
                trnsfr_food_dummy_pre_use = dplyr::case_when(sum(trnsfr_food_pre_use, na.rm = TRUE) > 0 ~ 1, TRUE ~ 0),
                trnsfr_inkind_dummy_pre_use = dplyr::case_when(sum(trnsfr_inkind_pre_use, na.rm = TRUE) > 0 ~ 1, TRUE ~ 0)
                ) %>%
  dplyr::mutate(trnsfr_any_dummy_pre_use = dplyr::case_when((trnsfr_cash_dummy_pre_use > 0) |(trnsfr_food_dummy_pre_use > 0 )| (trnsfr_inkind_dummy_pre_use > 0) ~ 1, TRUE ~ 0)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(hhid_use, .keep_all =TRUE)

# 先に14aと14bは接続しないといけない
dataset_2018$sect14a_harvestw4 %<>% 
  dplyr::left_join(., dataset_2018$sect14b_harvestw4, by = "hhid_use") %>%
  dplyr::mutate(trnsfr_cash_use = dplyr::case_when(trnsfr_cash_dummy_use == 0 ~ 0, trnsfr_cash_dummy_use == 1 ~ trnsfr_cash_pre_use, TRUE ~ NA_real_),
                trnsfr_food_use = dplyr::case_when(trnsfr_food_dummy_use == 0 ~ 0, trnsfr_food_dummy_use == 1 ~ trnsfr_food_pre_use, TRUE ~ NA_real_),
                trnsfr_inkind_use = dplyr::case_when(trnsfr_inkind_dummy_use == 0 ~ 0, trnsfr_inkind_dummy_use == 1 ~ trnsfr_inkind_pre_use, TRUE ~ NA_real_))

# disaster: sectc6_harvestを使用
#dataset_2018$sect15a_harvestw4 %<>%
#  dplyr::group_by(hhid_use) %>%
#  dplyr::mutate(nd_flood_use = dplyr::case_when(sum(shock_cd == 13 & s15aq1 == 1, na.rm = TRUE) > 0 ~ 1, TRUE ~ 0),
#                nd_pest_use = dplyr::case_when(sum(shock_cd == 14 & s15aq1 == 1, na.rm = TRUE) > 0 ~ 1, TRUE ~ 0),
#                nd_drought_use = dplyr::case_when(sum(shock_cd == 12 & s15aq1 == 1, na.rm = TRUE) > 0 ~ 1, TRUE ~ 0)
#  ) %>%
#  dplyr::ungroup()

# dataset_2018$sect3_plantingw4: sect3_plantingw4を使用
dataset_2018$sect3_plantingw4 %<>%
  dplyr::mutate(., 
                elite_public_use = dplyr::case_when((.$s3q4 == 1) & (.$s3q14 == 13) ~ 1, is.na(.$s3q4) ~ .$s3q4, TRUE ~ 0),
                elite_lc_gov_use  =  dplyr::case_when((.$s3q4 == 1) & (.$s3q15 == 3) ~ 1, is.na(.$s3q4) ~ .$s3q4, TRUE ~ 0),
                elite_gov_use  = dplyr::case_when((.$s3q4 == 1) & ((.$s3q15 == 3)|(.$s3q15 == 1)|(.$s3q15 == 2)) ~ 1, is.na(.$s3q4) ~ .$s3q4, TRUE ~ 0),
  ) %>%
  dplyr::mutate(., elite_con_use  = dplyr::case_when((.$elite_public_use == 0) & (.$elite_gov_use == 1) ~ 1, is.na(.$elite_gov_use) ~ .$elite_gov_use, TRUE ~ 0),) %>%
  dplyr::group_by(hhid_use) %>%
  dplyr::mutate(elite_public_use = sum(elite_public_use, na.rm = TRUE),
                elite_lc_gov_use = sum(elite_lc_gov_use, na.rm = TRUE),
                elite_gov_use = sum(elite_gov_use, na.rm = TRUE),
                elite_con_use = sum(elite_con_use, na.rm = TRUE)
                ) %>%
  dplyr::ungroup()

# asset
asset_list = as.character(unique(dataset_2018$sect5_plantingw4$item_cd))
names(asset_list) = paste("asset_cd", unique(dataset_2018$sect5_plantingw4$item_cd), "use", sep = "_")

dataset_2018$sect5_plantingw4 %<>% 
  dplyr::select(., c(hhid_use, comid_use, item_cd, s5q1a, s5q1)) %>%
  dplyr::mutate(s5q1 = dplyr::case_when(s5q1a == 1 ~ s5q1, s5q1a == 2 ~ 0, TRUE ~ NA_real_)) %>%
  tidyr::pivot_wider(., id_cols = c("hhid_use", "comid_use"), names_from = item_cd, values_from = s5q1) %>%
  rename_var(., asset_list)

for (i in names(asset_list)) {
  dataset_2018$sect5_plantingw4  %<>% 
    dplyr::mutate(!!paste("dummy", i, sep = "_") := dplyr::case_when(.[[i]] > 0 ~ 1, .[[i]] == 0 ~ 0, TRUE ~ NA_real_),
                  !!paste("std", i, sep = "_") := (.[[i]] - mean(.[[i]], na.rm = TRUE)) / sd(.[[i]], na.rm = TRUE)
    )
}

#dataset_2018$sect9_plantingw4 %<>% # あまり使い道がわからないので一旦保留 
#  dplyr::mutate(foodsec_runout_use = dplyr::case_when(s9q8f == 1 ~ 1, s9q8f == 2 ~ 0, TRUE ~ NA_real_),
#                foodsec_hunger_use = dplyr::case_when(s9q8g == 1 ~ 1, s9q8g == 2 ~ 0, TRUE ~ NA_real_),
#                foodsec_noteat_use = dplyr::case_when(s9q8h == 1 ~ 1, s9q8h == 2 ~ 0, TRUE ~ NA_real_)
#                )

# urban
dataset_2018$sectc_plantingw4 %<>%
  dplyr::mutate(urban_use = dplyr::case_when(sector == 1 ~ 1, sector == 2 ~ 0, TRUE ~ NA_real_))
  

# food price
#dataset_2018$sectc2_plantingw4 %<>%
#  dplyr::select(., c(comid_use, item_cd, c2q1, c2q3)) %>%
#  dplyr::mutate(., food_cd_use = paste(comid_use, item_cd, sep = "_")) %>%
#  dplyr::rename(., pfood_use = c2q3)
  
# dataset_2018$sectc3_harvestw4: 個人と紐付けできない

# disaster
dataset_2018$sectc6_harvestw4 %<>%
  dplyr::group_by(comid_use) %>%
  dplyr::mutate(nd_drought_use = dplyr::case_when(sum(event_cd == 1 & c6q1 == 1, na.rm = TRUE) > 0 ~ 1, TRUE ~ 0),
                nd_flood_use = dplyr::case_when(sum(event_cd == 2 & c6q1 == 1, na.rm = TRUE) > 0 ~ 1, TRUE ~ 0),
                nd_pest_use = dplyr::case_when(sum(event_cd == 3 & c6q1 == 1, na.rm = TRUE) > 0 ~ 1, TRUE ~ 0),#crop disease / pest
                nd_livestock_use = dplyr::case_when(sum(event_cd == 4 & c6q1 == 1, na.rm = TRUE) > 0 ~ 1, TRUE ~ 0),
                nd_epi_use = dplyr::case_when(sum(event_cd == 5 & c6q1 == 1, na.rm = TRUE) > 0 ~ 1, TRUE ~ 0),
  ) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(comid_use, .keep_all = TRUE)

# dataset_2018$sectc8_harvestw4: sectc2_plantingw4でfood priceデータは取得している







### 2020
## function
# demographic
clean_sect_1 = function(data){
  df =  data %>%
    dplyr::mutate(urban_use = dplyr::case_when(
      stringi::stri_detect_regex(.$sector, ".*Urban.*") ~ 1, stringi::stri_detect_regex(.$sector, ".*Rural*") ~ 0, TRUE ~ NA_real_
    )) %>%
    dplyr::distinct(hhid_use, .keep_all = TRUE)
  return(df)
}
# network
clean_sect_10 = function(data){
  list_shock = list(shock_sickness_use = 1, 
                    shock_jobloss_use = 5,
                    shock_business_use = 6,
                    shock_theft_use = 7,
                    shock_agri_use = 8,
                    shock_input_use = 10,
                    shock_output_use = 11,
                    shock_inf_use = 12
                    )
  df = data %>%
    dplyr::group_by(hhid_use) %>%
    dplyr::mutate(share_use = dplyr::case_when(sum(s10q3__7 == 1, na.rm = TRUE) > 0 ~ 1, TRUE ~ 0))
  for (i in names(list_shock)) {
    pattern = paste0(list_shock[[i]], ".*")
    df %<>% dplyr::group_by(hhid_use) %>%
      dplyr::mutate(!!i := dplyr::case_when(sum(stringi::stri_detect_regex(s10q1, "1.*") & stringi::stri_detect_regex(shock_cd, pattern), na.rm = TRUE) > 0 ~ 1, 
                                            sum(stringi::stri_detect_regex(s10q1, "2.*") & stringi::stri_detect_regex(shock_cd, pattern), na.rm = TRUE) > 0 ~ 0, 
                                            TRUE ~ NA_real_)
      ) %>%
      dplyr::ungroup()
  }
  df %<>% dplyr::distinct(hhid_use, .keep_all = TRUE)
  return(df)
}
# aid since mid march
clean_sect_11 = function(data){
  df = data %>%
    dplyr::group_by(hhid_use) %>%
    dplyr::mutate(trnsfr_food_dummy_use = dplyr::case_when(sum(stringi::stri_detect_regex(assistance_cd, ".*1.*") & stringi::stri_detect_regex(s11q1, ".*1.*"), na.rm = TRUE) > 0 ~ 1, TRUE ~ 0),
                  trnsfr_cash_dummy_use = dplyr::case_when(sum(stringi::stri_detect_regex(assistance_cd, ".*2.*") & stringi::stri_detect_regex(s11q1, ".*1.*"), na.rm = TRUE) > 0 ~ 1,TRUE ~  0),
                  trnsfr_inkind_dummy_use = dplyr::case_when(sum(stringi::stri_detect_regex(assistance_cd, ".*3.*") & stringi::stri_detect_regex(s11q1, ".*1.*"), na.rm = TRUE) > 0 ~ 1,TRUE ~  0)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate( trnsfr_any_dummy_use = dplyr::case_when((trnsfr_food_dummy_use == 1)|(trnsfr_cash_dummy_use == 1)|(trnsfr_inkind_dummy_use == 1) ~ 1, TRUE ~ 0),
                   assistance_cd_2 = dplyr::case_when(stringi::stri_detect_regex(assistance_cd, ".*1.*") ~ "food_use",
                                                      stringi::stri_detect_regex(assistance_cd, ".*2.*") ~ "cash_use",
                                                      stringi::stri_detect_regex(assistance_cd, ".*3.*") ~ "inkind_use",
                                                      TRUE ~ NA_character_
                                                      ),
                   value = dplyr::case_when(stringi::stri_detect_regex(s11q1, ".*2.*") ~ 0,
                                            stringi::stri_detect_regex(s11q1, ".*1.*") ~ s11q2, #s11q2: total value
                                            TRUE ~ NA_real_
                                            )
                   ) %>%
    tidyr::pivot_wider(names_prefix = "trnsfr_", names_from = assistance_cd_2, values_from = value, id_cols = c("hhid_use","comid_use","zone","state","lga","ea","hhid","trnsfr_food_dummy_use","trnsfr_cash_dummy_use","trnsfr_inkind_dummy_use", "trnsfr_any_dummy_use")) %>%
    dplyr::distinct(hhid_use, .keep_all = TRUE)
  return(df)
}

clean_sect_long = function(data){
  df = data %>%
    dplyr::mutate(elite_public_use = dplyr::case_when(stringi::stri_detect_regex(.$s6q5, ".*8.*") ~ 1, TRUE ~ 0))
    
  return(df)
}

## round1
# demographich
dataset_2020$r1_sect_1 %<>% clean_sect_1(.)
# network
dataset_2020$r1_sect_10 %<>% clean_sect_10(.)
# aid since mid march
dataset_2020$r1_sect_11 %<>% clean_sect_11(.)
# elite
dataset_2020$r1_sect_a_3_4_5_6_8_9_12 %<>% clean_sect_long(.)

## round2
# demographic
dataset_2020$r2_sect_1 %<>% clean_sect_1(.)
# aid since mid march
dataset_2020$r2_sect_11 %<>% clean_sect_11(.)
# elite
dataset_2020$r2_sect_a_2_5_6_8_12 %<>% clean_sect_long(.)

## round3
dataset_2020$r3_sect_1 %<>% clean_sect_1(.)
dataset_2020$r3_sect_10 %<>% clean_sect_10(.)
dataset_2020$r3_sect_11 %<>% clean_sect_11(.)
dataset_2020$r3_sect_a_2_5_5a_6_12 %<>% clean_sect_long(.)

# round4
dataset_2020$r4_sect_1 %<>% clean_sect_1(.)
dataset_2020$r4_sect_11 %<>% clean_sect_11(.)
dataset_2020$r4_sect_a_2_5_5b_6_8_9_12 %<>% clean_sect_long(.)



### widerにしてidで照合するのは最後で良い。他のファイルでいいかも。それまではlongで持ってた方がdplyr::group_byで編集の加工しやすい
#mutate_widedata = function(dataset, wide_list){
#  file = names(wade_list)
#  dataset[[file]] %<>% tidyr::pivot_wider(data = ., id_cols = wide_list[[file]][[1]], names_from = wide_list[[file]][[2]], values_from = wide_list[[file]][[3]])
#  return(data)
#}
#tidyr::pivot_wider(data = dataset_2015[["15_plantingw3"]], id_cols = wide_list_2015[["15_plantingw3"]][[1]], names_from = wide_list_2015[["sect5_plantingw3"]][[2]], values_from = unlist(wide_list_2015[["sect5_plantingw3"]][[3]]))
#wide_list_2015 = list(sect5_plantingw3 = list("hhid_use", "item_cd", list("s5q1", "s5q4")),
#                      sect11i_plantingw3 = list("hhid_use","animal_cd", list("s11iq2", "s11iq2", "s11iq3")),
#                      sect10c_harvestw3 = list("hhid_use",  "item_cd","s10cq8"),
#                      sect10b_harvestw3 = list("hhid_use",  "item_cd",list("s10bq1", "s10bq4")),
#                      sect10a_harvestw3 = list("hhid_use",  "item_cd",list("s10aq1", "s10aq2")),
#                      sect11a_harvestw3 = list("hhid_use",  "item_cd",list("s11aq1", "s11aq2")),
#                      sect11b_harvestw3 = list("hhid_use",  "item_cd",list("s11bq3", "s11bq4") ),
#                      sect11c_harvestw3 = list("hhid_use",  "item_cd",list("s11cq5", "s11cq6")  ),
#                      sect11d_harvestw3 = list("hhid_use",  "item_cd",list("s11dq7", "s11dq8")  ),
#                      sect11e_harvestw3 = list("hhid_use",  "item_cd",list("s11eq9", "s11eq11")  ),
#                      sect14_harvestw3 =  list("hhid_use", "snet_cd", list("s14q1", "s14q2a", "s14q2d", "s14q2e")),
#                      sect15a_harvestw3 = list("hhid_use",  "shock_cd", "s15aq1") 
#                      )
#id_list = c("pid_use", "hhid_use", "comid_use")
#dataset_2015_byid = list()
#names(dataset_2015)
#for (i in id_list) { # idのタイプ毎にデータを突合
#  if(i == "pid_use"){
#    df = purrr::reduce(dataset_2015 %>% purrr::keep( ~sum(stringi::stri_detect_regex(colnames(.x), i)) > 0), 
#                       dplyr::full_join, 
#                       by = i
#    )
#  } else if(i == "hhid_use"){ 
#    df = purrr::reduce(dataset_2015 %>% 
#                         purrr::keep( ~sum(stringi::stri_detect_regex(colnames(.x), "pid_use")) < 1 & ~sum(stringi::stri_detect_regex(colnames(.x), i)) > 0),
#                       dplyr::full_join, 
#                       by = i
#    )
#  } else {
    # 同じhhidで複数行ある可能性
    # ectc1_plantingw3 pid
    # sectc2a_plantingw3 item_cd
    # sectc6_harvestw3 event_cd
    # sectc1_harvestw3 serial_no
    # sectc3_harvestw3 group_id
    # sectc8a_harvestw3 item_cd
    # secta10_harvestw3 sa10q2 network_cd
    # sect12_plantingw3 s12q2
    #df = purrr::reduce(dataset_2015 %>% 
    #                     purrr::keep( ~sum(stringi::stri_detect_regex(colnames(.x), "(pid_use)|(hhid_use)")) < 1 & ~sum(stringi::stri_detect_regex(colnames(.x), i)) > 0), 
    #                   dplyr::full_join, 
    #                   by = "com_id"
    #)
#    next
#  }
#  dataset_2015_byid[[i]] = df
#}
print("done: clean.R")

