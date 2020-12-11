#"""
#simulate
#"""
library(ggplot2)
  
## step  
# 2018援助額
a = dataset_2018_all %>% dplyr::select(c(trnsfr_cash_use, trnsfr_food_use, trnsfr_inkind_use)) %>%
  dplyr::mutate(tottrnsfr = apply(., MARGIN = 1, FUN = function(x) sum(x, na.rm = TRUE))) %>%
  dplyr::mutate(trnsfr_cash_use = ifelse(trnsfr_cash_use == 0, NA, trnsfr_cash_use),
                trnsfr_food_use = ifelse(trnsfr_food_use == 0, NA, trnsfr_food_use),
                trnsfr_inkind_use = ifelse(trnsfr_inkind_use == 0, NA, trnsfr_inkind_use),
                tottrnsfr = ifelse(tottrnsfr == 0, NA, tottrnsfr)
                ) %>%
  tidyr::pivot_longer(., cols = c(trnsfr_cash_use, trnsfr_food_use, trnsfr_inkind_use, tottrnsfr), names_to = "transfer", values_to = "amount")
g = ggplot(a, aes(x = amount, y = transfer)) +
  geom_boxplot() +
  scale_y_discrete(labels = c("total amount", "cash", "food", "inkind")) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=10, color="grey") 
plot(g)

a %>% dplyr::group_by(transfer) %>%
  dplyr::summarise(
                   mean = mean(amount, na.rm = TRUE)
                   )
# 2015援助額

a = dataset_2020_r1_all %>% dplyr::select(c(trnsfr_cash_use, trnsfr_food_use, trnsfr_inkind_use)) %>%
  dplyr::mutate(tottrnsfr = apply(., MARGIN = 1, FUN = function(x) sum(x, na.rm = TRUE))) %>%
  dplyr::mutate(trnsfr_cash_use = ifelse(trnsfr_cash_use == 0, NA, trnsfr_cash_use),
                trnsfr_food_use = ifelse(trnsfr_food_use == 0, NA, trnsfr_food_use),
                trnsfr_inkind_use = ifelse(trnsfr_inkind_use == 0, NA, trnsfr_inkind_use),
                tottrnsfr = ifelse(tottrnsfr == 0, NA, tottrnsfr)
  ) %>%
  tidyr::pivot_longer(., cols = c(trnsfr_cash_use, trnsfr_food_use, trnsfr_inkind_use, tottrnsfr), names_to = "transfer", values_to = "amount")
g = ggplot(a, aes(x = amount, y = transfer)) +
  geom_boxplot() +
  scale_y_discrete(labels = c("total amount", "cash", "food", "inkind")) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=10, color="grey") 
plot(g)

a %>% dplyr::group_by(transfer) %>%
  dplyr::summarise(
    mean = mean(amount, na.rm = TRUE)
  )
mean(dataset_2018_all$cons_food_day_use, na.rm = TRUE)


#crra 応用関数
crra_func = function(percapcons, ganma = 3){
  crra = (percapcons^(1-ganma)) / (1-ganma)
  return(crra)
}


###2020

#推計モデルのエリートキャプチャーあり
df_elitecap = merge(dataset_list$dataset_2020_all, list_est_error$trnsfr_any_dummys2Id$augment$.fitted, by ="row.names")
df_elitecap %<>% 
  dplyr::mutate(utility_ganma2_cash = crra_func((percapcons + y * 20000), ganma = 2),
                utility_ganma3_cash = crra_func((percapcons + y * 20000), ganma = 3),
                utility_ganma5_cash = crra_func((percapcons + y * 20000), ganma = 5) 
                )%>%
                  dplyr::mutate(y_food = ifelse(stringi::stri_detect_regex(df_elitecap$hhid, "(.37.*)|(.27.*)|(.24.*)"), y, 0)
                )%>% 
  dplyr::mutate(
    utility_ganma2_food = crra_func((percapcons + y_food * 43092), ganma = 2),
    utility_ganma3_food = crra_func((percapcons + y_food * 43092), ganma = 3),
    utility_ganma5_food = crra_func((percapcons + y_food * 43092), ganma = 5)
  )
 

sum(df_elitecap$utility_ganma2_cash,na.rm = TRUE)
sum(df_elitecap$utility_ganma3_cash,na.rm = TRUE)
sum(df_elitecap$utility_ganma5_cash,na.rm = TRUE)

sum(df_elitecap$utility_ganma2_food,na.rm = TRUE)
sum(df_elitecap$utility_ganma3_food,na.rm = TRUE)
sum(df_elitecap$utility_ganma5_food,na.rm = TRUE)


g = ggplot2::ggplot(df_elitecap, aes(x = lpercapcons, y = utility_ganma2_cash)) +
  geom_point()+
  ylim(-0.05, 0.01) +
  geom_path(data=data.frame(X=x<-seq(1,5,len=100), Y=x^(1-2) / (1-2))+0.2, aes(x=X,y=Y))
plot(g)

g = ggplot2::ggplot(df_elitecap, aes(x = lpercapcons, y = utility_ganma3_cash)) +
  geom_point()+
  ylim(-0.005, 0.01) +
  geom_path(data=data.frame(X=x<-seq(1,5,len=100), Y=x^(1-3) / (1-3))+0.2, aes(x=X,y=Y))
plot(g)

g = ggplot2::ggplot(df_elitecap, aes(x = lpercapcons, y = utility_ganma5_cash)) +
  geom_point()+
  ylim(-1, 1) 
  #geom_path(data=data.frame(X=x<-seq(1,5,len=1000), Y=x^(1-7)/(1-7)), aes(x=X,y=Y))

plot(p)
p <- ggplot(data=data.frame(X=c(-4,4)), aes(x=X))
p <- p +stat_function(fun=function(x) x^(1-0.5)/(1-0.5))
#推計モデルのエリートキャプチャーなし
df_elitecap %<>% dplyr::mutate(y_without_elitecap = y - elite_lc_gov * (-0.112833071))
df_elitecap %<>% dplyr::mutate(utility_ganma3_cash_without_elitecap = crra_func((percapcons + y_without_elitecap * 20000), ganma = 3),
                              utility_ganma5_cash_without_elitecap = crra_func((percapcons + y_without_elitecap * 20000), ganma = 5),
                              utility_ganma2_cash_without_elitecap = crra_func((percapcons + y_without_elitecap * 20000), ganma = 2)
) %>%
  dplyr::mutate(y_food = ifelse(stringi::stri_detect_regex(df_elitecap$hhid, "(.37.*)|(.27.*)|(.24.*)"), y_without_elitecap, 0)
  )%>% 
  dplyr::mutate(
  utility_ganma2_food_without_elitecap = crra_func((percapcons + y_food * 43092), ganma = 2),
  utility_ganma3_food_without_elitecap = crra_func((percapcons + y_food * 43092), ganma = 3),
  utility_ganma5_food_without_elitecap = crra_func((percapcons + y_food * 43092), ganma = 5)
)

sum(df_elitecap$utility_ganma2_cash_without_elitecap,na.rm = TRUE)
sum(df_elitecap$utility_ganma3_cash_without_elitecap,na.rm = TRUE)
sum(df_elitecap$utility_ganma5_cash_without_elitecap,na.rm = TRUE)

sum(df_elitecap$utility_ganma2_food_without_elitecap,na.rm = TRUE)
sum(df_elitecap$utility_ganma3_food_without_elitecap,na.rm = TRUE)
sum(df_elitecap$utility_ganma5_food_without_elitecap,na.rm = TRUE)

df_elitecap$elite_lc_gov

#PMT基準の受益者選定
mutate_pmt = function(){
  var = list_est_pmt$lpercapcons_s4_Id$estimate$term
  df_use = dataset_list$dataset_2018_all[[var[[!stringi::stri_detect_regex(var, ".*comid.*")]]]]
  
}
rownames(list_est_pmt$lpercapcons_s4_Id$estimate$term[["plot"]])
list_est_pmt$lpercapcons_s4_Id$estimate$estimate[1] + list_est_pmt$lpercapcons_s4_Id$estimate$term["plot"]

lfe::predict(est, newdata = dataset_list$dataset_2018_all)
formula = list_est_pmt$lpercapcons_s4_Id$formula
est = lm(as.formula(formula), data = dataset_list$dataset_2015_all )

pmt = predict(est, newdata = levels(droplevels(dataset_list$dataset_2018_all)))

df_pmt = merge(dataset_list$dataset_2018_all, pmt, by = "row.names")
a = df_pmt %>% dplyr::filter(stringi::stri_detect_regex(.$hhid, "(.37.*)|(.27.*)|(.24.*)"))
df_pmt %<>% dplyr::mutate(povline_pmt = ifelse(y <= log10(137430/365), 1, 0),
                          popbnfcry_pmt = ifelse(y <= quantile(y, c(0.092)), 1, 0)
                   ) %>%
  dplyr::mutate(utility_ganma3_pmt_povline = crra_func((percapcons + povline_pmt * 20000), ganma = 2),
               utility_ganma5_pmt_povline = crra_func((percapcons + povline_pmt * 20000), ganma = 5),
               utility_ganma2_pmt_povline = crra_func((percapcons + povline_pmt * 20000), ganma = 3),
               povline_pmt_food = ifelse(percapcons <= (137430/365) & stringi::stri_detect_regex(hhid, "(.37.*)|(.27.*)|(.24.*)") == TRUE, 
                                         1, 0),
               utility_ganma3_pmt_popbnfcry = crra_func((percapcons + popbnfcry_pmt * 20000), ganma = 3),
               utility_ganma5_pmt_popbnfcry = crra_func((percapcons + popbnfcry_pmt * 20000), ganma = 5),
               utility_ganma2_pmt_popbnfcry = crra_func((percapcons + popbnfcry_pmt * 20000), ganma = 2),
  ) %>%
  dplyr::mutate(popbnfcry_pmt_food = ifelse(percapcons <= quantile(a$percapcons, c(0.092), na.rm =TRUE)& stringi::stri_detect_regex(hhid, "(.37.*)|(.27.*)|(.24.*)") == TRUE, 
                                          1, 0)) %>%
  dplyr::mutate(
    utility_ganma3_pmt_povline_food = crra_func((percapcons + povline_pmt_food * 43092), ganma = 3),
    utility_ganma5_pmt_povline_food = crra_func((percapcons + povline_pmt_food * 43092), ganma = 5),
    utility_ganma2_pmt_povline_food = crra_func((percapcons + povline_pmt_food * 43092), ganma = 2),
    utility_ganma3_pmt_popbnfcry_food = crra_func((percapcons + popbnfcry_pmt_food * 43092), ganma = 3),
    utility_ganma5_pmt_popbnfcry_food = crra_func((percapcons + popbnfcry_pmt_food * 43092), ganma = 5),
    utility_ganma2_pmt_popbnfcry_food = crra_func((percapcons + popbnfcry_pmt_food * 43092), ganma = 2)
  )

#cash  
sum(df_pmt$utility_ganma2_pmt_povline,na.rm = TRUE)
sum(df_pmt$utility_ganma3_pmt_povline,na.rm = TRUE)
sum(df_pmt$utility_ganma5_pmt_povline,na.rm = TRUE)
sum(df_pmt$utility_ganma2_pmt_popbnfcry,na.rm = TRUE)
sum(df_pmt$utility_ganma3_pmt_popbnfcry,na.rm = TRUE)
sum(df_pmt$utility_ganma5_pmt_popbnfcry,na.rm = TRUE)


sum(df_pmt$utility_ganma2_pmt_povline_food,na.rm = TRUE)
sum(df_pmt$utility_ganma3_pmt_povline_food,na.rm = TRUE)
sum(df_pmt$utility_ganma5_pmt_povline_food,na.rm = TRUE)
sum(df_pmt$utility_ganma2_pmt_popbnfcry_food,na.rm = TRUE)
sum(df_pmt$utility_ganma3_pmt_popbnfcry_food,na.rm = TRUE)
sum(df_pmt$utility_ganma5_pmt_popbnfcry_food,na.rm = TRUE)



  
  
#実際のper capita consの受益者選定


#ガンマの値帰る
df_use = dataset_list$dataset_2020_all %>%
  dplyr::rename(povline_cons = povline, popbnfcry_cons = popbnfcry)

aid = c(food = 43092, cash = 20000)
method = c("cons", "pmt")
cutoff = c("povline", "popbnfcry")
ganma = c("3","5","7")

a = df_use %>% dplyr::filter(., stringi::stri_detect_regex(.$hhid, "(.37.*)|(.27.*)|(.24.*)"))

df_use %<>% 
       dplyr::mutate(beneficiary_food_popbnfcry_cons = ifelse(percapcons <= quantile(a$percapcons, c(0.092), na.rm = TRUE) & stringi::stri_detect_regex(hhid, "(.37.*)|(.27.*)|(.24.*)") == TRUE, 1, 0)) %>%
  dplyr::mutate(beneficiary_food_povline_cons = ifelse(stringi::stri_detect_regex(hhid, "(.37.*)|(.27.*)|(.24.*)") == TRUE & povline_cons == 1, 1, 0)) %>%
  dplyr::mutate(percapcons_food_povline_cons = percapcons + beneficiary_food_povline_cons * 43092,
                crra_food_povline_cons_ganma3 = crra_func(percapcons_food_povline_cons, ganma = 3),
                crra_food_povline_cons_ganma5 = crra_func(percapcons_food_povline_cons, ganma = 5),
                crra_food_povline_cons_ganma2 = crra_func(percapcons_food_povline_cons, ganma = 2),
                percapcons_food_popbnfcry_cons = percapcons + beneficiary_food_popbnfcry_cons * 43092,
                crra_food_popbnfcry_cons_ganma3 = crra_func(percapcons_food_popbnfcry_cons, ganma = 3),
                crra_food_popbnfcry_cons_ganma5 = crra_func(percapcons_food_popbnfcry_cons, ganma = 5),
                crra_food_popbnfcry_cons_ganma2 = crra_func(percapcons_food_popbnfcry_cons, ganma = 2)
                ) %>%
  dplyr::mutate(crra_cash_povlinec_ons_ganma2 = crra_func(percapcons + povline_cons * 20000, ganma = 2),
                crra_cash_povline_cons_ganma3 = crra_func(percapcons + povline_cons * 20000, ganma = 3),
                crra_cash_povline_cons_ganma5 = crra_func(percapcons + povline_cons * 20000, ganma = 5),
                crra_cash_popbnfcry_cons_ganma2 = crra_func(percapcons + popbnfcry_cons * 20000, ganma = 2),
                crra_cash_popbnfcry_cons_ganma3 = crra_func(percapcons + popbnfcry_cons * 20000, ganma = 3),
                crra_cash_popbnfcry_cons_ganma5 = crra_func(percapcons + popbnfcry_cons * 20000, ganma = 5)
  ) 

list_cons = list("crra_food_povline_cons_ganma3", "crra_food_povline_cons_ganma5", "crra_food_povline_cons_ganma2", 
                 "crra_food_popbnfcry_cons_ganma3", "crra_food_popbnfcry_cons_ganma5", "crra_food_popbnfcry_cons_ganma2",
                 "crra_cash_popbnfcry_cons_ganma2", "crra_cash_ppopbnfcry_cons_ganma3", "crra_cash_popbnfcry_cons_ganma5",
                 "crra_cash_povlinec_ons_ganma2", "crra_cash_povline_cons_ganma3", "crra_cash_povline_cons_ganma5")
                 
for (i in list_cons) {
  print(i)
  print(sum(df_use[[i]], na.rm = TRUE))
  print("")
}


############################################################

#推計モデルのエリートキャプチャーあり
df_elitecap = merge(dataset_list$dataset_2018_all, list_est_error$trnsfr_any_dummys2Id18$augment$.fitted, by ="row.names")
df_elitecap %<>% 
  dplyr::mutate(utility_ganma2_cash = crra_func((percapcons + y * 20000), ganma = 2),
                utility_ganma3_cash = crra_func((percapcons + y * 20000), ganma = 3),
                utility_ganma5_cash = crra_func((percapcons + y * 20000), ganma = 5) 
  )%>%
  dplyr::mutate(y_food = ifelse(stringi::stri_detect_regex(df_elitecap$hhid, "(.37.*)|(.27.*)|(.24.*)"), y, 0)
  )%>% 
  dplyr::mutate(
    utility_ganma2_food = crra_func((percapcons + y_food * 43092), ganma = 2),
    utility_ganma3_food = crra_func((percapcons + y_food * 43092), ganma = 3),
    utility_ganma5_food = crra_func((percapcons + y_food * 43092), ganma = 5)
  )


sum(df_elitecap$utility_ganma2_cash,na.rm = TRUE)
sum(df_elitecap$utility_ganma3_cash,na.rm = TRUE)
sum(df_elitecap$utility_ganma5_cash,na.rm = TRUE)

sum(df_elitecap$utility_ganma2_food,na.rm = TRUE)
sum(df_elitecap$utility_ganma3_food,na.rm = TRUE)
sum(df_elitecap$utility_ganma5_food,na.rm = TRUE)


g = ggplot2::ggplot(df_elitecap, aes(x = lpercapcons, y = utility_ganma2_cash)) +
  geom_point()+
  ylim(-0.05, 0.01) +
  geom_path(data=data.frame(X=x<-seq(1,5,len=100), Y=x^(1-2) / (1-2))+0.2, aes(x=X,y=Y))
plot(g)

g = ggplot2::ggplot(df_elitecap, aes(x = lpercapcons, y = utility_ganma3_cash)) +
  geom_point()+
  ylim(-0.005, 0.01) +
  geom_path(data=data.frame(X=x<-seq(1,5,len=100), Y=x^(1-3) / (1-3))+0.2, aes(x=X,y=Y))
plot(g)

g = ggplot2::ggplot(df_elitecap, aes(x = lpercapcons, y = utility_ganma5_cash)) +
  geom_point()+
  ylim(-1, 1) 
#geom_path(data=data.frame(X=x<-seq(1,5,len=1000), Y=x^(1-7)/(1-7)), aes(x=X,y=Y))

plot(p)
p <- ggplot(data=data.frame(X=c(-4,4)), aes(x=X))
p <- p +stat_function(fun=function(x) x^(1-0.5)/(1-0.5))
#推計モデルのエリートキャプチャーなし
df_elitecap %<>% dplyr::mutate(y_without_elitecap = y - elite_lc_gov * (-0.112833071))
df_elitecap %<>% dplyr::mutate(utility_ganma3_cash_without_elitecap = crra_func((percapcons + y_without_elitecap * 20000), ganma = 3),
                               utility_ganma5_cash_without_elitecap = crra_func((percapcons + y_without_elitecap * 20000), ganma = 5),
                               utility_ganma2_cash_without_elitecap = crra_func((percapcons + y_without_elitecap * 20000), ganma = 2)
) %>%
  dplyr::mutate(y_food = ifelse(stringi::stri_detect_regex(df_elitecap$hhid, "(.37.*)|(.27.*)|(.24.*)"), y_without_elitecap, 0)
  )%>% 
  dplyr::mutate(
    utility_ganma2_food_without_elitecap = crra_func((percapcons + y_food * 43092), ganma = 2),
    utility_ganma3_food_without_elitecap = crra_func((percapcons + y_food * 43092), ganma = 3),
    utility_ganma5_food_without_elitecap = crra_func((percapcons + y_food * 43092), ganma = 5)
  )

sum(df_elitecap$utility_ganma2_cash_without_elitecap,na.rm = TRUE)
sum(df_elitecap$utility_ganma3_cash_without_elitecap,na.rm = TRUE)
sum(df_elitecap$utility_ganma5_cash_without_elitecap,na.rm = TRUE)

sum(df_elitecap$utility_ganma2_food_without_elitecap,na.rm = TRUE)
sum(df_elitecap$utility_ganma3_food_without_elitecap,na.rm = TRUE)
sum(df_elitecap$utility_ganma5_food_without_elitecap,na.rm = TRUE)

df_elitecap$elite_lc_gov

#PMT基準の受益者選定
mutate_pmt = function(){
  var = list_est_pmt$lpercapcons_s4_Id$estimate$term
  df_use = dataset_list$dataset_2018_all[[var[[!stringi::stri_detect_regex(var, ".*comid.*")]]]]
  
}
rownames(list_est_pmt$lpercapcons_s4_Id$estimate$term[["plot"]])
list_est_pmt$lpercapcons_s4_Id$estimate$estimate[1] + list_est_pmt$lpercapcons_s4_Id$estimate$term["plot"]

lfe::predict(est, newdata = dataset_list$dataset_2018_all)
formula = list_est_pmt$lpercapcons_s4_Id$formula
est = lm(as.formula(formula), data = dataset_list$dataset_2015_all )

pmt = predict(est, newdata = levels(droplevels(dataset_list$dataset_2018_all)))

df_pmt = merge(dataset_list$dataset_2018_all, pmt, by = "row.names")
a = df_pmt %>% dplyr::filter(stringi::stri_detect_regex(.$hhid, "(.37.*)|(.27.*)|(.24.*)"))
df_pmt %<>% dplyr::mutate(povline_pmt = ifelse(y <= log10(137430/365), 1, 0),
                          popbnfcry_pmt = ifelse(y <= quantile(y, c(0.092)), 1, 0)
) %>%
  dplyr::mutate(utility_ganma3_pmt_povline = crra_func((percapcons + povline_pmt * 20000), ganma = 2),
                utility_ganma5_pmt_povline = crra_func((percapcons + povline_pmt * 20000), ganma = 5),
                utility_ganma2_pmt_povline = crra_func((percapcons + povline_pmt * 20000), ganma = 3),
                povline_pmt_food = ifelse(percapcons <= (137430/365) & stringi::stri_detect_regex(hhid, "(.37.*)|(.27.*)|(.24.*)") == TRUE, 
                                          1, 0),
                utility_ganma3_pmt_popbnfcry = crra_func((percapcons + popbnfcry_pmt * 20000), ganma = 3),
                utility_ganma5_pmt_popbnfcry = crra_func((percapcons + popbnfcry_pmt * 20000), ganma = 5),
                utility_ganma2_pmt_popbnfcry = crra_func((percapcons + popbnfcry_pmt * 20000), ganma = 2),
  ) %>%
  dplyr::mutate(popbnfcry_pmt_food = ifelse(percapcons <= quantile(a$percapcons, c(0.092), na.rm =TRUE)& stringi::stri_detect_regex(hhid, "(.37.*)|(.27.*)|(.24.*)") == TRUE, 
                                            1, 0)) %>%
  dplyr::mutate(
    utility_ganma3_pmt_povline_food = crra_func((percapcons + povline_pmt_food * 43092), ganma = 3),
    utility_ganma5_pmt_povline_food = crra_func((percapcons + povline_pmt_food * 43092), ganma = 5),
    utility_ganma2_pmt_povline_food = crra_func((percapcons + povline_pmt_food * 43092), ganma = 2),
    utility_ganma3_pmt_popbnfcry_food = crra_func((percapcons + popbnfcry_pmt_food * 43092), ganma = 3),
    utility_ganma5_pmt_popbnfcry_food = crra_func((percapcons + popbnfcry_pmt_food * 43092), ganma = 5),
    utility_ganma2_pmt_popbnfcry_food = crra_func((percapcons + popbnfcry_pmt_food * 43092), ganma = 2)
  )

#cash  
sum(df_pmt$utility_ganma2_pmt_povline,na.rm = TRUE)
sum(df_pmt$utility_ganma3_pmt_povline,na.rm = TRUE)
sum(df_pmt$utility_ganma5_pmt_povline,na.rm = TRUE)
sum(df_pmt$utility_ganma2_pmt_popbnfcry,na.rm = TRUE)
sum(df_pmt$utility_ganma3_pmt_popbnfcry,na.rm = TRUE)
sum(df_pmt$utility_ganma5_pmt_popbnfcry,na.rm = TRUE)


sum(df_pmt$utility_ganma2_pmt_povline_food,na.rm = TRUE)
sum(df_pmt$utility_ganma3_pmt_povline_food,na.rm = TRUE)
sum(df_pmt$utility_ganma5_pmt_povline_food,na.rm = TRUE)
sum(df_pmt$utility_ganma2_pmt_popbnfcry_food,na.rm = TRUE)
sum(df_pmt$utility_ganma3_pmt_popbnfcry_food,na.rm = TRUE)
sum(df_pmt$utility_ganma5_pmt_popbnfcry_food,na.rm = TRUE)





#実際のper capita consの受益者選定


#ガンマの値帰る
df_use = dataset_list$dataset_2018_all %>%
  dplyr::rename(povline_cons = povline, popbnfcry_cons = popbnfcry)

aid = c(food = 43092, cash = 20000)
method = c("cons", "pmt")
cutoff = c("povline", "popbnfcry")
ganma = c("3","5","7")

a = df_use %>% dplyr::filter(., stringi::stri_detect_regex(.$hhid, "(.37.*)|(.27.*)|(.24.*)"))

df_use %<>% 
  dplyr::mutate(beneficiary_food_popbnfcry_cons = ifelse(percapcons <= quantile(a$percapcons, c(0.092), na.rm = TRUE) & stringi::stri_detect_regex(hhid, "(.37.*)|(.27.*)|(.24.*)") == TRUE, 1, 0)) %>%
  dplyr::mutate(beneficiary_food_povline_cons = ifelse(stringi::stri_detect_regex(hhid, "(.37.*)|(.27.*)|(.24.*)") == TRUE & povline_cons == 1, 1, 0)) %>%
  dplyr::mutate(percapcons_food_povline_cons = percapcons + beneficiary_food_povline_cons * 43092,
                crra_food_povline_cons_ganma3 = crra_func(percapcons_food_povline_cons, ganma = 3),
                crra_food_povline_cons_ganma5 = crra_func(percapcons_food_povline_cons, ganma = 5),
                crra_food_povline_cons_ganma2 = crra_func(percapcons_food_povline_cons, ganma = 2),
                percapcons_food_popbnfcry_cons = percapcons + beneficiary_food_popbnfcry_cons * 43092,
                crra_food_popbnfcry_cons_ganma3 = crra_func(percapcons_food_popbnfcry_cons, ganma = 3),
                crra_food_popbnfcry_cons_ganma5 = crra_func(percapcons_food_popbnfcry_cons, ganma = 5),
                crra_food_popbnfcry_cons_ganma2 = crra_func(percapcons_food_popbnfcry_cons, ganma = 2)
  ) %>%
  dplyr::mutate(crra_cash_povlinec_ons_ganma2 = crra_func(percapcons + povline_cons * 20000, ganma = 2),
                crra_cash_povline_cons_ganma3 = crra_func(percapcons + povline_cons * 20000, ganma = 3),
                crra_cash_povline_cons_ganma5 = crra_func(percapcons + povline_cons * 20000, ganma = 5),
                crra_cash_popbnfcry_cons_ganma2 = crra_func(percapcons + popbnfcry_cons * 20000, ganma = 2),
                crra_cash_popbnfcry_cons_ganma3 = crra_func(percapcons + popbnfcry_cons * 20000, ganma = 3),
                crra_cash_popbnfcry_cons_ganma5 = crra_func(percapcons + popbnfcry_cons * 20000, ganma = 5)
  ) 

list_cons = list("crra_food_povline_cons_ganma3", "crra_food_povline_cons_ganma5", "crra_food_povline_cons_ganma2", 
                 "crra_food_popbnfcry_cons_ganma3", "crra_food_popbnfcry_cons_ganma5", "crra_food_popbnfcry_cons_ganma2",
                 "crra_cash_popbnfcry_cons_ganma2", "crra_cash_ppopbnfcry_cons_ganma3", "crra_cash_popbnfcry_cons_ganma5",
                 "crra_cash_povlinec_ons_ganma2", "crra_cash_povline_cons_ganma3", "crra_cash_povline_cons_ganma5")

for (i in list_cons) {
  print(i)
  print(sum(df_use[[i]], na.rm = TRUE))
  print("")
}











