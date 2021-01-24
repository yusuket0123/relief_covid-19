#"""
# estimation functiions
#"""

library(R6)

est = R6Class(
  "est", # estというクラスを作成
  
  public = list(
    data = NA,
    
    initialize = function(data){ 
      #コンストラクタは、"initialize"
      #インスタンス変数はself$
      self$data = data
    },
    
    ### メソッドを定義
    ## did estimation
    est_did = function(year = "2020", outcome, covariates) {
      # dataframe for estimation
      year = y
      df_use = self$data %>%
        dplyr::mutate(post = dplyr::case_when(year == y ~ 1, TRUE ~ 0)
        )
      # estimation
      formula = paste(outcome, paste(covariates, collapse = " + "), sep = " ~ ")
      reg = lfe::felm(as.formula(formula), df_use)
      summary = list(formula = formula, 
                     tidy = broom::tidy(reg), 
                     augment = broom::augment(reg), 
                     glance = broom::glance(reg)
      )
      return(summary)
    },
    
    ## linear prob. model at household level
    estimate_error_hhlevel = function(comid = "yes", outcome, covariates){
      if(comid == "yes"){
        formula = paste(outcome, paste(paste(covariates, collapse = " + "), "comid", sep = " + "), sep = " ~ ")
      } else {
        formula = paste(outcome, paste(covariates, collapse = " + "), sep = " ~ ")
      }
      est = lfe::felm(as.formula(formula) , data = self$data)
      summary = list(formula = formula, estimate = broom::tidy(est), glance = broom::glance(est), augment = broom::augment(est))
      return(summary)
    },
    ## linear prob. model at community level
    estimate_error_comlevel = function(outcome, covariates){
      formula = paste(outcome, paste(covariates, collapse = " + "), sep = " ~ ")
      est = lfe::felm(as.formula(formula) , data = self$data)
      summary = list(formula = formula, estimate = broom::tidy(est), glance = broom::glance(est), augment = broom::augment(est))
      return(summary)
    },
    ## change instance var.
    set_data = function(data){
      self$data = data
    }
)
)

output.excelsheets = R6Class(
  "output.excelsheets",
  
  public = list(
    file_name = NA,
    
    initialize = function(file_name){
      self$file_name = file_name
    },
    ## write xlsx files
    output_xlsxfile = function(list_est){
      wb <- openxlsx::createWorkbook()
      
      for (i in names(list_est)[!stringi::stri_detect_regex(names(list_est), ".*imp.*")]) {
        sheet = i
        openxlsx::addWorksheet(wb, paste0(sheet,"e"))
        openxlsx::addWorksheet(wb, paste0(sheet,"g"))
        openxlsx::writeData(wb, sheet = paste0(sheet,"e"), x = list_est[[i]]$tidy)
        openxlsx::writeData(wb, sheet = paste0(sheet,"g"), x = list_est[[i]]$glance)
      }
      ## Save workbook to working directory
      openxlsx::saveWorkbook(wb, file = paste0(self$file_name, ".xlsx"), overwrite = TRUE)
      print("saving succeed")
    },
    set_file_name = function(file_name){
      self$file_name = file_name
    }
  )
)




