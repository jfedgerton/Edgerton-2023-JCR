## ------------------------------------- ##
##    Create tables Edgerton (2023 JCR)  ## 
## ------------------------------------- ##
rm(list = ls())

load("data_for_analysis.Rda")
## Load models
all_models <- list.files("model_output")
for (i in 1:length(all_models))
{
  load(paste0("model_output/", all_models[i])) 
}

## Fixed effects coefs
extract.pois.fe <- function(model,
                            include.deviance = TRUE,
                            include.nobs = TRUE,
                            ...) {
  s <- summary(model)
  coefficient.names <- rownames(s$coeftable)
  coefficient.names[grepl("sunni_10", coefficient.names)]         <- "Excluded Sunni Mulsim group (10 years)"
  coefficient.names[grepl("sunni_20", coefficient.names)]         <- "Excluded Sunni Mulsim group (20 years)"
  coefficient.names[grepl("sunni_30", coefficient.names)]         <- "Excluded Sunni Mulsim group (30 years)"
  
  coefficient.names[coefficient.names == "nlights_calib_mean"] <- "Nighttime lights (calibrated mean)$^1$"
  coefficient.names[coefficient.names == "log(pop_gpw_sum)"]   <- "Population$^2$"
  coefficient.names[coefficient.names == "log(bdist1)"]        <- "Distinace to the border$^2$"
  coefficient.names[coefficient.names == "mountains_mean"]     <- "Mountainous area (%)"
  coefficient.names[coefficient.names == "petroleum_s"]        <- "Petroleum$^3$"
  coefficient.names[coefficient.names == "log(ttime_mean)"]    <- "Travel time to capital$^2$"
  coefficient.names[coefficient.names == "log(landarea)"]      <- "Cell land area$^2$"
  coefficient.names[coefficient.names == "log(gcp_ppp)"]       <- "Gross cell product$^2$"
  
  co <- s$coeftable[, 1]
  se <- s$coeftable[, 2]
  pval <- s$coeftable[, 4]
  
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  
  if (include.deviance == TRUE) {
    gof <- c(gof, deviance(model))
    gof.names <- c(gof.names, "Deviance")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.nobs == TRUE) {
    n <- s$nobs["nobs"]
    gof <- c(gof, n)
    gof.names <- c(gof.names, "Num. obs.")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  
  
  
  tr <- createTexreg(
    coef.names = coefficient.names,
    coef = co,
    se = se,
    pvalues = pval,
    gof.names = gof.names,
    gof = gof,
    gof.decimal = gof.decimal
  )
  return(tr)
}


## random effects coefficients
extract.pois.re <- function(model,
                            include.loglik = TRUE,
                            include.deviance = TRUE,
                            include.nobs = TRUE,
                            ...) {
  s <- summary(model)
  coefficient.names <- rownames(s$coefficients)
  coefficient.names[grepl("excluded_sunni_10_yrs", coefficient.names)]         <- "Excluded Sunni Mulsim group (10 years)"
  coefficient.names[grepl("excluded_sunni_20_yrs", coefficient.names)]         <- "Excluded Sunni Mulsim group (20 years)"
  coefficient.names[grepl("excluded_sunni_30_yrs", coefficient.names)]         <- "Excluded Sunni Mulsim group (30 years)"
  
  coefficient.names[coefficient.names == "nlights_calib_mean"] <- "Nighttime lights (calibrated mean)$^1$"
  coefficient.names[coefficient.names == "log(pop_gpw_sum)"]   <- "Population$^2$"
  coefficient.names[coefficient.names == "log(bdist1)"]        <- "Distinace to the border$^2$"
  coefficient.names[coefficient.names == "mountains_mean"]     <- "Mountainous area (%)"
  coefficient.names[coefficient.names == "petroleum_s"]        <- "Petroleum$^3$"
  coefficient.names[coefficient.names == "log(ttime_mean)"]    <- "Travel time to capital$^2$"
  coefficient.names[coefficient.names == "log(landarea)"]      <- "Cell land area$^2$"
  coefficient.names[coefficient.names == "log(gcp_ppp)"]       <- "Gross cell product$^2$"
  coefficient.names[coefficient.names == "p_polity2"]          <- "Polity 2"
  coefficient.names[coefficient.names == "p_polity2_sqr"]      <- "Polity 2 squared"
  coefficient.names[coefficient.names == "al_ethnic"]          <- "Ethnic fractionalization"
  coefficient.names[coefficient.names == "al_religion"]        <- "Religious fractionalization"
  coefficient.names[coefficient.names == "al_language"]        <- "Language fractionalization"
  
  coefficient.names[coefficient.names == "sunni_10_years"]     <- "Sunni Muslims Powerless/Discriminated against (10 years)"
  coefficient.names[coefficient.names == "sunni_20_years"]     <- "Sunni Muslims Powerless/Discriminated against (20 years)"
  coefficient.names[coefficient.names == "sunni_30_years"]     <- "Sunni Muslims Powerless/Discriminated against (30 years)"
  
  coefficient.names[coefficient.names == "excluded_10_years"]  <- "Excluded group (10 years)"
  coefficient.names[coefficient.names == "excluded_20_years"]  <- "Excluded group (20 years)"
  coefficient.names[coefficient.names == "excluded_30_years"]  <- "Excluded group (30 years)"
  
  coefficient.names[coefficient.names == "log(wdi_unempmne)"]  <- "Unemployment men (\\%)$^2$"
  coefficient.names[coefficient.names == "(Intercept)"]        <- "Constant"
  
  co <- s$coefficients[, 1]
  se <- s$coefficients[, 2]
  pval <- s$coefficients[, 4]
  
  
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.loglik == TRUE) {
    lik <- logLik(model)
    gof <- c(gof, lik)
    gof.names <- c(gof.names, "Log Likelihood")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.deviance == TRUE) {
    gof <- c(gof, deviance(model))
    gof.names <- c(gof.names, "Deviance")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.nobs == TRUE) {
    n <- nrow(model@frame)
    gof <- c(gof, n)
    gof.names <- c(gof.names, "Num. obs.")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  
  
  
  tr <- createTexreg(
    coef.names = coefficient.names,
    coef = co,
    se = se,
    pvalues = pval,
    gof.names = gof.names,
    gof = gof,
    gof.decimal = gof.decimal
  )
  return(tr)
}


simple_10_tr  <- extract.pois.fe(simple_model_fe_10yrs)
full_fe_10_tr <- extract.pois.fe(full_model_fe_10yrs)
full_re_10_tr <- extract.pois.re(full_model_re_10yrs_int)

simple_20_tr  <- extract.pois.fe(simple_model_fe_20yrs)
full_fe_20_tr <- extract.pois.fe(full_model_fe_20yrs)
full_re_20_tr <- extract.pois.re(full_model_re_20yrs_int)

simple_30_tr  <- extract.pois.fe(simple_model_fe_30yrs)
full_fe_30_tr <- extract.pois.fe(full_model_fe_30yrs)
full_re_30_tr <- extract.pois.re(full_model_re_30yrs_int)


table_2 <- texreg(list(full_fe_10_tr, full_re_10_tr,
            full_fe_20_tr, full_re_20_tr,
            full_fe_30_tr, full_re_30_tr), stars = 0.05,
       reorder.coef = c(1, 19, 22, 17, 18, 20, 21,  23, 24, 
                        2, 3, 4, 5, 6, 7, 8, 9, 
                        11, 12, 13, 14, 15, 16, 10))
write.table(table_2, file = "summary_stats/table_2.txt")


fe_ols_10_tr  <- extract.pois.fe(fe_ols_model_fe_10yrs)
fe_ols_20_tr  <- extract.pois.fe(fe_ols_model_fe_20yrs)
fe_ols_30_tr  <- extract.pois.fe(fe_ols_model_fe_30yrs)

si_table_8 <- texreg(list(fe_ols_10_tr, fe_ols_20_tr, fe_ols_30_tr), stars = 0.05)
write.table(si_table_8, file = "summary_stats/si_table_8.txt")

simple_10_tr  <- extract.pois.fe(simple_model_fe_10yrs)
simple_20_tr  <- extract.pois.fe(simple_model_fe_20yrs)
simple_30_tr  <- extract.pois.fe(simple_model_fe_30yrs)
simple_nlight <- extract.pois.fe(nlights_simple_model_fe)

fe_10_yrs_tr  <- extract.pois.fe(fe_10_yrs)
fe_20_yrs_tr  <- extract.pois.fe(fe_20_yrs)
fe_30_yrs_tr  <- extract.pois.fe(fe_30_yrs)


re_10_yrs_tr  <- extract.pois.re(full_model_re_10yrs_int_other)
re_20_yrs_tr  <- extract.pois.re(full_model_re_20yrs_int_other)
re_30_yrs_tr  <- extract.pois.re(full_model_re_30yrs_int_other)

no_outliers_model_fe_10_tr  <- extract.pois.fe(no_outliers_model_fe_10yrs)
no_outliers_model_fe_20_tr  <- extract.pois.fe(no_outliers_model_fe_20yrs)
no_outliers_model_fe_30_tr  <- extract.pois.fe(no_outliers_model_fe_30yrs)

no_outliers_tab <- texreg(list(no_outliers_model_fe_10_tr, no_outliers_model_fe_20_tr, no_outliers_model_fe_30_tr), stars = 0.05
)

write.table(no_outliers_tab, file = "summary_stats/appendix_table_7.txt")



tr_no_iraq_syria_fe_10_yrs  <- extract.pois.fe(no_iraq_syria_fe_10_yrs)
tr_no_iraq_syria_fe_20_yrs  <- extract.pois.fe(no_iraq_syria_fe_20_yrs)
tr_no_iraq_syria_fe_30_yrs  <- extract.pois.fe(no_iraq_syria_fe_30_yrs)

tr_full_no_iraq_syria_fe_10_yrs  <- extract.pois.fe(full_no_iraq_syria_model_fe_10yrs)
tr_full_no_iraq_syria_fe_20_yrs  <- extract.pois.fe(full_no_iraq_syria_model_fe_20yrs)
tr_full_no_iraq_syria_fe_30_yrs  <- extract.pois.fe(full_no_iraq_syria_model_fe_30yrs)

appendix_table_4 <- texreg(list(tr_no_iraq_syria_fe_10_yrs, 
            tr_no_iraq_syria_fe_20_yrs, 
            tr_no_iraq_syria_fe_30_yrs,
            tr_full_no_iraq_syria_fe_10_yrs,
            tr_full_no_iraq_syria_fe_20_yrs,
            tr_full_no_iraq_syria_fe_30_yrs), stars = 0.05
)

write.table(appendix_table_4, file = "summary_stats/appendix_table_4.txt")


present_patterns_bivariate_tr  <- extract.pois.fe(present_patterns_bivariate)
present_patterns_simple_tr     <- extract.pois.fe(present_patterns_simple)
present_patterns_full_tr       <- extract.pois.fe(present_patterns_full)


appendix_table_5 <- texreg(list(present_patterns_bivariate_tr, present_patterns_simple_tr, present_patterns_full_tr),  stars = 0.05)
write.table(appendix_table_5, file = "summary_stats/appendix_table_5.txt")


appendix_table_2 <- texreg(list(simple_10_tr, simple_20_tr, simple_30_tr, simple_nlight,
            fe_10_yrs_tr, fe_20_yrs_tr, fe_30_yrs_tr,
            re_10_yrs_tr, re_20_yrs_tr, re_30_yrs_tr), stars = 0.05
)
write.table(appendix_table_2, file = "summary_stats/appendix_table_2.txt")