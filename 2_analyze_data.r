## -------------------------------------- ##
##    Analyze data Edgerton (2023 JCR)    ## 
## -------------------------------------- ##

rm(list = ls())
## Import data
load("data_for_analysis.Rda")

## Format variables
data_for_analysis$ttime_mean <- data_for_analysis$ttime_mean + 1
data_for_analysis$landarea <- data_for_analysis$landarea + 1
data_for_analysis$sunni_20_years <- ifelse(data_for_analysis$sunni_10_years == 1, 1, data_for_analysis$sunni_20_years)
data_for_analysis$sunni_30_years <- ifelse(data_for_analysis$sunni_20_years == 1, 1, data_for_analysis$sunni_30_years)
data_for_analysis$sunni_30_years <- ifelse(data_for_analysis$sunni_10_years == 1, 1, data_for_analysis$sunni_30_years)
data_for_analysis$p_polity2_sqr <- data_for_analysis$p_polity2^2
data_for_analysis$excluded_20_years <- ifelse(data_for_analysis$excluded_10_years == 1, 1, data_for_analysis$excluded_20_years)
data_for_analysis$excluded_30_years <- ifelse(data_for_analysis$excluded_20_years == 1, 1, data_for_analysis$excluded_30_years)
data_for_analysis$excluded_30_years <- ifelse(data_for_analysis$excluded_10_years == 1, 1, data_for_analysis$excluded_30_years)

## Simple models
data_for_analysis$petroleum_s[is.na(data_for_analysis$petroleum_s)] <- 0
data_for_analysis$nlights_calib_mean <- as.numeric(scale(data_for_analysis$nlights_calib_mean))
simple_model_fe_10yrs <- fepois(count ~ excluded_sunni_10_yrs + nlights_calib_mean + offset(log(pop_gpw_sum)) | country_name + year, data = data_for_analysis)
simple_model_fe_20yrs <- fepois(count ~ excluded_sunni_20_yrs + nlights_calib_mean + offset(log(pop_gpw_sum)) | country_name + year, data = data_for_analysis)
simple_model_fe_30yrs <- fepois(count ~ excluded_sunni_30_yrs + nlights_calib_mean + offset(log(pop_gpw_sum)) | country_name + year, data = data_for_analysis)


pois_10yrs_inter <- glm(count ~ excluded_sunni_10_yrs + nlights_calib_mean + year + log(pop_gpw_sum) + sunni_10_years + excluded_10_years + log(bdist1) + log(landarea) + mountains_mean + petroleum_s + log(ttime_mean) + log(landarea) + log(gcp_ppp) + offset(log(pop_gpw_sum)), family = poisson, data = data_for_analysis)
pois_20yrs_inter <- glm(count ~ excluded_sunni_20_yrs + nlights_calib_mean + year + log(pop_gpw_sum) + sunni_20_years + excluded_20_years + log(bdist1) + log(landarea) + mountains_mean + petroleum_s + log(ttime_mean) + log(landarea) + log(gcp_ppp) + offset(log(pop_gpw_sum)), family = poisson, data = data_for_analysis)
pois_30yrs_inter <- glm(count ~ excluded_sunni_30_yrs + nlights_calib_mean + year + log(pop_gpw_sum) + sunni_30_years + excluded_30_years + log(bdist1) + log(landarea) + mountains_mean + petroleum_s + log(ttime_mean) + log(landarea) + log(gcp_ppp) + offset(log(pop_gpw_sum)), family = poisson, data = data_for_analysis)


## Full fixed effect regressions
full_model_fe_10yrs <- fepois(count ~ excluded_sunni_10_yrs + nlights_calib_mean + log(pop_gpw_sum) + log(bdist1) + log(landarea) + mountains_mean + petroleum_s + log(ttime_mean) + log(gcp_ppp) + offset(log(pop_gpw_sum)) | country_name + year, data = data_for_analysis)
full_model_fe_20yrs <- fepois(count ~ excluded_sunni_20_yrs + nlights_calib_mean + log(pop_gpw_sum) + log(bdist1) + log(landarea) + mountains_mean + petroleum_s + log(ttime_mean) + log(gcp_ppp) + offset(log(pop_gpw_sum)) | country_name + year, data = data_for_analysis)
full_model_fe_30yrs <- fepois(count ~ excluded_sunni_30_yrs + nlights_calib_mean + log(pop_gpw_sum) + log(bdist1) + log(landarea) + mountains_mean + petroleum_s + log(ttime_mean) + log(gcp_ppp) + offset(log(pop_gpw_sum)) | country_name + year, data = data_for_analysis)

## Full fixed effect regressions
fe_ols_model_fe_10yrs <- feols(log(count + 1) ~ excluded_sunni_10_yrs + nlights_calib_mean + log(pop_gpw_sum) + log(bdist1) + log(landarea) + mountains_mean + petroleum_s + log(ttime_mean) + log(gcp_ppp) + offset(log(pop_gpw_sum)) | country_name + year, data = data_for_analysis)
fe_ols_model_fe_20yrs <- feols(log(count + 1) ~ excluded_sunni_20_yrs + nlights_calib_mean + log(pop_gpw_sum) + log(bdist1) + log(landarea) + mountains_mean + petroleum_s + log(ttime_mean) + log(gcp_ppp) + offset(log(pop_gpw_sum)) | country_name + year, data = data_for_analysis)
fe_ols_model_fe_30yrs <- feols(log(count + 1) ~ excluded_sunni_30_yrs + nlights_calib_mean + log(pop_gpw_sum) + log(bdist1) + log(landarea) + mountains_mean + petroleum_s + log(ttime_mean) + log(gcp_ppp) + offset(log(pop_gpw_sum)) | country_name + year, data = data_for_analysis)

summary(fe_ols_model_fe_10yrs)
summary(fe_ols_model_fe_20yrs)
summary(fe_ols_model_fe_30yrs)

## Full random effect regressions
suppressWarnings({full_model_re_10yrs_int <- glmer(count ~ excluded_sunni_10_yrs + nlights_calib_mean + log(pop_gpw_sum) + log(bdist1) + mountains_mean + petroleum_s + log(ttime_mean) + log(gcp_ppp) + log(landarea) + p_polity2 + p_polity2_sqr + al_ethnic + al_language + al_religion + log(wdi_unempmne) + (1|country_name) + (1|year) + sunni_10_years + excluded_10_years + offset(log(pop_gpw_sum)), family = poisson(link = log), data = data_for_analysis)})
suppressWarnings({full_model_re_20yrs_int <- glmer(count ~ excluded_sunni_20_yrs + nlights_calib_mean + log(pop_gpw_sum) + log(bdist1) + mountains_mean + petroleum_s + log(ttime_mean) + log(gcp_ppp) + log(landarea) + p_polity2 + p_polity2_sqr + al_ethnic + al_language + al_religion + log(wdi_unempmne) + (1|country_name) + (1|year) + sunni_20_years + excluded_20_years + offset(log(pop_gpw_sum)), family = poisson(link = log), data = data_for_analysis)})
suppressWarnings({full_model_re_30yrs_int <- glmer(count ~ excluded_sunni_30_yrs + nlights_calib_mean + log(pop_gpw_sum) + log(bdist1) + mountains_mean + petroleum_s + log(ttime_mean) + log(gcp_ppp) + log(landarea) + p_polity2 + p_polity2_sqr + al_ethnic + al_language + al_religion + log(wdi_unempmne) + (1|country_name) + (1|year) + sunni_30_years + excluded_30_years + offset(log(pop_gpw_sum)), family = poisson(link = log), data = data_for_analysis)})

suppressWarnings({
  ss <- getME(full_model_re_10yrs_int,c("theta","fixef"))
  full_model_re_10yrs_int <- update(full_model_re_10yrs_int, start = ss, control = glmerControl(optCtrl=list(maxfun = 5e4)))
  ss <- getME(full_model_re_20yrs_int,c("theta","fixef"))
  full_model_re_20yrs_int <- update(full_model_re_20yrs_int, start = ss, control = glmerControl(optCtrl=list(maxfun = 5e4)))
  ss <- getME(full_model_re_30yrs_int,c("theta","fixef"))
  full_model_re_30yrs_int <- update(full_model_re_30yrs_int, start = ss, control = glmerControl(optCtrl=list(maxfun = 5e4)))
  ss <- getME(full_model_re_30yrs_int,c("theta","fixef"))
  full_model_re_30yrs_int <- update(full_model_re_30yrs_int, start = ss, control = glmerControl(optCtrl=list(maxfun = 5e4)))
})


summary(full_model_re_10yrs_int)
summary(full_model_re_20yrs_int)
summary(full_model_re_30yrs_int)


## Alt specifications

data_for_analysis <- mutate(
  data_for_analysis,
  active_groups = ifelse(
    country_name %in% c(
      "Afghanistan", 
      "Yemen (Arab Republic of Yemen)", 
      "Somalia", 
      "Iraq", 
      "Syria",
      "Somalia", 
      "Nigeria", 
      "Pakistan"
    ), 1, 0
  )
)

suppressWarnings({full_model_re_10yrs_int_other <- glmer(count ~ excluded_sunni_10_yrs + nlights_calib_mean + log(pop_gpw_sum) + log(bdist1) + mountains_mean + petroleum_s + log(ttime_mean) + log(gcp_ppp) + log(landarea) + p_polity2 + p_polity2_sqr + al_ethnic + al_language + al_religion + active_groups + log(wdi_unempmne) + (1|country_name) + (1|year) + sunni_10_years + excluded_10_years + offset(log(pop_gpw_sum)), family = poisson(link = log), data = data_for_analysis)})
suppressWarnings({full_model_re_20yrs_int_other <- glmer(count ~ excluded_sunni_20_yrs + nlights_calib_mean + log(pop_gpw_sum) + log(bdist1) + mountains_mean + petroleum_s + log(ttime_mean) + log(gcp_ppp) + log(landarea) + p_polity2 + p_polity2_sqr + al_ethnic + al_language + al_religion + active_groups + log(wdi_unempmne) + (1|country_name) + (1|year) + sunni_20_years + excluded_20_years + offset(log(pop_gpw_sum)), family = poisson(link = log), data = data_for_analysis)})
suppressWarnings({full_model_re_30yrs_int_other <- glmer(count ~ excluded_sunni_30_yrs + nlights_calib_mean + log(pop_gpw_sum) + log(bdist1) + mountains_mean + petroleum_s + log(ttime_mean) + log(gcp_ppp) + log(landarea) + p_polity2 + p_polity2_sqr + al_ethnic + al_language + al_religion + active_groups + log(wdi_unempmne) + (1|country_name) + (1|year) + sunni_30_years + excluded_30_years + offset(log(pop_gpw_sum)), family = poisson(link = log), data = data_for_analysis)})

dir.create("model_output", showWarnings = F)
save(simple_model_fe_10yrs, file = "model_output/simple_model_fe_10yrs.Rda")
save(simple_model_fe_20yrs, file = "model_output/simple_model_fe_20yrs.Rda")
save(simple_model_fe_30yrs, file = "model_output/simple_model_fe_30yrs.Rda")
save(simple_model_fe_10yrs, file = "model_output/simple_model_fe_10yrs.Rda")
save(simple_model_fe_20yrs, file = "model_output/simple_model_fe_20yrs.Rda")
save(simple_model_fe_30yrs, file = "model_output/simple_model_fe_30yrs.Rda")
save(full_model_fe_10yrs, file = "model_output/full_model_fe_10yrs.Rda")
save(full_model_fe_20yrs, file = "model_output/full_model_fe_20yrs.Rda")
save(full_model_fe_30yrs, file = "model_output/full_model_fe_30yrs.Rda")
save(pois_10yrs_inter, file = "model_output/pois_10yrs_inter.Rda")
save(pois_20yrs_inter, file = "model_output/pois_20yrs_inter.Rda")
save(pois_30yrs_inter, file = "model_output/pois_30yrs_inter.Rda")
save(full_model_re_10yrs_int, file = "model_output/full_model_re_10yrs_int.Rda")
save(full_model_re_20yrs_int, file = "model_output/full_model_re_20yrs_int.Rda")
save(full_model_re_30yrs_int, file = "model_output/full_model_re_30yrs_int.Rda")
save(full_model_re_10yrs_int_other, file = "model_output/full_model_re_10yrs_int_active_groups.Rda")
save(full_model_re_20yrs_int_other, file = "model_output/full_model_re_20yrs_int_active_groups.Rda")
save(full_model_re_30yrs_int_other, file = "model_output/full_model_re_30yrs_int_active_groups.Rda")
save(fe_ols_model_fe_10yrs, file = "model_output/fe_ols_model_fe_10yrs.Rda")
save(fe_ols_model_fe_20yrs, file = "model_output/fe_ols_model_fe_20yrs.Rda")
save(fe_ols_model_fe_30yrs, file = "model_output/fe_ols_model_fe_30yrs.Rda")

## Alt specication
match_data <- dplyr::select(
  data_for_analysis,
  c(excluded_sunni_10_yrs, excluded_sunni_20_yrs, excluded_sunni_30_yrs, 
    nlights_calib_mean, pop_gpw_sum, capdist, bdist1, 
    mountains_mean, petroleum_s, ttime_mean, landarea, gcp_ppp,
    country_name, year, count)
)

match_data <- match_data[complete.cases(match_data),]
set.seed(1)
suppressMessages(suppressWarnings(ex_match <- MatchIt::matchit(excluded_sunni_10_yrs ~ nlights_calib_mean + log(pop_gpw_sum) + log(bdist1) + mountains_mean + petroleum_s + log(ttime_mean) + log(landarea) + log(gcp_ppp) + country_name + year,
                    method = "nearest", data = match_data)))
matched_excluded   <- MatchIt::match.data(ex_match)
treated_match <- subset(matched_excluded, excluded_sunni_10_yrs == 1)
untreated_match <- subset(matched_excluded, excluded_sunni_10_yrs == 0)
t.test.match.10 <- t.test(log(treated_match$count + 1), log(untreated_match$count + 1))
t.test.match.10

full_model_matched_10yrs <- fepois(count ~ excluded_sunni_10_yrs + nlights_calib_mean + log(pop_gpw_sum) + log(bdist1) + log(landarea) + mountains_mean + petroleum_s + log(ttime_mean) + log(gcp_ppp) + offset(log(pop_gpw_sum))  | country_name + year, data = matched_excluded)
save(t.test.match.10, file = "model_output/t.test.match.10.Rda")
save(full_model_matched_10yrs, file = "model_output/full_model_matched_10yrs.Rda")

suppressMessages(suppressWarnings(ex_match <- MatchIt::matchit(excluded_sunni_20_yrs ~ nlights_calib_mean + log(pop_gpw_sum) +  log(bdist1) + mountains_mean + petroleum_s + log(ttime_mean) + log(landarea) + log(gcp_ppp) + country_name + year,
                    method = "nearest", data = match_data)))
matched_excluded   <- MatchIt::match.data(ex_match)
treated_match <- subset(matched_excluded, excluded_sunni_20_yrs == 1)
untreated_match <- subset(matched_excluded, excluded_sunni_20_yrs == 0)
t.test.match.20 <- t.test(log(treated_match$count + 1), log(untreated_match$count + 1))
full_model_matched_20yrs <- fepois(count ~ excluded_sunni_20_yrs + nlights_calib_mean + log(pop_gpw_sum) + log(bdist1) + log(landarea) + mountains_mean + petroleum_s + log(ttime_mean) + log(gcp_ppp) + offset(log(pop_gpw_sum)) | country_name + year, data = matched_excluded)
save(t.test.match.20, file = "model_output/t.test.match.20.Rda")
save(full_model_matched_20yrs, file = "model_output/full_model_matched_20yrs.Rda")
suppressMessages(suppressWarnings(ex_match <- MatchIt::matchit(excluded_sunni_30_yrs ~   nlights_calib_mean + log(pop_gpw_sum) + log(bdist1) + mountains_mean + petroleum_s + log(ttime_mean) + log(landarea) + log(gcp_ppp)   + country_name + year,
                    method = "nearest", data = match_data)))
matched_excluded   <- MatchIt::match.data(ex_match)
treated_match <- subset(matched_excluded, excluded_sunni_30_yrs == 1)
untreated_match <- subset(matched_excluded, excluded_sunni_30_yrs == 0)
t.test.match.30 <- t.test(log(treated_match$count + 1), log(untreated_match$count + 1))
full_model_matched_30yrs <- fepois(count ~ excluded_sunni_30_yrs + nlights_calib_mean + log(pop_gpw_sum) + log(bdist1) + log(landarea) + mountains_mean + petroleum_s + log(ttime_mean) + log(gcp_ppp) + offset(log(pop_gpw_sum)) | country_name + year, data = matched_excluded)
save(t.test.match.30, file = "model_output/t.test.match.30.Rda")
save(full_model_matched_30yrs, file = "model_output/full_model_matched_30yrs.Rda")

load("data_for_analysis.Rda")
data_for_analysis$isis_attacks <- ifelse(is.na(data_for_analysis$isis_attacks), 0, data_for_analysis$isis_attacks)
data_for_analysis$nlights_calib_mean <- as.numeric(scale(data_for_analysis$nlights_calib_mean))
data_for_analysis$sunni_20_years <- ifelse(data_for_analysis$sunni_10_years == 1, 1, data_for_analysis$sunni_20_years)
data_for_analysis$sunni_30_years <- ifelse(data_for_analysis$sunni_20_years == 1, 1, data_for_analysis$sunni_30_years)
data_for_analysis$sunni_30_years <- ifelse(data_for_analysis$sunni_10_years == 1, 1, data_for_analysis$sunni_30_years)

data_for_analysis$excluded_20_years <- ifelse(data_for_analysis$excluded_10_years == 1, 1, data_for_analysis$excluded_20_years)
data_for_analysis$excluded_30_years <- ifelse(data_for_analysis$excluded_20_years == 1, 1, data_for_analysis$excluded_30_years)
data_for_analysis$excluded_30_years <- ifelse(data_for_analysis$excluded_10_years == 1, 1, data_for_analysis$excluded_30_years)

## Simple models
data_for_analysis$petroleum_s[is.na(data_for_analysis$petroleum_s)] <- 0
simple_model_fe_10yrs <- fepois(count ~ excluded_sunni_10_yrs + offset(log(pop_gpw_sum))  | country_name + year, data = data_for_analysis)
simple_model_fe_20yrs <- fepois(count ~ excluded_sunni_20_yrs + offset(log(pop_gpw_sum))  | country_name + year, data = data_for_analysis)
simple_model_fe_30yrs <- fepois(count ~ excluded_sunni_30_yrs + offset(log(pop_gpw_sum))  | country_name + year, data = data_for_analysis)

nlights_simple_model_fe <- fepois(count ~ nlights_calib_mean + offset(log(pop_gpw_sum))  | country_name + year, data = data_for_analysis)


## fixed effect regressions
data_for_analysis$p_polity2_sqr <- data_for_analysis$p_polity2^2
fe_10_yrs <- fepois(count ~ excluded_sunni_10_yrs + nlights_calib_mean + log(pop_gpw_sum) + offset(log(pop_gpw_sum)) | country_name + year, data = data_for_analysis)
fe_20_yrs <- fepois(count ~ excluded_sunni_20_yrs + nlights_calib_mean + log(pop_gpw_sum) + offset(log(pop_gpw_sum)) | country_name + year, data = data_for_analysis)
fe_30_yrs <- fepois(count ~ excluded_sunni_30_yrs + nlights_calib_mean + log(pop_gpw_sum) + offset(log(pop_gpw_sum)) | country_name + year, data = data_for_analysis)


## Interaction check
fe_10_yrs_int_check <- fepois(count ~ excluded_sunni_10_yrs * nlights_calib_mean + log(pop_gpw_sum) + offset(log(pop_gpw_sum)) | country_name + year, data = data_for_analysis)
fe_20_yrs_int_check <- fepois(count ~ excluded_sunni_20_yrs * nlights_calib_mean + log(pop_gpw_sum) + offset(log(pop_gpw_sum)) | country_name + year, data = data_for_analysis)
fe_30_yrs_int_check <- fepois(count ~ excluded_sunni_30_yrs * nlights_calib_mean + log(pop_gpw_sum) + offset(log(pop_gpw_sum)) | country_name + year, data = data_for_analysis)



## NB regressions
full_nb_re_10yrs <- glm(count ~ excluded_sunni_10_yrs + nlights_calib_mean + log(pop_gpw_sum) + offset(log(pop_gpw_sum))  + year, data = data_for_analysis, family = "quasipoisson")
full_nb_re_20yrs <- glm(count ~ excluded_sunni_20_yrs + nlights_calib_mean + log(pop_gpw_sum) + offset(log(pop_gpw_sum))  + year, data = data_for_analysis, family = "quasipoisson")
full_nb_re_30yrs <- glm(count ~ excluded_sunni_30_yrs + nlights_calib_mean + log(pop_gpw_sum) + offset(log(pop_gpw_sum))  + year, data = data_for_analysis, family = "quasipoisson")


save(simple_model_fe_10yrs, file = "model_output/simple_model_fe_10yrs.Rda")
save(simple_model_fe_20yrs, file = "model_output/simple_model_fe_20yrs.Rda")
save(simple_model_fe_30yrs, file = "model_output/simple_model_fe_30yrs.Rda")

save(fe_10_yrs, file = "model_output/simple_fe_10yrs.Rda")
save(fe_20_yrs, file = "model_output/simple_fe_20yrs.Rda")
save(fe_30_yrs, file = "model_output/simple_fe_30yrs.Rda")

save(full_nb_re_10yrs, file = "model_output/full_nb_re_10yrs.Rda")
save(full_nb_re_20yrs, file = "model_output/full_nb_re_20yrs.Rda")
save(full_nb_re_30yrs, file = "model_output/full_nb_re_30yrs.Rda")

## Jack knife analysis 
coef_exclusion_10 <- c()
coef_exclusion_20 <- c()
coef_exclusion_30 <- c()
coef_nlights_10   <- c()
coef_nlights_20   <- c()
coef_nlights_30   <- c()
pvalue_coef_exclusion_10 <- c()
pvalue_coef_exclusion_20 <- c()
pvalue_coef_exclusion_30 <- c()
pvalue_coef_nlights_10   <- c()
pvalue_coef_nlights_20   <- c()
pvalue_coef_nlights_30   <- c()
set.seed(1)
for (i in 1:1000)
{
  drop_rows <- sample(nrow(data_for_analysis), round(0.05 * nrow(data_for_analysis)))
  full_model_fe_10yrs  <- fepois(count ~ excluded_sunni_10_yrs + nlights_calib_mean + log(pop_gpw_sum) + log(bdist1) + mountains_mean + petroleum_s + log(ttime_mean) + log(landarea) + log(gcp_ppp) + offset(log(pop_gpw_sum)) | country_name + year, data = data_for_analysis[-drop_rows,])
  full_model_fe_20yrs  <- fepois(count ~ excluded_sunni_20_yrs + nlights_calib_mean + log(pop_gpw_sum) + log(bdist1) + mountains_mean + petroleum_s + log(ttime_mean) + log(landarea) + log(gcp_ppp) + offset(log(pop_gpw_sum)) | country_name + year, data = data_for_analysis[-drop_rows,])
  full_model_fe_30yrs  <- fepois(count ~ excluded_sunni_30_yrs + nlights_calib_mean + log(pop_gpw_sum) + log(bdist1) + mountains_mean + petroleum_s + log(ttime_mean) + log(landarea) + log(gcp_ppp) + offset(log(pop_gpw_sum)) | country_name + year, data = data_for_analysis[-drop_rows,])
  coef_exclusion_10[i] <- coef(full_model_fe_10yrs)[1]
  coef_exclusion_20[i] <- coef(full_model_fe_20yrs)[1]
  coef_exclusion_30[i] <- coef(full_model_fe_30yrs)[1]
  coef_nlights_10[i]   <- coef(full_model_fe_10yrs)[2]
  coef_nlights_20[i]   <- coef(full_model_fe_20yrs)[2]
  coef_nlights_30[i]   <- coef(full_model_fe_30yrs)[2]
  
  pvalue_coef_exclusion_10[i] <- summary(full_model_fe_10yrs)$coeftable[1,4]
  pvalue_coef_exclusion_20[i] <- summary(full_model_fe_20yrs)$coeftable[1,4]
  pvalue_coef_exclusion_30[i] <- summary(full_model_fe_30yrs)$coeftable[1,4]
  pvalue_coef_nlights_10[i]   <- summary(full_model_fe_10yrs)$coeftable[2,4]
  pvalue_coef_nlights_20[i]   <- summary(full_model_fe_20yrs)$coeftable[2,4]
  pvalue_coef_nlights_30[i]   <- summary(full_model_fe_30yrs)$coeftable[2,4]
}

jacknife_output <- data.frame(
  exclusion_10 = coef_exclusion_10,
  exclusion_20 = coef_exclusion_20,
  exclusion_30 = coef_exclusion_30,
  nlights_10 = coef_nlights_10,
  nlights_20 = coef_nlights_20,
  nlights_30 = coef_nlights_30,
  pvalue_exclusion_10 = pvalue_coef_exclusion_10,
  pvalue_exclusion_20 = pvalue_coef_exclusion_20,
  pvalue_exclusion_30 = pvalue_coef_exclusion_30,
  pvalue_nlights_10 = pvalue_coef_nlights_10,
  pvalue_nlights_20 = pvalue_coef_nlights_20,
  pvalue_nlights_30 = pvalue_coef_nlights_30
)
save(jacknife_output, file = "model_output/jacknife_output.Rda")

nlights_simple_model_fe <- fepois(count ~ nlights_calib_mean + offset(log(pop_gpw_sum))  | country_name + year, data = data_for_analysis)

## Drop leverage points 
no_outliers_model_fe_10yrs  <- fepois(count ~ excluded_sunni_10_yrs + nlights_calib_mean + log(pop_gpw_sum) + log(bdist1) + mountains_mean + petroleum_s + log(ttime_mean) + log(landarea) + log(gcp_ppp) + offset(log(pop_gpw_sum)) | country_name + year, data = subset(data_for_analysis, count < 10))
no_outliers_model_fe_20yrs  <- fepois(count ~ excluded_sunni_20_yrs + nlights_calib_mean + log(pop_gpw_sum) + log(bdist1) + mountains_mean + petroleum_s + log(ttime_mean) + log(landarea) + log(gcp_ppp) + offset(log(pop_gpw_sum)) | country_name + year, data = subset(data_for_analysis, count < 10))
no_outliers_model_fe_30yrs  <- fepois(count ~ excluded_sunni_30_yrs + nlights_calib_mean + log(pop_gpw_sum) + log(bdist1) + mountains_mean + petroleum_s + log(ttime_mean) + log(landarea) + log(gcp_ppp) + offset(log(pop_gpw_sum)) | country_name + year, data = subset(data_for_analysis, count < 10))


present_patterns_bivariate <- fepois(count ~ excluded_sunni_present | country_name + year, data = data_for_analysis)
present_patterns_simple    <- fepois(count ~ excluded_sunni_present + nlights_calib_mean + log(pop_gpw_sum) + offset(log(pop_gpw_sum)) | country_name + year, data = data_for_analysis)
present_patterns_full      <- fepois(count ~ excluded_sunni_present + nlights_calib_mean + log(pop_gpw_sum) + log(bdist1) + mountains_mean + petroleum_s + log(ttime_mean) + log(landarea) + log(gcp_ppp) + offset(log(pop_gpw_sum)) | country_name + year, data = data_for_analysis)

save(nlights_simple_model_fe, file = "model_output/nlights_simple_model_fe.Rda")
save(no_outliers_model_fe_10yrs, file = "model_output/no_outliers_model_fe_10yrs.Rda")
save(no_outliers_model_fe_20yrs, file = "model_output/no_outliers_model_fe_20yrs.Rda")
save(no_outliers_model_fe_30yrs, file = "model_output/no_outliers_model_fe_30yrs.Rda")

no_iraq_syria_fe_10_yrs <- fepois(count ~ excluded_sunni_10_yrs + nlights_calib_mean + log(pop_gpw_sum) + offset(log(pop_gpw_sum)) | country_name + year, data = subset(data_for_analysis, !(country_name %in% c("Syria", "Iraq"))))
no_iraq_syria_fe_20_yrs <- fepois(count ~ excluded_sunni_20_yrs + nlights_calib_mean + log(pop_gpw_sum) + offset(log(pop_gpw_sum)) | country_name + year, data = subset(data_for_analysis, !(country_name %in% c("Syria", "Iraq"))))
no_iraq_syria_fe_30_yrs <- fepois(count ~ excluded_sunni_30_yrs + nlights_calib_mean + log(pop_gpw_sum) + offset(log(pop_gpw_sum)) | country_name + year, data = subset(data_for_analysis, !(country_name %in% c("Syria", "Iraq"))))

full_no_iraq_syria_model_fe_10yrs  <- fepois(count ~ excluded_sunni_10_yrs + nlights_calib_mean + log(pop_gpw_sum) + log(bdist1) + mountains_mean + petroleum_s + log(ttime_mean) + log(landarea) + log(gcp_ppp) + offset(log(pop_gpw_sum)) | country_name + year, data = subset(data_for_analysis, !(country_name %in% c("Syria", "Iraq"))))
full_no_iraq_syria_model_fe_20yrs  <- fepois(count ~ excluded_sunni_20_yrs + nlights_calib_mean + log(pop_gpw_sum) + log(bdist1) + mountains_mean + petroleum_s + log(ttime_mean) + log(landarea) + log(gcp_ppp) + offset(log(pop_gpw_sum)) | country_name + year, data = subset(data_for_analysis, !(country_name %in% c("Syria", "Iraq"))))
full_no_iraq_syria_model_fe_30yrs  <- fepois(count ~ excluded_sunni_30_yrs + nlights_calib_mean + log(pop_gpw_sum) + log(bdist1) + mountains_mean + petroleum_s + log(ttime_mean) + log(landarea) + log(gcp_ppp) + offset(log(pop_gpw_sum)) | country_name + year, data = subset(data_for_analysis, !(country_name %in% c("Syria", "Iraq"))))

save(no_iraq_syria_fe_10_yrs, file = "model_output/no_iraq_syria_fe_10_yrs.Rda")
save(no_iraq_syria_fe_20_yrs, file = "model_output/no_iraq_syria_fe_20_yrs.Rda")
save(no_iraq_syria_fe_30_yrs, file = "model_output/no_iraq_syria_fe_30_yrs.Rda")

save(full_no_iraq_syria_model_fe_10yrs, file = "model_output/full_no_iraq_syria_model_fe_10yrs.Rda")
save(full_no_iraq_syria_model_fe_20yrs, file = "model_output/full_no_iraq_syria_model_fe_20yrs.Rda")
save(full_no_iraq_syria_model_fe_30yrs, file = "model_output/full_no_iraq_syria_model_fe_30yrs.Rda")

save(present_patterns_bivariate, file = "model_output/present_patterns_bivariate.Rda")
save(present_patterns_simple, file = "model_output/present_patterns_simple.Rda")
save(present_patterns_full, file = "model_output/present_patterns_full.Rda")

pred_temp <- mutate(
  data_for_analysis, 
  pop_gpw_sum = log(pop_gpw_sum), 
  bdist1 = log(bdist1),
  ttime_mean = log(ttime_mean), 
  landarea = log(landarea), 
  gcp_ppp = log(gcp_ppp), 
  wdi_unempmne = log(wdi_unempmne)
)
fe_pois <- glm(count ~ excluded_sunni_10_yrs + nlights_calib_mean + pop_gpw_sum + bdist1 + mountains_mean + petroleum_s + ttime_mean + landarea + gcp_ppp + offset(pop_gpw_sum) + as.factor(country_name) + as.factor(year), family = "poisson", data = pred_temp)
new_data <- with(
  pred_temp,
  data.frame(
    excluded_sunni_10_yrs = sort(rep(c(0, 1), 1001)), 
    nlights_calib_mean = rep(seq(-5, 5, 0.01), 2),
    pop_gpw_sum = mean(pop_gpw_sum, na.rm = T), 
    bdist1  = mean(bdist1, na.rm = T),
    mountains_mean = mean(mountains_mean, na.rm = T),
    petroleum_s = mean(petroleum_s, na.rm = T), 
    ttime_mean = mean(ttime_mean, na.rm = T),
    landarea = mean(landarea, na.rm = T), 
    gcp_ppp = mean(gcp_ppp, na.rm = T), 
    country_name = "Saudi Arabia", 
    year = 2014
  )
)

preds <- predict(fe_pois, new_data, type="response", se.fit=TRUE)

new_data$pred <- preds$fit # predicted
new_data$lower <- preds$fit - (1.96*preds$se.fit) # lower bounds
new_data$upper <- preds$fit + (1.96*preds$se.fit) # upper bounds

plot_data_10yrs <- dplyr::select(
  new_data,
  c(
    excluded_sunni_10_yrs, nlights_calib_mean, 
    pred, lower, upper
  )
) %>% 
  mutate(
    pred = ifelse(pred < 0, 0, pred),
    upper = ifelse(upper < 0, 0, upper),
    lower = ifelse(lower < 0, 0, lower),
    excluded_sunni_10_yrs = ifelse(excluded_sunni_10_yrs == 1, 
                                   "Excluded Sunni Muslim groups",
                                   "No excluded Sunni Muslim groups"),
    label = "Within the last 10 years",
    excluded_sunni = excluded_sunni_10_yrs
  )

fe_pois <- glm(count ~ excluded_sunni_20_yrs + nlights_calib_mean + pop_gpw_sum + bdist1 + mountains_mean + petroleum_s + ttime_mean + landarea + gcp_ppp + offset(pop_gpw_sum) + as.factor(country_name) + as.factor(year), family = "poisson", data = pred_temp)
new_data <- with(
  pred_temp,
  data.frame(
    excluded_sunni_20_yrs = sort(rep(c(0, 1), 1001)), 
    nlights_calib_mean = rep(seq(-5, 5, 0.01), 2),
    pop_gpw_sum = mean(pop_gpw_sum, na.rm = T), 
    bdist1  = mean(bdist1, na.rm = T),
    mountains_mean = mean(mountains_mean, na.rm = T),
    petroleum_s = mean(petroleum_s, na.rm = T), 
    ttime_mean = mean(ttime_mean, na.rm = T),
    landarea = mean(landarea, na.rm = T), 
    gcp_ppp = mean(gcp_ppp, na.rm = T), 
    country_name = "Saudi Arabia", 
    year = 2014
  )
)

preds <- predict(fe_pois, new_data, type="response", se.fit=TRUE)

new_data$pred <- preds$fit # predicted
new_data$lower <- preds$fit - (1.96*preds$se.fit) # lower bounds
new_data$upper <- preds$fit + (1.96*preds$se.fit) # upper bounds

plot_data_20yrs <- dplyr::select(
  new_data,
  c(
    excluded_sunni_20_yrs, nlights_calib_mean, 
    pred, lower, upper
  )
) %>% 
  mutate(
    pred = ifelse(pred < 0, 0, pred),
    upper = ifelse(upper < 0, 0, upper),
    lower = ifelse(lower < 0, 0, lower),
    excluded_sunni_20_yrs = ifelse(excluded_sunni_20_yrs == 1, 
                                   "Excluded Sunni Muslim groups",
                                   "No excluded Sunni Muslim groups"),
    label = "Within the last 20 years",
    excluded_sunni = excluded_sunni_20_yrs
  )

fe_pois <- glm(count ~ excluded_sunni_30_yrs + nlights_calib_mean + pop_gpw_sum + bdist1 + mountains_mean + petroleum_s + ttime_mean + landarea + gcp_ppp + offset(pop_gpw_sum) + as.factor(country_name) + as.factor(year), family = "poisson", data = pred_temp)
new_data <- with(
  pred_temp,
  data.frame(
    excluded_sunni_30_yrs = sort(rep(c(0, 1), 1001)), 
    nlights_calib_mean = rep(seq(-5, 5, 0.01), 2),
    pop_gpw_sum = mean(pop_gpw_sum, na.rm = T), 
    bdist1  = mean(bdist1, na.rm = T),
    mountains_mean = mean(mountains_mean, na.rm = T),
    petroleum_s = mean(petroleum_s, na.rm = T), 
    ttime_mean = mean(ttime_mean, na.rm = T),
    landarea = mean(landarea, na.rm = T), 
    gcp_ppp = mean(gcp_ppp, na.rm = T), 
    country_name = "Saudi Arabia", 
    year = 2014
  )
)

preds <- predict(fe_pois, new_data, type="response", se.fit=TRUE)

new_data$pred <- preds$fit # predicted
new_data$lower <- preds$fit - (1.96*preds$se.fit) # lower bounds
new_data$upper <- preds$fit + (1.96*preds$se.fit) # upper bounds

plot_data_30yrs <- dplyr::select(
  new_data,
  c(
    excluded_sunni_30_yrs, nlights_calib_mean, 
    pred, lower, upper
  )
) %>% 
  mutate(
    pred = ifelse(pred < 0, 0, pred),
    upper = ifelse(upper < 0, 0, upper),
    lower = ifelse(lower < 0, 0, lower),
    excluded_sunni_30_yrs = ifelse(excluded_sunni_30_yrs == 1, 
                                   "Excluded Sunni Muslim groups",
                                   "No excluded Sunni Muslim groups"),
    label = "Within the last 30 years",
    excluded_sunni = excluded_sunni_30_yrs
  )

plot_data <- bind_rows(
  plot_data_10yrs, 
  plot_data_20yrs,
  plot_data_30yrs
) 
library('grid')
library('ggplot2')
inital_plot_10_yrs <- ggplot(plot_data, aes(x = nlights_calib_mean, y = pred, ymin = lower, ymax = upper, col = excluded_sunni), linetype =  as.factor(excluded_sunni)) + 
  geom_line(size = 1.1) + 
  scale_colour_brewer(palette = "Dark2") + 
  scale_fill_brewer(palette = "Dark2") + 
  geom_ribbon(linetype = 2,
              size = 1.1,
              alpha=0.08) + 
  theme_bw() + 
  facet_wrap(~label) +
  labs(x = "State capacity (operationalized through nighttime lights)", 
       y = "Predicted number of foreign fighters",
       col = "Geospatial regions",
       caption = "Plotted using state and year as control variables and predicted standard error.") +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.caption = element_text(size = 12),
        strip.text = element_text( size = 18),
        legend.text = element_text(size = 16),
        legend.box = "vertical", legend.margin = margin(),
        panel.spacing = unit(1.5, "lines")) 
dir.create("plot_output", showWarnings = F)
ggsave("plot_output/figure_5.png", inital_plot_10_yrs, units = "in", width = 14, height = 7)



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


matched_10_yrs_tr <- extract.pois.fe(full_model_matched_10yrs)
matched_20_yrs_tr <- extract.pois.fe(full_model_matched_20yrs)
matched_30_yrs_tr <- extract.pois.fe(full_model_matched_30yrs)

matched_data_regs <- texreg(
  list(matched_10_yrs_tr, matched_20_yrs_tr, matched_30_yrs_tr), stars = 0.05
)

write.table(matched_data_regs, file = "summary_stats/appendix_table_6.txt")

fe_10_yrs_int_check <- fepois(count ~ excluded_sunni_10_yrs * nlights_calib_mean + log(pop_gpw_sum) + offset(log(pop_gpw_sum)) | country_name + year, data = data_for_analysis)
fe_20_yrs_int_check <- fepois(count ~ excluded_sunni_20_yrs * nlights_calib_mean + log(pop_gpw_sum) + offset(log(pop_gpw_sum)) | country_name + year, data = data_for_analysis)
fe_30_yrs_int_check <- fepois(count ~ excluded_sunni_30_yrs * nlights_calib_mean + log(pop_gpw_sum) + offset(log(pop_gpw_sum)) | country_name + year, data = data_for_analysis)

appendix_table_3 <- texreg::texreg(
  list(
    extract.pois.fe(fe_10_yrs_int_check), 
    extract.pois.fe(fe_20_yrs_int_check), 
    extract.pois.fe(fe_30_yrs_int_check)
  ), 
  stars = 0.05)
write.table(appendix_table_3, file = "summary_stats/appendix_table_3.txt")
