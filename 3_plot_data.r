## ----------------------------------- ##
##    Plot data Edgerton (2023 JCR)    ## 
## ----------------------------------- ##
rm(list = ls())

## Create folder for plots
dir.create("plot_output", showWarnings = F)
dir.create("summary_stats", showWarnings = F)

## Load data
wb_unemployment     <- readr::read_csv("replication_data/WB_Unemployment_Males.csv", show_col_types = FALSE)
isis_unemployment   <- readRDS("replication_data/isis_unemployment.rds")
religious_knowledge <- readRDS("replication_data/religious_knowledge.rds")
plot_heat_map       <- readRDS("replication_data/plot_heat_map.rds")
daily_inflow        <- readRDS("replication_data/daily_inflow.rds")
load("data_for_analysis.Rda")

## Plot unemployment data z-tests
# Format unemployment data
colnames(wb_unemployment) <- tolower(gsub(" ", "_", colnames(wb_unemployment)))
colnames(wb_unemployment)[3:ncol(wb_unemployment)] <- paste0("X", colnames(wb_unemployment)[3:ncol(wb_unemployment)])
wb_unemployment$country_name <- toupper(wb_unemployment$country_name)

## Summarize data
unemployment_z_tests <- left_join(
  isis_unemployment,
  wb_unemployment, 
  by = c(
    "home_country" = "country_name"
  )
) %>% dplyr::select(
    -c(
      country_code, X2008, X2009, X2010, X2011, X2012
    )
  )  %>% filter(
    !is.na(X2013)
  ) %>%  ## World Bank
  mutate(
    z_stat = (prop - X2013/100)/(sqrt((X2013/100*(1-X2013/100))/denominator)),
    p_value = pnorm(z_stat),
    ub = prop - X2013/100 + qnorm(1-0.025/18)*sqrt(X2013/100*(1-X2013/100)/denominator),
    lb = prop - X2013/100 - qnorm(1-0.025/18)*sqrt(X2013/100*(1-X2013/100)/denominator),
    plot_mean = prop - X2013/100,
    significant = ifelse((lb > 0 | ub < 0), "Statistically Significant", "Not statistically significant")
  ) %>% filter(
    !is.na(plot_mean)
  ) %>% arrange(
    -plot_mean
  ) %>% dplyr::rename(
    volunteers = denominator
  ) 

unemployment_z_tests$label <- "One-sample z-tests difference in proportion"
one_sided_z_test <- ggplot(unemployment_z_tests, aes(x = plot_mean, y = reorder(home_country, ub), xmin= lb, xmax = ub, col = significant)) + 
  geom_errorbar(size = 2) + 
  geom_vline(xintercept = 0, size = 1.5, linetype = "dashed") + 
  scale_colour_brewer(palette = "Dark2") + 
  labs(x = "Difference in proportion unemployed", 
       y = "") + 
  theme_bw() + 
  facet_wrap(~label) + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        plot.caption = element_text(face = "bold", hjust = 0, size = 20),
        strip.text = element_text( size = 18),
        legend.text = element_text(size = 16),
        legend.box = "vertical", legend.margin = margin())

## Plot religious knowledge

rel_ed_plot <- ggplot(religious_knowledge, aes(y = reorder(home_country, n), x = n, fill = sharia_experience)) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("#E7298A", "#66A61E", "#7570B3")) + 
  labs(x = "Count of combatants", 
       y = "") + 
  theme_bw() + 
  facet_wrap(~label) + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        plot.caption = element_text(face = "bold", hjust = 0, size = 20),
        strip.text = element_text( size = 18),
        legend.text = element_text(size = 16),
        legend.box = "vertical", legend.margin = margin()) 
cowplot::plot_grid(one_sided_z_test, rel_ed_plot, ncol = 2)
ggsave("plot_output/figure_3.png", units = "in", width = 17, height = 8)

## Religious knowledge output
religious_prop <- prop.table(table(religious_knowledge$sharia_experience))
saveRDS(religious_prop, file = "summary_stats/religious_prop.rds")


selected_vars <- dplyr::select(
  data_for_analysis,
  c(count, excluded_sunni_10_yrs, excluded_sunni_20_yrs, excluded_sunni_30_yrs, nlights_calib_mean,
    pop_gpw_sum, bdist1, mountains_mean, petroleum_s, ttime_mean, landarea, gcp_ppp,
    p_polity2, al_ethnic, al_language, al_religion, wdi_unempmne
  )
) %>% 
  mutate(
    petroleum_s  = tidyr::replace_na(petroleum_s, 0), 
    pop_gpw_sum  = log(pop_gpw_sum),
    bdist1       = log(bdist1),
    gcp_ppp      = log(gcp_ppp),
    ttime_mean   = log(ttime_mean + 1),
    landarea     = log(landarea + 1),
    wdi_unempmne = log(wdi_unempmne)
  )

summarize_data <- data.frame(
  matrix(
    NA, nrow = 17, ncol = 8
  )
)

summarize_data[,1] <- colnames(selected_vars)
summarize_data[,2] <- round(apply(selected_vars, 2, function(x){(sum(!is.na(x)))}), 2)
summarize_data[,3] <- round(apply(selected_vars, 2, min, na.rm = T), 2)
summarize_data[,4] <- round(apply(selected_vars, 2, quantile, na.rm = T, 0.25), 2)
summarize_data[,5] <- round(apply(selected_vars, 2, mean, na.rm = T), 2)
summarize_data[,6] <- round(apply(selected_vars, 2, median, na.rm = T), 2)
summarize_data[,7] <- round(apply(selected_vars, 2, quantile, na.rm = T, 0.75), 2)
summarize_data[,8] <- round(apply(selected_vars, 2, max, na.rm = T), 2)

for (i in 2:ncol(summarize_data))
{
  summarize_data[,i] <- paste0("&$", summarize_data[,i], "$")
}
print(summarize_data, row.names = F)

write.csv(summarize_data, file = "summary_stats/summary_data.csv", row.names = F)


## Plot heatmap
leaflet(plot_heat_map,
        options = leafletOptions(zoomControl = FALSE,
                                 dragging = F)) %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
  setView( median(plot_heat_map$Longitude, na.rm = T) + 10, median(plot_heat_map$Latitude), zoom = 3) %>%
  addHeatmap(
    lng = ~Longitude, 
    lat = ~Latitude, 
    intensity = ~n,
    blur = 11, max = max(log(plot_heat_map$n + 1)), radius = 6) 


ggplot(daily_inflow, aes(x = date_of_entry, y = n)) + 
  geom_smooth(se = F, span = 1/10, size = 1.5, col = "#1B9E77") + 
  geom_point(alpha = I(1/3)) + 
  geom_line(alpha = I(1/4), size = 1) + 
  theme_bw() + 
  facet_wrap(~label) + 
  labs(x = "Date of entry", y = "Count of Islamic State combatants") + 
  scale_y_continuous(limits = c(0, 50)) + 
  annotate("rect", xmin = as.Date("2012-01-01"), xmax = as.Date("2014-08-07"), ymin = 0, ymax = 50,
           alpha = .1,fill = "#D95F02") + 
  annotate("rect", xmin = as.Date("2014-08-07"), xmax = as.Date("2015-01-01"), ymin = 0, ymax = 50,
           alpha = .1,fill = "#7570B3") + 
  geom_vline(xintercept = as.Date("2014-08-07"), size = 1, linetype = "dashed") + 
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        strip.text = element_text( size = 14))
ggsave("plot_output/figure_2.png", units = "in", width = 9, height = 6)

load("model_output/full_nb_re_10yrs.Rda")
load("model_output/full_nb_re_20yrs.Rda")
load("model_output/full_nb_re_30yrs.Rda")

coef_table_10 <- data.frame(
  model = "Within 10 years", 
  mean = coef(full_nb_re_10yrs),
  lb = coef(full_nb_re_10yrs) - 1.96 * summary(full_nb_re_10yrs)$coefficients[,2],
  ub = coef(full_nb_re_10yrs) + 1.96 * summary(full_nb_re_10yrs)$coefficients[,2],
  names = c("Constant", "Excluded Sunni Muslim group", "Nighttime lights", "Population", "Year")
)

coef_table_20 <- data.frame(
  model = "Within 20 years", 
  mean = coef(full_nb_re_20yrs),
  lb = coef(full_nb_re_20yrs) - 1.96 * summary(full_nb_re_20yrs)$coefficients[,2],
  ub = coef(full_nb_re_20yrs) + 1.96 * summary(full_nb_re_20yrs)$coefficients[,2],
  names = c("Constant", "Excluded Sunni Muslim group", "Nighttime lights", "Population", "Year")
)

coef_table_30 <- data.frame(
  model = "Within 30 years", 
  mean = coef(full_nb_re_30yrs),
  lb = coef(full_nb_re_30yrs) - 1.96 * summary(full_nb_re_30yrs)$coefficients[,2],
  ub = coef(full_nb_re_30yrs) + 1.96 * summary(full_nb_re_30yrs)$coefficients[,2],
  names = c("Constant", "Excluded Sunni Muslim group", "Nighttime lights", "Population", "Year")
)

all_coef_quasi_poisson <- bind_rows(
  coef_table_10,
  coef_table_20,
  coef_table_30
) %>% 
  filter(
    names != "Constant"
  ) %>% 
  mutate(
    names = as.character(names),
    names = ifelse(names == "Excluded Sunni Muslim group", "Excluded Sunni\nMuslim group", names),
    names = ifelse(names == "Nighttime lights", "Nighttime\nlights", names)
  )

ggplot(all_coef_quasi_poisson, aes(x = mean, y = reorder(names, -mean), xmin = lb, xmax = ub)) + 
  geom_errorbar(width = 0.25, size = 1) + 
  facet_wrap(~model) + 
  geom_vline(xintercept = 0, linetype = "dashed", size = 1.2) + 
  theme_bw() + 
  labs(x = "Quasi Poisson regression results", y = "") + 
  scale_fill_brewer(palette = "Dark2") + 
  theme(legend.position = "none",
        legend.title = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.caption = element_text(face = "bold", hjust = 0, size = 20),
        strip.text = element_text( size = 18),
        legend.text = element_text(size = 16),
        legend.box = "vertical", legend.margin = margin(),
        panel.spacing = unit(1.5, "lines"))

ggsave("plot_output/si_figure_1.png", units = "in", width = 12.5, height = 5)


## Jack knife data
load("model_output/jacknife_output.Rda")
jacknife_output_plot <- reshape2::melt(jacknife_output)
jacknife_output_plot$time <- "30 years"
jacknife_output_plot$time <- ifelse(grepl("10", jacknife_output_plot$variable), "10 years", jacknife_output_plot$time)
jacknife_output_plot$time <- ifelse(grepl("20", jacknife_output_plot$variable), "20 years", jacknife_output_plot$time)
jacknife_output_plot$cat  <- ifelse(grepl("exclusion", jacknife_output_plot$variable), "Sunni Muslim group exclusion", "Nighttime lights")
jacknife_output_plot$type  <- ifelse(grepl("pvalue", jacknife_output_plot$variable), "p-value", "Coefficient estimate")
jacknife_output_plot$name = paste0(jacknife_output_plot$cat, "\n(", jacknife_output_plot$time, " years)")
jacknife_output_plot$name <- gsub("Sunni Muslim ", "Sunni Muslim\n", jacknife_output_plot$name)
boxplot_jackknife <- ggplot(subset(jacknife_output_plot, type != "p-value"), aes(y = value, x = name, fill = time)) + 
  geom_boxplot() + 
  facet_wrap(~cat, scales = "free") + 
  theme_bw() + 
  labs(x = "", y = "Pseudo jackknife estimated values") + 
  scale_fill_brewer(palette = "Dark2") + 
  theme(legend.position = "none",
        legend.title = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.caption = element_text(face = "bold", hjust = 0, size = 20),
        strip.text = element_text( size = 18),
        legend.text = element_text(size = 16),
        legend.box = "vertical", legend.margin = margin(),
        panel.spacing = unit(1.5, "lines"))

ggsave("plot_output/si_figure_2.png", boxplot_jackknife, units = "in", width = 12.5, height = 7)
