# Pre-Processing ----------
crime_model <- crime_hourly %>%
  mutate(
    date = as.Date(datetime),
    dst_date = as.Date(dst_change_datetime),
    running_day = as.integer(difftime(date, dst_date, units = "days")),
    weekday_name = wday(date, label = TRUE, abbr = FALSE),
    
    # Pre-DST week dummies
    pre_monday    = as.integer(running_day == -6 & weekday == 2),
    pre_tuesday   = as.integer(running_day == -5 & weekday == 3),
    pre_wednesday = as.integer(running_day == -4 & weekday == 4),
    pre_thursday  = as.integer(running_day == -3 & weekday == 5),
    pre_friday    = as.integer(running_day == -2 & weekday == 6),
    pre_saturday  = as.integer(running_day == -1 & weekday == 7),
    pre_sunday    = as.integer(running_day == -7 & weekday == 1),
    
    # DST Sunday
    dst_sunday    = as.integer(running_day == 0 & weekday == 1),
    
    # Post-DST week dummies
    post_monday    = as.integer(running_day == 1 & weekday == 2),
    post_tuesday   = as.integer(running_day == 2 & weekday == 3),
    post_wednesday = as.integer(running_day == 3 & weekday == 4),
    post_thursday  = as.integer(running_day == 4 & weekday == 5),
    post_friday    = as.integer(running_day == 5 & weekday == 6),
    post_saturday  = as.integer(running_day == 6 & weekday == 7),
    post_sunday    = as.integer(running_day == 7 & weekday == 1)
  )

# Table 3 --------------
p_load(lmtest, sandwich)
p_load(fixest)

model_ols_total3 <- lm(crime_count ~ 
                        pre_sunday + pre_monday + pre_tuesday + pre_wednesday + pre_thursday + pre_friday + pre_saturday + 
                        dst_sunday +
                        post_monday + post_tuesday + post_wednesday + post_thursday + post_friday + post_saturday + post_sunday,
                      data = crime_model)
summary(model_ols_total3)

model_poisson_total3 <- glm(crime_count ~ 
                              pre_sunday + pre_monday + pre_tuesday + pre_wednesday + pre_thursday + pre_friday + pre_saturday +
                             dst_sunday +
                             post_monday + post_tuesday + post_wednesday + post_thursday + post_friday + post_saturday + post_sunday,
                           family = poisson(link = "log"),
                           data = crime_model)
robust_vcov <- vcovHC(model_poisson_total3, type = "HC3")
coeftest(model_poisson_total3, vcov = robust_vcov)

model_ols_total_theft3 <- lm(theft ~
                        pre_sunday + pre_monday + pre_tuesday + pre_wednesday + pre_thursday + pre_friday + pre_saturday +
                        dst_sunday +
                        post_monday + post_tuesday + post_wednesday + post_thursday + post_friday + post_saturday + post_sunday,
                      data = crime_model)
summary(model_ols_total_theft3)

model_poisson_total_theft3 <- glm(theft ~ 
                             pre_sunday + pre_monday + pre_tuesday + pre_wednesday + pre_thursday + pre_friday + pre_saturday +
                             dst_sunday +
                             post_monday + post_tuesday + post_wednesday + post_thursday + post_friday + post_saturday + post_sunday,
                           family = poisson(link = "log"),
                           data = crime_model)
robust_vcov <- vcovHC(model_poisson_total_theft3, type = "HC3")
coeftest(model_poisson_total_theft3, vcov = robust_vcov)

model_ols_total_robbery3 <- lm(robbery ~
                              pre_sunday + pre_monday + pre_tuesday + pre_wednesday + pre_thursday + pre_friday + pre_saturday +
                              dst_sunday +
                              post_monday + post_tuesday + post_wednesday + post_thursday + post_friday + post_saturday + post_sunday,
                            data = crime_model)
summary(model_ols_total_robbery3)

model_poisson_total_robbery3 <- glm(robbery ~ 
                                   pre_sunday + pre_monday + pre_tuesday + pre_wednesday + pre_thursday + pre_friday + pre_saturday +
                                   dst_sunday +
                                   post_monday + post_tuesday + post_wednesday + post_thursday + post_friday + post_saturday + post_sunday,
                                 family = poisson(link = "log"),
                                 data = crime_model)
robust_vcov <- vcovHC(model_poisson_total_robbery3, type = "HC3")
coeftest(model_poisson_total_robbery3, vcov = robust_vcov)

# Table 3.1
model_ols_total3 <- lm(crime_count ~ 
                         pre_sunday + pre_monday + pre_tuesday + pre_wednesday + pre_thursday + pre_friday + pre_saturday + 
                         dst_sunday +
                         post_monday + post_tuesday + post_wednesday + post_thursday + post_friday + post_saturday + post_sunday,
                       data = crime_model)
summary(model_ols_total3)

model_poisson_total3 <- glm(crime_count ~ 
                              pre_sunday + pre_monday + pre_tuesday + pre_wednesday + pre_thursday + pre_friday + pre_saturday +
                              dst_sunday +
                              post_monday + post_tuesday + post_wednesday + post_thursday + post_friday + post_saturday + post_sunday,
                            family = poisson(link = "log"),
                            data = crime_model)
robust_vcov <- vcovHC(model_poisson_total3, type = "HC3")
coeftest(model_poisson_total3, vcov = robust_vcov)

model_ols_total_theft3 <- lm(theft ~
                               pre_sunday + pre_monday + pre_tuesday + pre_wednesday + pre_thursday + pre_friday + pre_saturday +
                               dst_sunday +
                               post_monday + post_tuesday + post_wednesday + post_thursday + post_friday + post_saturday + post_sunday,
                             data = crime_model)
summary(model_ols_total_theft3)

model_poisson_total_theft3 <- glm(theft ~ 
                                    pre_sunday + pre_monday + pre_tuesday + pre_wednesday + pre_thursday + pre_friday + pre_saturday +
                                    dst_sunday +
                                    post_monday + post_tuesday + post_wednesday + post_thursday + post_friday + post_saturday + post_sunday,
                                  family = poisson(link = "log"),
                                  data = crime_model)
robust_vcov <- vcovHC(model_poisson_total_theft3, type = "HC3")
coeftest(model_poisson_total_theft3, vcov = robust_vcov)

model_ols_total_robbery3 <- lm(robbery ~
                                 pre_sunday + pre_monday + pre_tuesday + pre_wednesday + pre_thursday + pre_friday + pre_saturday +
                                 dst_sunday +
                                 post_monday + post_tuesday + post_wednesday + post_thursday + post_friday + post_saturday + post_sunday,
                               data = crime_model)
summary(model_ols_total_robbery3)

model_poisson_total_robbery3 <- glm(robbery ~ 
                                      pre_sunday + pre_monday + pre_tuesday + pre_wednesday + pre_thursday + pre_friday + pre_saturday +
                                      dst_sunday +
                                      post_monday + post_tuesday + post_wednesday + post_thursday + post_friday + post_saturday + post_sunday,
                                    family = poisson(link = "log"),
                                    data = crime_model)
robust_vcov <- vcovHC(model_poisson_total_robbery3, type = "HC3")
coeftest(model_poisson_total_robbery3, vcov = robust_vcov)
# Table 4 ------------------------------
model_ols_total4 <- lm(crime_count ~ 
                        dst_sunday +
                        post_monday + post_tuesday + post_wednesday + post_thursday + post_friday + post_saturday + post_sunday +
                        darkness + 
                        factor(hour) + factor(weekday) + factor(month) + factor(year),
                      data = crime_model)
summary(model_ols_total4)

model_poisson_total4 <- glm(crime_count ~ 
                             dst_sunday +
                             post_monday + post_tuesday + post_wednesday + post_thursday + post_friday + post_saturday + post_sunday +
                             darkness + 
                             factor(hour) + factor(weekday) + factor(month) + factor(year),
                           family = poisson(link = "log"),
                           data = crime_model)
robust_vcov <- vcovHC(model_poisson_total4, type = "HC3")
coeftest(model_poisson_total4, vcov = robust_vcov)

model_ols_total_theft4 <- lm(theft ~
                              dst_sunday +
                              post_monday + post_tuesday + post_wednesday + post_thursday + post_friday + post_saturday + post_sunday +
                              darkness + 
                              factor(hour) + factor(weekday) + factor(month) + factor(year),
                            data = crime_model)
summary(model_ols_total_theft4)

model_poisson_total_theft4 <- glm(theft ~ 
                                   dst_sunday +
                                   post_monday + post_tuesday + post_wednesday + post_thursday + post_friday + post_saturday + post_sunday +
                                   darkness + 
                                   factor(hour) + factor(weekday) + factor(month) + factor(year),
                                 family = poisson(link = "log"),
                                 data = crime_model)
robust_vcov <- vcovHC(model_poisson_total_theft4, type = "HC3")
coeftest(model_poisson_total_theft4, vcov = robust_vcov)

model_ols_total_robbery4 <- lm(robbery ~
                                dst_sunday +
                                post_monday + post_tuesday + post_wednesday + post_thursday + post_friday + post_saturday + post_sunday +
                                darkness + 
                                factor(hour) + factor(weekday) + factor(month) + factor(year),
                              data = crime_model)
summary(model_ols_total_robbery4)

model_poisson_total_robbery4 <- glm(robbery ~ 
                                     dst_sunday +
                                     post_monday + post_tuesday + post_wednesday + post_thursday + post_friday + post_saturday + post_sunday +
                                     darkness + 
                                     factor(hour) + factor(weekday) + factor(month) + factor(year),
                                   family = poisson(link = "log"),
                                   data = crime_model)
robust_vcov <- vcovHC(model_poisson_total_robbery4, type = "HC3")
coeftest(model_poisson_total_robbery4, vcov = robust_vcov)

# Regression Tables ---------------
library(pacman)
p_load(modelsummary, kableExtra)

models <- list(
  "Total Crime (OLS)" = model_ols_total,
  "Total Crime (Poisson)" = model_poisson_total,
  "Robbery (OLS)" = model_ols_total_robbery,
  "Robbery (Poisson)" = model_poisson_total_robbery,
  "Theft (OLS)" = model_ols_total_theft,
  "Theft (Poisson)" = model_poisson_total_theft
)

modelsummary(
  models,
  output = "latex",
  statistic = c("std.error", "p.value"), # You can also use "conf.int" if desired
  stars = c('*' = .1, '**' = .05, '***' = .01),
  coef_rename = c(
    "pre_monday" = "Pre-DST change: Monday",
    "pre_tuesday" = "Pre-DST change: Tuesday",
    "pre_wednesday" = "Pre-DST change: Wednesday",
    "pre_thursday" = "Pre-DST change: Thursday",
    "pre_friday" = "Pre-DST change: Friday",
    "pre_saturday" = "Pre-DST change: Saturday",
    "pre_sunday" = "Pre-DST change: Sunday",
    "dst_sunday" = "DST: Sunday",
    "post_monday" = "Post-DST change: Monday",
    "post_tuesday" = "Post-DST change: Tuesday",
    "post_wednesday" = "Post-DST change: Wednesday",
    "post_thursday" = "Post-DST change: Thursday",
    "post_friday" = "Post-DST change: Friday",
    "post_saturday" = "Post-DST change: Saturday",
    "post_sunday" = "Post-DST change: Sunday",
    "darkness" = "Darkness"
  ),
  gof_omit = ".*", # hide goodness-of-fit stats if you want to match the image
  notes = c(
    "All regressions include hourly data and day-of-the-week, hour, month, and year dummies.",
    "* p < 0.1; ** p < 0.05; *** p < 0.01"
  ),
  title = "Spring Daylight Saving Time (DST) and Crime"
)

# RDD -------------

# Residualized Model using basic controls
resid_model <- lm(crime_count ~ factor(sunday) + factor(year) + darkness, data = crime_model_clean)
resid_modelr <- lm(robbery ~ factor(sunday) + factor(year) + darkness, data = crime_model_clean)
resid_modelt <- lm(theft ~ factor(sunday) + factor(year) + darkness, data = crime_model_clean)
# Residualized Models using all controls
resid_model2 <- lm(crime_count ~ factor(month_weekday) + factor(hour_weekday) + factor(year) + darkness, data = crime_model)
resid_modelr2 <- lm(robbery ~ factor(month_weekday) + factor(hour_weekday) + factor(year) + darkness, data = crime_model)
resid_modelt2 <- lm(theft ~ factor(month_weekday) + factor(hour_weekday) + factor(year) + darkness, data = crime_model)

crime_model_clean <- crime_model_clean %>%
  mutate(resid_crime1 = resid(resid_model),
         resid_robbery1 = resid(resid_modelr),
         resid_theft1 = resid(resid_modelt),
         resid_crime2 = resid(resid_model2),
         resid_robbery2 = resid(resid_modelr2),
         resid_theft2 = resid(resid_modelt2))

# BANDWIDTH 72h
summary(rdrobust(y = crime_model_clean$resid_crime2, 
                 x = crime_model_clean$running,
                 c = 0, 
                 p = 1,
                 h = 72,
                 vce = "hc0",
                 kernel = "triangular"))

summary(rdrobust(y = crime_model_clean$resid_robbery2, 
                 x = crime_model_clean$running,
                 c = 0, 
                 p = 1,
                 h = 72,
                 vce = "hc0",
                 kernel = "triangular"))

summary(rdrobust(y = crime_model_clean$resid_theft2, 
                 x = crime_model_clean$running,
                 c = 0, 
                 p = 1,
                 h = 72,
                 vce = "hc0",
                 kernel = "triangular"))
# BANDWIDTH 96h
summary(rdrobust(y = crime_model_clean$resid_crime2, 
                 x = crime_model_clean$running,
                 c = 0, 
                 p = 1,
                 h = 96,
                 vce = "hc0",
                 kernel = "triangular"))

summary(rdrobust(y = crime_model_clean$resid_robbery2, 
                 x = crime_model_clean$running,
                 c = 0, 
                 p = 1,
                 h = 96,
                 vce = "hc0",
                 kernel = "triangular"))

summary(rdrobust(y = crime_model_clean$resid_theft2, 
                 x = crime_model_clean$running,
                 c = 0, 
                 p = 1,
                 h = 96,
                 vce = "hc0",
                 kernel = "triangular"))

# BANDWIDTH 96h p=2
summary(rdrobust(y = crime_model_clean$resid_crime2, 
                 x = crime_model_clean$running,
                 c = 0, 
                 p = 2,
                 h = 96,
                 vce = "hc0",
                 kernel = "triangular"))

summary(rdrobust(y = crime_model_clean$resid_robbery2, 
                 x = crime_model_clean$running,
                 c = 0, 
                 p = 2,
                 h = 96,
                 vce = "hc0",
                 kernel = "triangular"))

summary(rdrobust(y = crime_model_clean$resid_theft2, 
                 x = crime_model_clean$running,
                 c = 0, 
                 p = 2,
                 h = 96,
                 vce = "hc0",
                 kernel = "triangular"))

# BANDWIDTH 96h p=4
summary(rdrobust(y = crime_model_clean$resid_crime2, 
                 x = crime_model_clean$running,
                 c = 0, 
                 p = 4,
                 h = 96,
                 vce = "hc0",
                 kernel = "triangular"))

summary(rdrobust(y = crime_model_clean$resid_robbery2, 
                 x = crime_model_clean$running,
                 c = 0, 
                 p = 4,
                 h = 96,
                 vce = "hc0",
                 kernel = "triangular"))

summary(rdrobust(y = crime_model_clean$resid_theft2, 
                 x = crime_model_clean$running,
                 c = 0, 
                 p = 4,
                 h = 96,
                 vce = "hc0",
                 kernel = "triangular"))

# BANDWIDTH 120h
summary(rdrobust(y = crime_model_clean$resid_crime2, 
                 x = crime_model_clean$running,
                 c = 0, 
                 p = 1,
                 h = 120,
                 vce = "hc0",
                 kernel = "triangular"))

summary(rdrobust(y = crime_model_clean$resid_robbery2, 
                 x = crime_model_clean$running,
                 c = 0, 
                 p = 1,
                 h = 120,
                 vce = "hc0",
                 kernel = "triangular"))

summary(rdrobust(y = crime_model_clean$resid_theft2, 
                 x = crime_model_clean$running,
                 c = 0, 
                 p = 1,
                 h = 120,
                 vce = "hc0",
                 kernel = "triangular"))

# Plot with residualized counts
pdf("Figures/rdd_plot_total_resid_munyo_168p0.pdf")
rdplot(y = crime_model_clean$resid_crime1, 
       x = crime_model_clean$running,
       c = 0, 
       h = 168, 
       p = 0, 
       subset = (crime_model_clean$running >= -336 & crime_model_clean$running <= 336),
       binselect = "esmv", 
       kernel = "triangular",
       x.label = "Hours From Cutoff",
       y.label = "Total Crime per Hour (Residualized)",
       title="")
dev.off()
pdf("Figures/rdd_plot_robbery_resid_munyo_168p0.pdf")
rdplot(y = crime_model_clean$resid_robbery1, 
       x = crime_model_clean$running,
       c = 0, 
       h = 168, 
       p = 0, 
       subset = (crime_model_clean$running >= -336 & crime_model_clean$running <= 336),
       binselect = "esmv", 
       kernel = "triangular",
       x.label = "Hours From Cutoff",
       y.label = "Robberies per Hour (Residualized)",
       title="")
dev.off()
pdf("Figures/rdd_plot_theft_resid_munyo_168p0.pdf")
rdplot(y = crime_model_clean$resid_theft1, 
       x = crime_model_clean$running,
       c = 0, 
       h = 168, 
       p = 0, 
       subset = (crime_model_clean$running >= -336 & crime_model_clean$running <= 336),
       binselect = "esmv", 
       kernel = "triangular",
       x.label = "Hours From Cutoff",
       y.label = "Thefts per Hour (Residualized)",
       title="")
dev.off()
# plot with raw counts
pdf("Figures/rdd_plot_total_raw_96p1.pdf")
rdplot(y = crime_model_clean$crime_count, 
       x = crime_model_clean$running,
       c = 0, 
       #h = 168, 
       p = 2, 
       subset = (crime_model_clean$running >= -336 & crime_model_clean$running <= 336),
       binselect = "esmv", 
       kernel = "triangular",
       x.label = "Hours From Cutoff",
       y.label = "Total Crime per Hour")
dev.off()
pdf("Figures/rdd_plot_robberies_raw_96p1.pdf")
rdplot(y = crime_model_clean$robbery, 
       x = crime_model_clean$running,
       c = 0, 
       h = 96, 
       p = 1, 
       subset = (crime_model_clean$running >= -336 & crime_model_clean$running <= 336), #&
         #(crime_model_clean$running != 0),
       binselect = "esmv", 
       kernel = "triangular",
       x.label = "Hours From Cutoff",
       y.label = "Robberies per Hour")
dev.off()
pdf("Figures/rdd_plot_theft_raw_96p1.pdf")
rdplot(y = crime_model_clean$theft, 
       x = crime_model_clean$running,
       c = 0, 
       h = 96, 
       p = 1, 
       subset = (crime_model_clean$running >= -336 & crime_model_clean$running <= 336),
       binselect = "esmv", 
       kernel = "triangular",
       x.label = "Hours From Cutoff",
       y.label = "Thefts per Hour")
dev.off()
# plot with residuals 
pdf("Figures/rdd_plot__total_resid.pdf")
rdplot(y = crime_model_clean$resid_crime, 
       x = crime_model_clean$running,
       c = 0, 
       h = 336, 
       p = 1, 
       subset = (crime_model_clean$running >= -336 & crime_model_clean$running <= 336),
       binselect = "esmv", 
       kernel = "triangular",
       title = "RDD Estimate (Residualized)",
       x.label = "Hours From Cutoff",
       y.label = "Residualized Total Crime per Hour")
dev.off()

rdplot(y = crime_model_clean$resid_crime, 
       x = crime_model_clean$running,
       c = 0, 
       h = 120, 
       p = 1, 
       subset = (crime_model_clean$running >= -120 & crime_model_clean$running <= 120),
       binselect = "esmv", 
       kernel = "triangular")

crime_model_clean$sunday <- as.integer(weekdays(crime_model_clean$datetime) == "Sunday")

covariates_munyo <- model.matrix(~ factor(sunday) + factor(darkness) + factor(year), data = crime_model_clean)[, -1]
covariates <- model.matrix(~ factor(month) + factor(weekday) + factor(hour) + factor(darkness), data = crime_model_clean)[, -1]

# RDD Batch All Covs -------
model_rdd96 <- rdrobust(y = crime_model_clean$crime_count, 
                      x = crime_model_clean$running,
                      covs = covariates,
                      c = 0, 
                      h = 96, 
                      p = 1, 
                      kernel = "triangular")

model_rdd72 <- rdrobust(y = crime_model_clean$crime_count, 
                         x = crime_model_clean$running,
                         covs = covariates,
                         c = 0, 
                         h = 72, 
                         p = 1, 
                         kernel = "triangular")

model_rdd120 <- rdrobust(y = crime_model_clean$crime_count, 
                         x = crime_model_clean$running,
                         covs = covariates,
                         c = 0, 
                         h = 120, 
                         p = 1, 
                         kernel = "triangular")

model_rdd96_2 <- rdrobust(y = crime_model_clean$crime_count, 
                         x = crime_model_clean$running,
                         covs = covariates,
                         c = 0, 
                         h = 96, 
                         p = 2, 
                         kernel = "triangular")

model_rdd96_4 <- rdrobust(y = crime_model_clean$crime_count, 
                         x = crime_model_clean$running,
                         covs = covariates,
                         c = 0, 
                         h = 96, 
                         p = 4, 
                         kernel = "triangular")

summary(model_rdd96)
summary(model_rdd72)
summary(model_rdd120)
summary(model_rdd96_2)
summary(model_rdd96_4)

# RDD Batch Munyo Covs
model_rdd96m <- rdrobust(y = crime_model_clean$crime_count, 
                        x = crime_model_clean$running,
                        covs = covariates_munyo,
                        c = 0, 
                        h = 96, 
                        p = 1, 
                        vce = "HC3",
                        kernel = "triangular")

model_rdd72m <- rdrobust(y = crime_model_clean$crime_count, 
                        x = crime_model_clean$running,
                        covs = covariates_munyo,
                        c = 0, 
                        h = 72, 
                        p = 1, 
                        vce = "HC3",
                        kernel = "triangular")

model_rdd120m <- rdrobust(y = crime_model_clean$crime_count, 
                         x = crime_model_clean$running,
                         covs = covariates_munyo,
                         c = 0, 
                         h = 120, 
                         p = 1, 
                         vce = "HC3",
                         kernel = "triangular")

model_rdd96_2m <- rdrobust(y = crime_model_clean$crime_count, 
                          x = crime_model_clean$running,
                          covs = covariates_munyo,
                          c = 0, 
                          h = 96, 
                          p = 2, 
                          kernel = "triangular")

model_rdd96_4m <- rdrobust(y = crime_model_clean$crime_count, 
                          x = crime_model_clean$running,
                          covs = covariates_munyo,
                          c = 0, 
                          h = 96, 
                          p = 4, 
                          vce = "HC3",
                          kernel = "triangular")

summary(model_rdd96m)
summary(model_rdd72m)
summary(model_rdd120m)
summary(model_rdd96_2m)
summary(model_rdd96_4m)
# THEFT ++++++++++++++++
model_rdd96m_theft <- rdrobust(y = crime_model_clean$theft, 
                         x = crime_model_clean$running,
                         covs = covariates_munyo,
                         c = 0, 
                         h = 96, 
                         p = 1, 
                         vce = "HC3",
                         kernel = "triangular")

model_rdd72m_theft <- rdrobust(y = crime_model_clean$theft, 
                         x = crime_model_clean$running,
                         covs = covariates_munyo,
                         c = 0, 
                         h = 72, 
                         p = 1, 
                         vce = "HC3",
                         kernel = "triangular")

model_rdd120m_theft <- rdrobust(y = crime_model_clean$theft, 
                          x = crime_model_clean$running,
                          covs = covariates_munyo,
                          c = 0, 
                          h = 120, 
                          p = 1, 
                          vce = "HC3",
                          kernel = "triangular")

model_rdd96_2m_theft <- rdrobust(y = crime_model_clean$theft, 
                           x = crime_model_clean$running,
                           covs = covariates_munyo,
                           c = 0, 
                           h = 96, 
                           p = 2, 
                           kernel = "triangular")

model_rdd96_4m_theft <- rdrobust(y = crime_model_clean$theft, 
                           x = crime_model_clean$running,
                           covs = covariates_munyo,
                           c = 0, 
                           h = 96, 
                           p = 4, 
                           vce = "HC3",
                           kernel = "triangular")

summary(model_rdd96m_theft)
summary(model_rdd72m_theft)
summary(model_rdd120m_theft)
summary(model_rdd96_2m_theft)
summary(model_rdd96_4m_theft)

# ROBBERY ++++++++++++++++
model_rdd96m_robbery <- rdrobust(y = crime_model_clean$robbery, 
                               x = crime_model_clean$running,
                               covs = covariates_munyo,
                               c = 0, 
                               h = 96, 
                               p = 1, 
                               vce = "HC3",
                               kernel = "triangular")

model_rdd72m_robbery <- rdrobust(y = crime_model_clean$robbery, 
                               x = crime_model_clean$running,
                               covs = covariates_munyo,
                               c = 0, 
                               h = 72, 
                               p = 1, 
                               vce = "HC3",
                               kernel = "triangular")

model_rdd120m_robbery <- rdrobust(y = crime_model_clean$robbery, 
                                x = crime_model_clean$running,
                                covs = covariates_munyo,
                                c = 0, 
                                h = 120, 
                                p = 1, 
                                vce = "HC3",
                                kernel = "triangular")

model_rdd96_2m_robbery <- rdrobust(y = crime_model_clean$robbery, 
                                 x = crime_model_clean$running,
                                 covs = covariates_munyo,
                                 c = 0, 
                                 h = 96, 
                                 p = 2, 
                                 kernel = "triangular")

model_rdd96_4m_robbery <- rdrobust(y = crime_model_clean$robbery, 
                                 x = crime_model_clean$running,
                                 covs = covariates_munyo,
                                 c = 0, 
                                 h = 96, 
                                 p = 4, 
                                 vce = "HC3",
                                 kernel = "triangular")


summary(model_rdd96m)
summary(model_rdd72m)
summary(model_rdd120m)
summary(model_rdd96_2m)
summary(model_rdd96_4m)
summary(model_rdd96m_theft)
summary(model_rdd72m_theft)
summary(model_rdd120m_theft)
summary(model_rdd96_2m_theft)
summary(model_rdd96_4m_theft)
summary(model_rdd96m_robbery)
summary(model_rdd72m_robbery)
summary(model_rdd120m_robbery)
summary(model_rdd96_2m_robbery)
summary(model_rdd96_4m_robbery)

# Placebo Tests RDD-------
# Get a grid of running values and the years they appear in
running_years <- crime_model_clean %>%
  group_by(running_global, year) %>%
  summarize(n = n(), .groups = "drop") %>%
  group_by(running_global) %>%
  summarize(year_count = n())

# Filter only running values that appear in *all* years
n_years <- length(unique(crime_model_clean$year))

# Get only running values that appear in all years
valid_running_vals <- running_years %>%
  filter(year_count == n_years) %>%
  pull(running_global)

# Find min and max offset that exists across all years
min_valid <- min(valid_running_vals)
max_valid <- max(valid_running_vals)

# Generate placebo cutoffs in weekly (168h) steps
# placebo_offsets <- seq(from = min_valid, to = max_valid, by = 168)
# Keep only those that are exact weekly intervals from DST (i.e., modulo 168 = 0)
valid_placebo_vals <- valid_running_vals[valid_running_vals %% 168 == 0]
# Exclude the real DST cutoff (0)
placebo_offsets <- valid_placebo_vals[valid_placebo_vals != 0]

results_96 <- data.frame(
  offset_hours = numeric(),
  offset_week = numeric(),
  coef = numeric(),
  p_value = numeric(),
  significant_negative = logical()
)

for (offset in placebo_offsets) {
  tryCatch({
    model <- rdrobust(
      y = crime_model_clean$crime_count,
      x = crime_model_clean$running_global,
      covs = covariates,  
      c = offset,
      h = 96,
      p = 1,
      kernel = "triangular"
    )
    
    coef <- model$coef[1]
    p_val <- model$pv[1]
    
    results_96 <- rbind(results_96, data.frame(
      offset_hours = offset,
      offset_week = (offset / 168),
      coef = coef,
      p_value = p_val,
      significant_negative = (coef < 0 & p_val < 0.05)
    ))
    
  }, error = function(e) {
    message("Error at offset ", offset, ": ", e$message)
  })
}

# Look at this insane mess
results_96

# Larger Bandwith (1 Week)
results_168 <- data.frame(
  offset_hours = numeric(),
  offset_week = numeric(),
  coef = numeric(),
  p_value = numeric(),
  significant_negative = logical()
)

for (offset in placebo_offsets) {
  tryCatch({
    model <- rdrobust(
      y = crime_model_clean$crime_count,
      x = crime_model_clean$running_global,
      covs = covariates,  
      c = offset,
      h = 168,
      p = 1,
      kernel = "triangular"
    )
    
    coef <- model$coef[1]
    p_val <- model$pv[1]
    
    results_168 <- rbind(results_168, data.frame(
      offset_hours = offset,
      offset_week = (offset / 168),
      coef = coef,
      p_value = p_val,
      significant_negative = (coef < 0 & p_val < 0.05)
    ))
    
  }, error = function(e) {
    message("Error at offset ", offset, ": ", e$message)
  })
}

# This is not better
results_168
# Fall DST Poisson----------
# 1. Generate first Sunday of November for each year
november_placebo_dates <- tibble(
  year = 2006:2024,
  placebo_date = map(year, ~ {
    first_nov <- ymd(paste0(.x, "-11-01"))
    first_sunday <- first_nov + days((7 - wday(first_nov)) %% 7)
    return(first_sunday)
  }) %>% unlist()
)

# 2. Join with crime_model
crime_placebo <- crime_hourly %>%
  mutate(
    date = as.Date(datetime),
    year = year(date)
  ) %>%
  left_join(dst_dates, by = c("year" = "dst_year")) %>%
  mutate(
    placebo_date = dst_end_date,
    running_day_placebo = as.integer(difftime(date, placebo_date, units = "days")),
    
    # Pre-placebo week
    placebo_pre_monday    = as.integer(running_day_placebo == -6 & weekday == 2),
    placebo_pre_tuesday   = as.integer(running_day_placebo == -5 & weekday == 3),
    placebo_pre_wednesday = as.integer(running_day_placebo == -4 & weekday == 4),
    placebo_pre_thursday  = as.integer(running_day_placebo == -3 & weekday == 5),
    placebo_pre_friday    = as.integer(running_day_placebo == -2 & weekday == 6),
    placebo_pre_saturday  = as.integer(running_day_placebo == -1 & weekday == 7),
    placebo_pre_sunday    = as.integer(running_day_placebo == -7 & weekday == 1),
    
    # Placebo Sunday
    placebo_sunday = as.integer(running_day_placebo == 0 & weekday == 1),
    
    # Post-placebo week
    placebo_monday    = as.integer(running_day_placebo == 1 & weekday == 2),
    placebo_tuesday   = as.integer(running_day_placebo == 2 & weekday == 3),
    placebo_wednesday = as.integer(running_day_placebo == 3 & weekday == 4),
    placebo_thursday  = as.integer(running_day_placebo == 4 & weekday == 5),
    placebo_friday    = as.integer(running_day_placebo == 5 & weekday == 6),
    placebo_saturday  = as.integer(running_day_placebo == 6 & weekday == 7),
    placebo_sunday_after = as.integer(running_day_placebo == 7 & weekday == 1)
  )


# Placebo Models
# OLS: Total crime count (placebo)
model_ols_total4_plc <- lm(crime_count ~ 
                             placebo_sunday +
                             placebo_monday + placebo_tuesday + placebo_wednesday + placebo_thursday + placebo_friday + placebo_saturday + placebo_sunday_after +
                             darkness + 
                             factor(hour) + factor(weekday) + factor(month) + factor(year),
                           data = crime_placebo)
summary(model_ols_total4_plc)

# Poisson: Total crime count (placebo)
model_poisson_total4_plc <- glm(crime_count ~ 
                                  placebo_sunday +
                                  placebo_monday + placebo_tuesday + placebo_wednesday + placebo_thursday + placebo_friday + placebo_saturday + placebo_sunday_after +
                                  darkness + 
                                  factor(hour) + factor(weekday) + factor(month) + factor(year),
                                family = poisson(link = "log"),
                                data = crime_placebo)
robust_vcov_total4_plc <- vcovHC(model_poisson_total4_plc, type = "HC3")
coeftest(model_poisson_total4_plc, vcov = robust_vcov_total4_plc)

# OLS: Theft (placebo)
model_ols_total_theft4_plc <- lm(theft ~
                                   placebo_sunday +
                                   placebo_monday + placebo_tuesday + placebo_wednesday + placebo_thursday + placebo_friday + placebo_saturday + placebo_sunday_after +
                                   darkness + 
                                   factor(hour) + factor(weekday) + factor(month) + factor(year),
                                 data = crime_placebo)
summary(model_ols_total_theft4_plc)

# Poisson: Theft (placebo)
model_poisson_total_theft4_plc <- glm(theft ~ 
                                        placebo_sunday +
                                        placebo_monday + placebo_tuesday + placebo_wednesday + placebo_thursday + placebo_friday + placebo_saturday + placebo_sunday_after +
                                        darkness + 
                                        factor(hour) + factor(weekday) + factor(month) + factor(year),
                                      family = poisson(link = "log"),
                                      data = crime_placebo)
robust_vcov_theft4_plc <- vcovHC(model_poisson_total_theft4_plc, type = "HC3")
coeftest(model_poisson_total_theft4_plc, vcov = robust_vcov_theft4_plc)

# OLS: Robbery (placebo)
model_ols_total_robbery4_plc <- lm(robbery ~
                                     placebo_sunday +
                                     placebo_monday + placebo_tuesday + placebo_wednesday + placebo_thursday + placebo_friday + placebo_saturday + placebo_sunday_after +
                                     darkness + 
                                     factor(hour) + factor(weekday) + factor(month) + factor(year),
                                   data = crime_placebo)
summary(model_ols_total_robbery4_plc)

# Poisson: Robbery (placebo)
model_poisson_total_robbery4_plc <- glm(robbery ~ 
                                          placebo_sunday +
                                          placebo_monday + placebo_tuesday + placebo_wednesday + placebo_thursday + placebo_friday + placebo_saturday + placebo_sunday_after +
                                          darkness + 
                                          factor(hour) + factor(weekday) + factor(month) + factor(year),
                                        family = poisson(link = "log"),
                                        data = crime_placebo)
robust_vcov_robbery4_plc <- vcovHC(model_poisson_total_robbery4_plc, type = "HC3")
coeftest(model_poisson_total_robbery4_plc, vcov = robust_vcov_robbery4_plc)

summary(model_ols_total4_plc)
coeftest(model_poisson_total4_plc, vcov = robust_vcov_total4_plc)
summary(model_ols_total_theft4_plc)
coeftest(model_poisson_total_theft4_plc, vcov = robust_vcov_theft4_plc)
summary(model_ols_total_robbery4_plc)
coeftest(model_poisson_total_robbery4_plc, vcov = robust_vcov_robbery4_plc)

# Placebo Poisson -----------
crime_model <- crime_model %>%
  mutate(
    # Placebo week dummies: 8 to 14 days after DST
    placebo_dst_sunday_pre = as.integer(running_day == 7 & weekday == 1),
    placebo_monday_pre    = as.integer(running_day == 8 & weekday == 2),
    placebo_tuesday_pre   = as.integer(running_day == 9 & weekday == 3),
    placebo_wednesday_pre = as.integer(running_day == 10 & weekday == 4),
    placebo_thursday_pre  = as.integer(running_day == 11 & weekday == 5),
    placebo_friday_pre    = as.integer(running_day == 12 & weekday == 6),
    placebo_saturday_pre  = as.integer(running_day == 13 & weekday == 7),
    placebo_sunday_dst    = as.integer(running_day == 14 & weekday == 1),
    placebo_monday_post    = as.integer(running_day == 15 & weekday == 2),
    placebo_tuesday_post   = as.integer(running_day == 16 & weekday == 3),
    placebo_wednesday_post = as.integer(running_day == 17 & weekday == 4),
    placebo_thursday_post  = as.integer(running_day == 18 & weekday == 5),
    placebo_friday_post    = as.integer(running_day == 19 & weekday == 6),
    placebo_saturday_post  = as.integer(running_day == 20 & weekday == 7),
    placebo_sunday_post    = as.integer(running_day == 21 & weekday == 1)
  )

crime_placebo <- crime_model

# Placebo Models
# OLS: Total crime count (placebo)
model_ols_total4_plc <- lm(crime_count ~ 
                             placebo_sunday +
                             placebo_monday + placebo_tuesday + placebo_wednesday + placebo_thursday + placebo_friday + placebo_saturday + placebo_sunday_after +
                             darkness + 
                             factor(hour) + factor(weekday) + factor(month) + factor(year),
                           data = crime_placebo)
summary(model_ols_total4_plc)

# Poisson: Total crime count (placebo)
model_poisson_total4_plc <- glm(crime_count ~ 
                                  placebo_sunday +
                                  placebo_monday + placebo_tuesday + placebo_wednesday + placebo_thursday + placebo_friday + placebo_saturday + placebo_sunday_after +
                                  darkness + 
                                  factor(hour) + factor(weekday) + factor(month) + factor(year),
                                family = poisson(link = "log"),
                                data = crime_placebo)
robust_vcov_total4_plc <- vcovHC(model_poisson_total4_plc, type = "HC3")
coeftest(model_poisson_total4_plc, vcov = robust_vcov_total4_plc)

# OLS: Theft (placebo)
model_ols_total_theft4_plc <- lm(theft ~
                                   placebo_sunday +
                                   placebo_monday + placebo_tuesday + placebo_wednesday + placebo_thursday + placebo_friday + placebo_saturday + placebo_sunday_after +
                                   darkness + 
                                   factor(hour) + factor(weekday) + factor(month) + factor(year),
                                 data = crime_placebo)
summary(model_ols_total_theft4_plc)

# Poisson: Theft (placebo)
model_poisson_total_theft4_plc <- glm(theft ~ 
                                        placebo_sunday +
                                        placebo_monday + placebo_tuesday + placebo_wednesday + placebo_thursday + placebo_friday + placebo_saturday + placebo_sunday_after +
                                        darkness + 
                                        factor(hour) + factor(weekday) + factor(month) + factor(year),
                                      family = poisson(link = "log"),
                                      data = crime_placebo)
robust_vcov_theft4_plc <- vcovHC(model_poisson_total_theft4_plc, type = "HC3")
coeftest(model_poisson_total_theft4_plc, vcov = robust_vcov_theft4_plc)

# OLS: Robbery (placebo)
model_ols_total_robbery4_plc <- lm(robbery ~
                                     placebo_sunday +
                                     placebo_monday + placebo_tuesday + placebo_wednesday + placebo_thursday + placebo_friday + placebo_saturday + placebo_sunday_after +
                                     darkness + 
                                     factor(hour) + factor(weekday) + factor(month) + factor(year),
                                   data = crime_placebo)
summary(model_ols_total_robbery4_plc)

# Poisson: Robbery (placebo)
model_poisson_total_robbery4_plc <- glm(robbery ~ 
                                          placebo_sunday +
                                          placebo_monday + placebo_tuesday + placebo_wednesday + placebo_thursday + placebo_friday + placebo_saturday + placebo_sunday_after +
                                          darkness + 
                                          factor(hour) + factor(weekday) + factor(month) + factor(year),
                                        family = poisson(link = "log"),
                                        data = crime_placebo)
robust_vcov_robbery4_plc <- vcovHC(model_poisson_total_robbery4_plc, type = "HC3")
coeftest(model_poisson_total_robbery4_plc, vcov = robust_vcov_robbery4_plc)

summary(model_ols_total4_plc)
coeftest(model_poisson_total4_plc, vcov = robust_vcov_total4_plc)
summary(model_ols_total_theft4_plc)
coeftest(model_poisson_total_theft4_plc, vcov = robust_vcov_theft4_plc)
summary(model_ols_total_robbery4_plc)
coeftest(model_poisson_total_robbery4_plc, vcov = robust_vcov_robbery4_plc)

p_load(modelsummary)

