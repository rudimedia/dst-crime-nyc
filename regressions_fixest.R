# Table 3 --------------
p_load(lmtest, sandwich)
p_load(fixest)

model_ols_total3 <- lm(crime_count ~ 
                         pre_sunday + pre_monday + pre_tuesday + pre_wednesday + pre_thursday + pre_friday + pre_saturday + 
                         dst_sunday +
                         post_monday + post_tuesday + post_wednesday + post_thursday + post_friday + post_saturday + post_sunday
                        + darkness + factor(hour) + factor(weekday) + factor(month) + factor(year),
                       data = crime_model)
print_main_effects(model_ols_total3)

model_poisson_total3 <- fepois(crime_count ~ 
                              pre_sunday + pre_monday + pre_tuesday + pre_wednesday + pre_thursday + pre_friday + pre_saturday +
                              dst_sunday +
                              post_monday + post_tuesday + post_wednesday + post_thursday + post_friday + post_saturday + post_sunday
                              + darkness | hour + weekday + month + year,
                            data = crime_model)
summary(model_poisson_total3, vcov = "hetero")

model_ols_total_theft3 <- lm(theft ~
                               pre_sunday + pre_monday + pre_tuesday + pre_wednesday + pre_thursday + pre_friday + pre_saturday +
                               dst_sunday +
                               post_monday + post_tuesday + post_wednesday + post_thursday + post_friday + post_saturday + post_sunday
                             + darkness + factor(hour) + factor(weekday) + factor(month) + factor(year),
                             data = crime_model)
print_main_effects(model_ols_total_theft3)

model_poisson_total_theft3 <- fepois(theft ~ 
                                    pre_sunday + pre_monday + pre_tuesday + pre_wednesday + pre_thursday + pre_friday + pre_saturday +
                                    dst_sunday +
                                    post_monday + post_tuesday + post_wednesday + post_thursday + post_friday + post_saturday + post_sunday
                                    + darkness | hour + weekday + month + year,
                                  data = crime_model)
summary(model_poisson_total_theft3, vcov = "hetero")

model_ols_total_robbery3 <- lm(robbery ~
                                 pre_sunday + pre_monday + pre_tuesday + pre_wednesday + pre_thursday + pre_friday + pre_saturday +
                                 dst_sunday +
                                 post_monday + post_tuesday + post_wednesday + post_thursday + post_friday + post_saturday + post_sunday
                               + darkness + factor(hour) + factor(weekday) + factor(month) + factor(year),
                               data = crime_model)
print_main_effects(model_ols_total_robbery3)

model_poisson_total_robbery3 <- fepois(robbery ~ 
                                      pre_sunday + pre_monday + pre_tuesday + pre_wednesday + pre_thursday + pre_friday + pre_saturday +
                                      dst_sunday +
                                      post_monday + post_tuesday + post_wednesday + post_thursday + post_friday + post_saturday + post_sunday
                                      + darkness | hour + weekday + month + year,
                                    data = crime_model)
summary(model_poisson_total_robbery3, vcov = "hetero")
# Table 3.1 ------
# Function to calculate Ash Wednesday for a given year
p_load(timeDate)
ashWednesday <- function(year) {
  as.Date(Easter(year)) - 46
}

# Add a logical 'holiday' column that's TRUE if date is Ash Wednesday
crime_model <- crime_model %>%
  mutate(
    ash_wed = ashWednesday(year),
    ash = date == ash_wed
)

model_ols_total4 <- lm(crime_count ~ 
                         pre_sunday + pre_monday + pre_tuesday + pre_wednesday + pre_thursday + pre_friday + pre_saturday + dst_sunday +
                         post_monday + post_tuesday + post_wednesday + post_thursday + post_friday + post_saturday + post_sunday +
                         darkness + ash +
                         factor(hour_weekday) + factor(month_weekday) + factor(year),
                       data = crime_model)
print_main_effects(model_ols_total4)

model_poisson_total4 <- fepois(crime_count ~ 
                                 pre_sunday + pre_monday + pre_tuesday + pre_wednesday + pre_thursday + pre_friday + pre_saturday + dst_sunday +
                                 post_monday + post_tuesday + post_wednesday + post_thursday + post_friday + post_saturday + post_sunday +
                                 darkness + ash| 
                                 hour_weekday + month_weekday + year,
                               data = crime_model)
summary(model_poisson_total4, vcov = "hetero")

model_ols_total_theft4 <- lm(theft ~
                               pre_sunday + pre_monday + pre_tuesday + pre_wednesday + pre_thursday + pre_friday + pre_saturday + dst_sunday +
                               post_monday + post_tuesday + post_wednesday + post_thursday + post_friday + post_saturday + post_sunday +
                               darkness + ash +
                               factor(hour_weekday) + factor(month_weekday) + factor(year),
                             data = crime_model)
print_main_effects(model_ols_total_theft4)

model_poisson_total_theft4 <- fepois(theft ~ 
                                       pre_sunday + pre_monday + pre_tuesday + pre_wednesday + pre_thursday + pre_friday + pre_saturday + dst_sunday +
                                       post_monday + post_tuesday + post_wednesday + post_thursday + post_friday + post_saturday + post_sunday +
                                       darkness + ash| 
                                       hour_weekday + month_weekday + year,,
                                     data = crime_model)
summary(model_poisson_total_theft4, vcov = "hetero")

model_ols_total_robbery4 <- lm(robbery ~
                                 pre_sunday + pre_monday + pre_tuesday + pre_wednesday + pre_thursday + pre_friday + pre_saturday + dst_sunday +
                                 post_monday + post_tuesday + post_wednesday + post_thursday + post_friday + post_saturday + post_sunday +
                                 darkness + ash +
                                 factor(hour_weekday) + factor(month_weekday) + factor(year),
                               data = crime_model)
print_main_effects(model_ols_total_robbery4)

model_poisson_total_robbery4 <- fepois(robbery ~ 
                                         pre_sunday + pre_monday + pre_tuesday + pre_wednesday + pre_thursday + pre_friday + pre_saturday + dst_sunday +
                                         post_monday + post_tuesday + post_wednesday + post_thursday + post_friday + post_saturday + post_sunday +
                                         darkness + ash| 
                                         hour_weekday + month_weekday + year,,
                                       data = crime_model)
summary(model_poisson_total_robbery4, vcov = "hetero")
# Table 4 ----------
print_main_effects <- function(model) {
  coefs <- summary(model)$coefficients
  keep <- !grepl("^factor\\(", rownames(coefs))
  coefs_filtered <- coefs[keep, , drop = FALSE]
  
  # Create stars based on p-values
  stars <- symnum(coefs_filtered[, "Pr(>|t|)"],
                  corr = FALSE, na = FALSE,
                  cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                  symbols = c("***", "**", "*", " "))
  
  # Add stars as a new column
  coefs_with_stars <- cbind(coefs_filtered, Signif = stars)
  
  print(coefs_with_stars)
}

crime_model$hour_weekday <- interaction(crime_model$hour, crime_model$weekday, drop = TRUE)
crime_model$month_weekday <- interaction(crime_model$month, crime_model$weekday, drop = TRUE)


model_ols_total4 <- lm(crime_count ~ 
                         dst_sunday +
                         post_monday + post_tuesday + post_wednesday + post_thursday + post_friday + post_saturday + post_sunday +
                         darkness +
                         factor(hour_weekday) + factor(month_weekday) + factor(year),
                       data = crime_model)
print_main_effects(model_ols_total4)

model_poisson_total4 <- fepois(crime_count ~ 
                              dst_sunday +
                              post_monday + post_tuesday + post_wednesday + post_thursday + post_friday + post_saturday + post_sunday +
                              darkness | 
                                hour_weekday + month_weekday + year,,
                            data = crime_model)
summary(model_poisson_total4, vcov = "hetero")

model_ols_total_theft4 <- lm(theft ~
                               dst_sunday +
                               post_monday + post_tuesday + post_wednesday + post_thursday + post_friday + post_saturday + post_sunday +
                               darkness + 
                               factor(hour_weekday) + factor(month_weekday) + factor(year),
                             data = crime_model)
print_main_effects(model_ols_total_theft4)

model_poisson_total_theft4 <- fepois(theft ~ 
                                    dst_sunday +
                                    post_monday + post_tuesday + post_wednesday + post_thursday + post_friday + post_saturday + post_sunday +
                                    darkness | 
                                      hour_weekday + month_weekday + year,,
                                  data = crime_model)
summary(model_poisson_total_theft4, vcov = "hetero")

model_ols_total_robbery4 <- lm(robbery ~
                                 dst_sunday +
                                 post_monday + post_tuesday + post_wednesday + post_thursday + post_friday + post_saturday + post_sunday +
                                 darkness + 
                                 factor(hour_weekday) + factor(month_weekday) + factor(year),
                               data = crime_model)
print_main_effects(model_ols_total_robbery4)

model_poisson_total_robbery4 <- fepois(robbery ~ 
                                      dst_sunday +
                                      post_monday + post_tuesday + post_wednesday + post_thursday + post_friday + post_saturday + post_sunday +
                                      darkness | 
                                        hour_weekday + month_weekday + year,,
                                    data = crime_model)
summary(model_poisson_total_robbery4, vcov = "hetero")
# Placebo --------------
crime_placebo <- crime_model

crime_placebo$hour_weekday <- interaction(crime_placebo$hour, crime_placebo$weekday, drop = TRUE)
crime_placebo$month_weekday <- interaction(crime_placebo$month, crime_placebo$weekday, drop = TRUE)

# Fall DST -------------
model_ols_total4_fall <- lm(crime_count ~ 
                              placebo_dst_sunday_pre +
                              placebo_monday_pre + placebo_tuesday_pre + placebo_wednesday_pre + placebo_thursday_pre + placebo_friday_pre + placebo_saturday_pre + 
                              placebo_sunday_dst + 
                              placebo_monday_post + placebo_tuesday_post + placebo_wednesday_post + placebo_thursday_post + placebo_friday_post + placebo_saturday_post + placebo_sunday_post +
                              darkness + 
                              factor(hour_weekday) + factor(month_weekday) + factor(year),
                            data = crime_placebo)
print_main_effects(model_ols_total4_fall)

# Poisson: Total crime count (placebo)
model_poisson_total4_fall <- fepois(crime_count ~ 
                                      placebo_dst_sunday_pre +
                                      placebo_monday_pre + placebo_tuesday_pre + placebo_wednesday_pre + placebo_thursday_pre + placebo_friday_pre + placebo_saturday_pre + 
                                      placebo_sunday_dst + 
                                      placebo_monday_post + placebo_tuesday_post + placebo_wednesday_post + placebo_thursday_post + placebo_friday_post + placebo_saturday_post + placebo_sunday_post +
                                      darkness | 
                                      hour_weekday + month_weekday + year,
                                    data = crime_placebo)
summary(model_poisson_total4_fall, vcov = "hetero")

# OLS: Theft (placebo)
model_ols_total_theft4_fall <- lm(theft ~
                                    placebo_dst_sunday_pre +
                                    placebo_monday_pre + placebo_tuesday_pre + placebo_wednesday_pre + placebo_thursday_pre + placebo_friday_pre + placebo_saturday_pre + 
                                    placebo_sunday_dst + 
                                    placebo_monday_post + placebo_tuesday_post + placebo_wednesday_post + placebo_thursday_post + placebo_friday_post + placebo_saturday_post + placebo_sunday_post +
                                    darkness + 
                                    factor(hour_weekday) + factor(month_weekday) + factor(year),
                                  data = crime_placebo)
print_main_effects(model_ols_total_theft4_fall)

# Poisson: Theft (placebo)
model_poisson_total_theft4_fall <- fepois(theft ~ 
                                            placebo_dst_sunday_pre +
                                            placebo_monday_pre + placebo_tuesday_pre + placebo_wednesday_pre + placebo_thursday_pre + placebo_friday_pre + placebo_saturday_pre + 
                                            placebo_sunday_dst + 
                                            placebo_monday_post + placebo_tuesday_post + placebo_wednesday_post + placebo_thursday_post + placebo_friday_post + placebo_saturday_post + placebo_sunday_post +
                                            darkness | 
                                            hour_weekday + month_weekday + year,
                                          data = crime_placebo)
summary(model_poisson_total_theft4_fall, vcov = "hetero")

# OLS: Robbery (placebo)
model_ols_total_robbery4_fall <- lm(robbery ~
                                      placebo_dst_sunday_pre +
                                      placebo_monday_pre + placebo_tuesday_pre + placebo_wednesday_pre + placebo_thursday_pre + placebo_friday_pre + placebo_saturday_pre + 
                                      placebo_sunday_dst + 
                                      placebo_monday_post + placebo_tuesday_post + placebo_wednesday_post + placebo_thursday_post + placebo_friday_post + placebo_saturday_post + placebo_sunday_post +
                                      darkness + 
                                      factor(hour_weekday) + factor(month_weekday) + factor(year),
                                    data = crime_placebo)
print_main_effects(model_ols_total_robbery4_fall)

# Poisson: Robbery (placebo)
model_poisson_total_robbery4_fall <- fepois(robbery ~ 
                                              placebo_dst_sunday_pre +
                                              placebo_monday_pre + placebo_tuesday_pre + placebo_wednesday_pre + placebo_thursday_pre + placebo_friday_pre + placebo_saturday_pre + 
                                              placebo_sunday_dst + 
                                              placebo_monday_post + placebo_tuesday_post + placebo_wednesday_post + placebo_thursday_post + placebo_friday_post + placebo_saturday_post + placebo_sunday_post +
                                              darkness | 
                                              hour_weekday + month_weekday + year,
                                            data = crime_placebo)
summary(model_poisson_total_robbery4_fall, vcov = "hetero")

# Fall DST -------------
model_ols_total4_fall <- lm(crime_count ~ 
                             placebo_sunday +
                             placebo_monday + placebo_tuesday + placebo_wednesday + placebo_thursday + placebo_friday + placebo_saturday + placebo_sunday_after +
                             darkness + 
                             factor(hour_weekday) + factor(month_weekday) + factor(year),
                           data = crime_placebo)
print_main_effects(model_ols_total4_fall)

# Poisson: Total crime count (placebo)
model_poisson_total4_fall <- fepois(crime_count ~ 
                                  placebo_sunday +
                                  placebo_monday + placebo_tuesday + placebo_wednesday + placebo_thursday + placebo_friday + placebo_saturday + placebo_sunday_after +
                                  darkness | 
                                  hour_weekday + month_weekday + year,
                                data = crime_placebo)
summary(model_poisson_total4_fall, vcov = "hetero")

# OLS: Theft (placebo)
model_ols_total_theft4_fall <- lm(theft ~
                                   placebo_sunday +
                                   placebo_monday + placebo_tuesday + placebo_wednesday + placebo_thursday + placebo_friday + placebo_saturday + placebo_sunday_after +
                                   darkness + 
                                    factor(hour_weekday) + factor(month_weekday) + factor(year),
                                 data = crime_placebo)
print_main_effects(model_ols_total_theft4_fall)

# Poisson: Theft (placebo)
model_poisson_total_theft4_fall <- fepois(theft ~ 
                                        placebo_sunday +
                                        placebo_monday + placebo_tuesday + placebo_wednesday + placebo_thursday + placebo_friday + placebo_saturday + placebo_sunday_after +
                                        darkness | 
                                          hour_weekday + month_weekday + year,
                                      data = crime_placebo)
summary(model_poisson_total_theft4_fall, vcov = "hetero")

# OLS: Robbery (placebo)
model_ols_total_robbery4_fall <- lm(robbery ~
                                     placebo_sunday +
                                     placebo_monday + placebo_tuesday + placebo_wednesday + placebo_thursday + placebo_friday + placebo_saturday + placebo_sunday_after +
                                     darkness + 
                                      factor(hour_weekday) + factor(month_weekday) + factor(year),
                                   data = crime_placebo)
print_main_effects(model_ols_total_robbery4_fall)

# Poisson: Robbery (placebo)
model_poisson_total_robbery4_fall <- fepois(robbery ~ 
                                          placebo_sunday +
                                          placebo_monday + placebo_tuesday + placebo_wednesday + placebo_thursday + placebo_friday + placebo_saturday + placebo_sunday_after +
                                          darkness | 
                                            hour_weekday + month_weekday + year,
                                        data = crime_placebo)
summary(model_poisson_total_robbery4_fall, vcov = "hetero")

print_main_effects(model_ols_total4_fall)
summary(model_poisson_total4_fall, vcov = "hetero")
print_main_effects(model_ols_total_theft4_fall)
summary(model_poisson_total_theft4_fall, vcov = "hetero")
print_main_effects(model_ols_total_robbery4_fall)
summary(model_poisson_total_robbery4_fall, vcov = "hetero")
