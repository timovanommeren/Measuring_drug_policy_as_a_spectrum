source(here::here("analysis", "R", "load_and_clean.R"))



#refactor
df <- df |>
  mutate(urb = haven::as_factor(urb),
         Corop_regio_rec = haven::as_factor(Corop_regio_rec),
         age_2 = fct_collapse(age_4_rec, "18-49" = c("18-34", "35-49"), "50+" = c("50-64", "65+"))) |>
  mutate(across(where(haven::is.labelled), haven::zap_labels))



df$legal_drugs <- (df$beleid_voorkeur_alcohol + df$beleid_voorkeur_tabak_en_vapes + df$beleid_voorkeur_cannabis) / 3
df$illicit_drugs <- (df$beleid_voorkeur_mdma +
                    df$beleid_voorkeur_psychedelica +
                    df$beleid_voorkeur_heroine +
                    df$beleid_voorkeur_speed +
                    df$beleid_voorkeur_cocaine) / 5



## model
cohort_illegal <- lm(illicit_drugs ~ age_2, df)
cohort_legal <- lm(legal_drugs ~ age_2, df)
cohort_urb <- lm(legal_drugs ~ urb, df)

summary(cohort_illegal)
summary(cohort_legal)
summary(cohort_urb)

## plot
plot(cohort_legal$coefficients, col = "blue")
points(cohort_illegal$coefficients, col = "red")


# predicted mean per age group
ages <- data.frame(age_2 = levels(df$age_2))
plot(predict(cohort_legal, ages), col = "blue", ylim = c(2, 5.5), xaxt = "n",
     xlab = "age", ylab = "predicted policy preference")
axis(1, at = seq_len(nrow(ages)), labels = ages$age_2)
points(predict(cohort_illegal, ages), col = "red")


lm(beleid_voorkeur_alcohol ~ beliefs_society_5 + beliefs_society_4 + beliefs_regulate_1, df)
