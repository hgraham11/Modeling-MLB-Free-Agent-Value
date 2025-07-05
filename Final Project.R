# MARKETING ANALYTICS FINAL PROJECT:

library(stringr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(knitr)
install.packages("cluster")
library(cluster)
install.packages("googlesheets4")
library(googlesheets4)
install.packages("broom")
library(broom)
install.packages("scales")
library(scales)
install.packages("car")
library(car)
install.packages("stringi")
library(stringi)
install.packages("randomForest")
library(randomForest)
install.packages("corrplot")
library(corrplot)
install.packages("xgboost")
library(xgboost)
install.packages("caret")
install.packages("lattice")
library(lattice)
library(caret)


# HITTERS_RAW_2022_2025 DF:
hitters_raw_2022_2025 <- read.csv("fangraphs-leaderboards.csv")


# HITTERS_RAW_2022_2025 DF: CLEANING NAMES:
hitters_raw_2022_2025$Name_clean <- str_trim(str_to_lower(hitters_raw_2022_2025$Name))
hitters_raw_2022_2025 <- hitters_raw_2022_2025 %>%
  select(Name_clean, everything())


# HITTERS_RAW_2022_2025 DF: CLEANING DF:
hitters_raw_2022_2025 <- hitters_raw_2022_2025 %>%
  mutate(Name_clean = str_trim(str_to_lower(Name)))


# HITTERS_CLEAN_2022_2025 DF: CREATION:
hitters_clean_2022_2025 <- hitters_raw_2022_2025 %>%
  select(Name_clean, Team, PA, HR, R, RBI, SB, `BB.`, `K.`, ISO, AVG, OBP, SLG,
         wOBA, `wRC.`, BsR, Off, Def, WAR)


# FA_SIGNINGS_2022_2025 DF:
fa_signings_2022_2025 <- read.csv("fa_signings_2022_2025.csv")


# FA_SIGNINGS_2022_2025 DF: CLEANING NAMES:
fa_signings_2022_2025$Name_clean <- str_trim(str_to_lower(fa_signings_2022_2025$Name))
fa_signings_2022_2025 <- fa_signings_2022_2025 %>%
  select(Name_clean, everything())
fa_signings_2022_2025 <- fa_signings_2022_2025 %>%
  select (-Name)


# JOIN HITTERS_CLEAN AND FA_SIGNINGS:
hitters_fa_join <- fa_signings_2022_2025 %>%
  left_join(hitters_clean_2022_2025, by = "Name_clean")


# HITTERS_FA_JOIN:FILTER OUT PITCHERS:
hitters_fa_join <- hitters_fa_join %>%
  filter(!Pos %in% c("SP", "RP", "P"))


# HITTERS_FA_JOIN: REMOVE NA'S:
hitters_fa_join <- na.omit((hitters_fa_join))


# HITTERS_FA_JOIN: CLUSTER ANALYSIS:
clustering_input <- hitters_fa_join %>%
  select(WAR, OBP, SLG, HR, `BB.`, `K.`, SB, Age, AAV) %>%
  scale()


# HITTERS_FA_JOIN: CLUSTER ANALYSIS : K-MEANS:
set.seed(123)
wss <- vector()
for (k in 1:10){
  kmeans_result <- kmeans (clustering_input, centers = k, nstart = 25)
  wss[k] <- kmeans_result$tot.withinss}


# HITTERS_FA_JOIN: CLUSTER ANALYSIS: ELBOW PLOT:
elbow_df <- data.frame(Clusters = 1:10, WSS = wss)
ggplot(elbow_df, aes(x = Clusters, y = WSS)) +
  geom_point(size = 3) +
  geom_line() +
  labs(title = "Elbow Plot for K-Means Clustering",
       x = "Number of Clusters (K)",
       y = "Total Within-Cluster Sum of Squares")


# HITTERS_FA_JOIN: CLUSTER ANALYSIS: RE-RUN K-MEANS:
set.seed(123)
kmeans_final <- kmeans(clustering_input, centers = 5, nstart = 25)


# HITTERS_FA_JOIN: ADD COLUMN:
hitters_fa_join$Cluster <- as.factor(kmeans_final$cluster)


# HITTERS_FA_JOIN: PROFILE EACH CLUSTER:
cluster_profiles <- hitters_fa_join %>%
  group_by(Cluster) %>%
  summarise(
    Count = n(),
    WAR = mean(WAR, na.rm = TRUE),
    HR = mean(HR, na.rm = TRUE),
    OBP = mean(OBP, na.rm = TRUE),
    SLG = mean(SLG, na.rm = TRUE),
    `BB.` = mean(`BB.`, na.rm = TRUE),
    `K.` = mean(`K.`, na.rm = TRUE),
    SB = mean(SB, na.rm = TRUE),
    Age = mean(Age, na.rm = TRUE),
    AAV = mean(AAV, na.rm = TRUE),
    .groups = "drop")


# HITTERS_FA_JOIN: RUN PCA:
pca_result <- prcomp(clustering_input)
hitters_fa_join$PC1 <- pca_result$x[, 1]
hitters_fa_join$PC2 <- pca_result$x[, 2]


# HITTERS_FA_JOIN: FLIP PC1:
hitters_fa_join$PC1 <- -hitters_fa_join$PC1


# HITTERS_FA_JOIN: RENAME AXIS FOR PLOTTING:
colnames(hitters_fa_join)[colnames(hitters_fa_join) == "PC1"] <- "Offensive Production"
colnames(hitters_fa_join)[colnames(hitters_fa_join) == "PC2"] <- "Speed, Defense, Athleticism"


# HITTERS_FA_JOIN: REMOVE DUPLICATES:
hitters_fa_join <- hitters_fa_join %>%
  select(-`Offensive Production`, -`Speed, Defense, Athleticism`)


# HITTERS_FA_JOIN: RECREATE / RENAME PROPERLY:
hitters_fa_join$PC1 <- -pca_result$x[, 1]
hitters_fa_join$PC2 <- pca_result$x[, 2]



# HITTERS_FA_JOIN: PLOT: CLUSTER GROUPS:
ggplot(hitters_fa_join, aes(x = `Offensive Production`, y = `Speed, Defense, Athleticism`, color = as.factor(Cluster))) +
  geom_point(alpha = 0.7) +
  scale_color_manual(
    name = "Player Segment",
    values = c(
      "1" = "#F8766D",
      "2" = "#C49A00",
      "3" = "#53B400",
      "4" = "#00C1A7",
      "5" = "#A58AFF"
    ),
    labels = c(
      "1: Good Hitting, Bad Speed, Def, Ath",
      "2: Average Starters",
      "3: Speed & Defense Specialists",
      "4: Replacement Level",
      "5: Very Good Hitters"
    )
  ) +
  labs(
    title = "Hitter Segmentation by Offensive and Athletic Profile",
    x = "Offensive Production",
    y = "Speed & Plate Disclipline"
  ) +
  theme_minimal()


# HITTERS_FA_JOIN: CREATING PCA COLUMNS:
pca_result <- prcomp(clustering_input)
hitters_fa_join$PC1 <- pca_result$x[, 1]
hitters_fa_join$PC2 <- pca_result$x[, 2]
colnames(hitters_fa_join)


# HITTERS_FA_JOIN: HOW MUCH EACH VARIABLE IMPACTS THE X AND Y AXIS:
pca_result <- prcomp(clustering_input)
# View loadings (aka rotation matrix)
pca_result$rotation[, 1:2]  # only show PC1 and PC2


# HITTERS_FA_JOIN: RE-NAMING THE CLUSTERS:
hitters_fa_join <- hitters_fa_join %>%
  mutate(Cluster_Name = case_when(
    Cluster == 1 ~ "Very Good Hitters",
    Cluster == 2 ~ "Good Hitting, Bad Speed, Def, Ath",
    Cluster == 3 ~ "Speed & Defense Specialists",
    Cluster == 4 ~ "Average Starters",
    Cluster == 5 ~ "Replacement Level"))


# HITTERS_FA_JOIN: STANDARDIZING ALL PREDICTING VARIABLES:
hitters_standardized <- hitters_fa_join %>%
  mutate(across(c(WAR, SLG, HR, Age, BsR, K.), scale))
standardized_vars <- scale(hitters_fa_join[, c("WAR", "SLG", "HR", "Age", "BsR", "K.")])
standardized_df <- as.data.frame(standardized_vars)
standardized_df$Cluster_Name <- hitters_fa_join$Cluster_Name


# HITTERS_FA_JOIN: MODELS
model_clean_aav <- lm(AAV ~ WAR + SLG + HR + Age + BsR + K. + Cluster_Name, data = hitters_standardized)
model_clean_years <- lm(Years ~ WAR + SLG + HR + Age + BsR + K. + Cluster_Name, data = hitters_standardized)
model_clean_aav_19 <- lm(AAV ~ Walk.Year.WAR + WAR + PA + AVG + OBP + SLG + HR + RBI + 
                           R + BB. + K. + SB + ISO + wOBA + Off + Def + BsR + Cluster, data = hitters_standardized)

# HITTERS_FA_JOIN: NEW COLUMNS:
hitters_fa_join$Predicted_AAV_M <- predict(model_clean_aav, newdata = hitters_standardized)
hitters_fa_join$Predicted_Years <- predict(model_clean_years, newdata = hitters_standardized)


# HITTERS_FA_JOIN: ROUND PREDICTED YEARS:
hitters_fa_join$Predicted_Years <- round(predict(model_clean_years, newdata = hitters_standardized))


# HITTERS_FA_JOIN: CONVERT COLUMNS TO MILLIONS:
hitters_fa_join$Predicted_AAV_M <- round(hitters_fa_join$Predicted_AAV / 1e6, 2)
hitters_fa_join$Total.Salary <- round(hitters_fa_join$Total.Salary / 1e6, 2)
names(hitters_fa_join) <- make.names(names(hitters_fa_join), unique = TRUE)
hitters_fa_join <- hitters_fa_join %>%
  select(-matches("\\.\\d+$"))
hitters_fa_join <- hitters_fa_join %>%
  select(-Predicted_Total_Salarly)






# HITTERS_FA_JOIN: TOTAL.SALARY COLUMN:
hitters_fa_join <- hitters_fa_join %>%
  mutate(Predicted_Total_Salarly = Predicted_AAV_M * Predicted_Years)
hitters_fa_join <- hitters_fa_join %>%
  mutate(Predicted_Total_Salary = Predicted_AAV_M * Predicted_Years)
hitters_fa_join$Predicted_Total_Salary <- round(hitters_fa_join$Predicted_Total_Salary, 2)



# HITTERS_FA_JOIN: SUMMARY OF MODELS:
summary(model_clean_aav)
summary(model_clean_years)
summary(model_clean_aav_19)

# HITTERS_FA_JOIN: WHAT VARIABLES AFFECT AAV: (AVG STARTERS IS BASELINE GROUP)
tidy(model_clean_aav) %>%
  filter(term != "(Intercept)") %>%
  mutate(term = case_when(
    term == "Cluster_NameVery Good Hitters" ~ "Very Good Hitters",
    term == "Cluster_NameReplacement Level" ~ "Replacement Level",
    term == "Cluster_NameGood Hitting, Bad Speed, Def, Ath" ~ "Power Only",
    term == "Cluster_NameSpeed & Defense Specialists" ~ "Speed/Defense",
    TRUE ~ term
  )) %>%
  ggplot(aes(x = reorder(term, estimate), y = estimate / 1e6, fill = estimate > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Key Player Traits Driving Free Agent AAV",
    x = "Predictor",
    y = "Effect on AAV ($ Millions)"
  ) +
  scale_fill_manual(values = c("red", "green")) +
  scale_y_continuous(labels = scales::label_dollar(scale = 1, suffix = "M")) +
  theme_minimal()


# HITTERS_FA_JOIN: WHAT VARIABLES AFFECT YEARS: (AVG STARTERS IS BASELINE GROUP)
tidy(model_clean_years) %>%
  filter(term != "(Intercept)") %>%
  mutate(term = case_when(
    term == "Cluster_NameVery Good Hitters" ~ "Very Good Hitters",
    term == "Cluster_NameReplacement Level" ~ "Replacement Level",
    term == "Cluster_NameGood Hitting, Bad Speed, Def, Ath" ~ "Power Only",
    term == "Cluster_NameSpeed & Defense Specialists" ~ "Speed/Defense",
    TRUE ~ term
  )) %>%
  ggplot(aes(x = reorder(term, estimate), y = estimate, fill = estimate > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Key Player Traits Driving Contract Length",
    x = "Predictor",
    y = "Effect on Years"
  ) +
  scale_fill_manual(values = c("red", "green")) +
  theme_minimal()


# HITTERS_FA_JOIN: BOX PLOT: AAV DIFFERENCES BY CLUSTER:
ggplot(hitters_fa_join, aes(x = Cluster_Name, y = Predicted_AAV_M, fill = Cluster_Name)) +
  geom_boxplot() +
  labs(title = "Average Annual Value (AAV) by Player Segment",
       x = "Player Segment", y = "AAV ($ Millions)") +
  theme_minimal() +
  scale_y_continuous(labels = scales::label_dollar(suffix = "M")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# HITTERS_FA_JOIN: BOX PLOT: YEARS DIFFERENCES BY CLUSTER:
ggplot(hitters_fa_join, aes(x = Cluster_Name, y = Predicted_Years, fill = Cluster_Name)) +
  geom_boxplot() +
  labs(
    title = "Contract Length (Years) by Player Segment",
    x = "Player Segment",
    y = "Contract Length (Years)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# HITTERS_FA_JOIN: BOX PLOT: TOTAL.SALARY DIFFERENCES BY CLUSTER:
ggplot(hitters_fa_join, aes(x = Cluster_Name, y = Total.Salary, fill = Cluster_Name)) +
  geom_boxplot() +
  labs(
    title = "Total Salary by Player Segment",
    x = "Player Segment",
    y = "Total Salary ($ Millions)"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::label_dollar(suffix = "M")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# HITTERS_FA_JOIN: EACH CLUSTER'S AVG AAV:
hitters_fa_join %>%
  group_by(Cluster_Name) %>%
  summarise(
    Avg_AAV = round(mean(Predicted_AAV_M, na.rm = TRUE)),
    Count = n())

# HITTERS_FA_JOIN: EACH CLUSTER'S AVG YEARS:
hitters_fa_join %>%
  group_by(Cluster_Name) %>%
  summarise(
    Avg_Years = round(mean(Predicted_Years, na.rm = TRUE)),
    Count = n())


# HITTERS_FA_JOIN: EACH CLUSTER'S AVG TOTAL.SALARY:
hitters_fa_join %>%
  group_by(Cluster_Name) %>%
  summarise(
    Avg_Total_Salary = round(mean(Total.Salary, na.rm = TRUE)),
    Count = n())


# HITTERS_FA_JOIN: TABLE: AAV, PREDICTED VS ACTUAL:
hitters_fa_join %>%
  mutate(
    Diff_AAV = round(Predicted_AAV_M - AAV, 2)
  ) %>%
  select(Name_clean, AAV, Predicted_AAV_M, Diff_AAV)


# HITTERS_FA_JOIN: TABLE: YEARS, PREDICTED VS ACTUAL:
hitters_fa_join %>%
  mutate(
    Diff_Years = round(Predicted_Years - Years, 2)
  ) %>%
  select(Name_clean, Years, Predicted_Years, Diff_Years)


# HITTERS_FA_JOIN: TABLE: TOTAL.SALARY, PREDICTED VS ACTUAL:
hitters_fa_join %>%
  mutate(
    Diff_Salary = round(Predicted_Total_Salary - Total.Salary, 2)
  ) %>%
  select(Name_clean, Total.Salary, Predicted_Total_Salary, Diff_Salary)


# HITTERS_FA_JOIN: PLOT: AAV MODEL ACCURACY:
ggplot(hitters_fa_join, aes(x = AAV, y = Predicted_AAV_M, color = Cluster_Name)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Linear Regression",
    x = "Actual AAV (Millions)", 
    y = "Predicted AAV (Millions)"
  ) +
  scale_x_continuous(labels = label_number(suffix = "M")) +
  scale_y_continuous(labels = label_number(suffix = "M")) +
  theme_minimal()


# HITTERS_FA_JOIN: PLOT: YEARS MODEL ACURACY:
ggplot(hitters_fa_join, aes(x = Years, y = Predicted_Years, color = Cluster_Name)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = seq(0, 15, 2), limits = c(0, 15)) +
  scale_y_continuous(breaks = seq(0, 15, 2), limits = c(0, 15)) +
  labs(
    title = "Linear Regression",
    x = "Actual Contract Length Years",
    y = "Predicted Contract Length Years",
    color = "Cluster_Name"
  ) +
  theme_minimal()


# HITTERS_FA_JOIN: PLOT: TOTAL SALARY MODEL ACCURACY:
ggplot(hitters_fa_join, aes(x = Total.Salary, y = Predicted_Total_Salary, color = Cluster_Name)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Linear Regression",
    x = "Actual Total Salary ($ Millions)",
    y = "Predicted Total Salary ($ Millions)"
  ) +
  scale_x_continuous(labels = scales::label_dollar(scale = 1, suffix = "M")) +
  scale_y_continuous(labels = scales::label_dollar(scale = 1, suffix = "M")) +
  theme_minimal()


# HITTERS_FA_JOIN: TABLE: MOST OVERPAID TO UNDERPAID PLAYERS:
view_table <- hitters_fa_join %>%
  mutate(
    Diff_M = round(Predicted_AAV_M - AAV, 2)
  ) %>%
  arrange(Diff_M) %>%
  select(
    Player = Name_clean,
    Year,
    `Actual AAV (M)` = AAV,
    `Predicted AAV (M)` = Predicted_AAV_M,
    `Diff (M)` = Diff_M
  )

View(view_table)


# HITTERS_FA_JOIN: REARRANGE COLUMNS:
hitters_fa_join <- hitters_fa_join %>%
  select(Name_clean, Age, Pos, Year, Years, Predicted_Years, Total.Salary,
         Predicted_Total_Salary, AAV, Predicted_AAV_M, Cluster_Name, Walk.Year.WAR, WAR,
         PA, AVG, OBP, SLG, HR, RBI, R, BB., K., SB, ISO, wOBA, Off, Def, BsR, 
         Cluster)


# PITCHERS_RAW_2022_2025 DF:
pitchers_raw_2022_2025 <- read.csv("fangraphs-leaderboards copy.csv")


# PITCHERS_RAW_2022_2025 DF: CLEANING:
pitchers_raw_2022_2025 <- pitchers_raw_2022_2025 %>%
  mutate(Name_clean = str_trim(str_to_lower(Name)))


# PITCHERS_CLEAN_2022_2025 DF CREATION:
pitchers_clean_2022_2025 <- pitchers_raw_2022_2025 %>%
  select("Name_clean", "W", "L", "SV", "IP", "K.9", "BB.9", "BABIP", "LOB.", 
         "GB.", "HR.FB", "vFA..pi.", "ERA", "FIP", "xFIP", "WAR")


# JOIN PITCHERS_CLEAN_2022_2025 AND FA_SIGNINGS_2022_2025:
pitchers_fa_join <- fa_signings_2022_2025 %>%
  left_join(pitchers_clean_2022_2025, by = "Name_clean")


# PITCHERS_FA_JOIN: FILTER OUT HITTERS:
pitchers_fa_join <- pitchers_fa_join %>%
  filter(Pos %in% c("SP", "RP", "P"))


# PITCHERS_FA_JOIN: REMOVE NA'S:
pitchers_fa_join <- na.omit(pitchers_fa_join)


# PITCHERS_FA_JOIN: CLUSTER ANALYSIS: SELECT VARIABLES:
clustering_input_p <- pitchers_fa_join %>%
  select(WAR, ERA, xFIP, IP, K.9, BB.9, vFA..pi., Age, AAV) %>%
  scale()


# PITCHERS_FA_JOIN: CLUSTER ANALYSIS: K-MEANS:
set.seed(123)
wss <- vector()
for (k in 1:10){
  kmeans_result_p <- kmeans (clustering_input_p, centers = k, nstart = 25)
  wss[k] <- kmeans_result_p$tot.withinss}


# PITCHERS_FA_JOIN: CLUSTER ANALYSIS: ELBOW PLOT:
elbow_df_p <- data.frame(Clusters = 1:10, WSS = wss)
ggplot(elbow_df_p, aes(x = Clusters, y = WSS)) +
  geom_point(size = 3) +
  geom_line() +
  labs(title = "Elbow Plot for K-Means Clustering (Pitchers)",
       x = "Number of Clusters (K)",
       y = "Total Within-Cluster Sum of Squares")


# PITCHERS_FA_JOIN: CLUSTER ANALYSIS: RE-RUN K-MEANS:
set.seed(123)
kmeans_final_p <- kmeans(clustering_input_p, centers = 3, nstart = 25)


# PITCHERS_FA_JOIN: ADD CLUSTER COLUMN:
pitchers_fa_join$Cluster <- as.factor(kmeans_final_p$cluster)


# PITCHERS_FA_JOIN: PROFILE EACH CLUSTER:
cluster_profiles_p <- pitchers_fa_join %>%
  group_by(Cluster) %>%
  summarise(
    Count = n(),
    WAR = mean(WAR, na.rm = TRUE),
    ERA = mean(ERA, na.rm = TRUE),
    xFIP = mean(xFIP, na.rm = TRUE),
    K.9 = mean(K.9, na.rm = TRUE),
    BB.9 = mean(BB.9, na.rm = TRUE),
    IP = mean(IP, na.rm = TRUE),
    vFA..pi. = mean(vFA..pi., na.rm = TRUE),
    Age = mean(Age, na.rm = TRUE),
    AAV = mean(AAV, na.rm = TRUE),
    .groups = "drop")


# PITCHERS_FA_JOIN: CREATING PCA COLUMNS:
pca_result_p <- prcomp(clustering_input_p)
pitchers_fa_join$PC1 <- pca_result_p$x[, 1]
pitchers_fa_join$PC2 <- pca_result_p$x[, 2]


# PITCHERS_FA_JOIN: PLOT: CLUSTER GROUPS:
ggplot(pitchers_fa_join, aes(x = -PC1, y = PC2, color = as.factor(Cluster))) +
  geom_point(alpha = 0.7) +
  scale_color_manual(
    name = "Cluster",
    values = c(
      "1" = "#F8766D",
      "2" = "#C49A00",
      "3" = "#53B400"
    ),
    labels = c(
      "1" = "Power Arms / Volatility Specialists",
      "2" = "Replacement Level",
      "3" = "Durable, Effective Starting Pitchers"
    )
  ) +
  labs(
    title = "Pitcher Segmentation Based on Performance and Pitching Style",
    x = "Performance Score (ERA, FIP, WAR, IP)",
    y = "Pitching Style Score (Velocity, K/9, BB/9)"
  ) +
  theme_minimal()


# PITCHERS_FA_JOIN: HOW MUCH EACH VARIABLE IMPACTS THE AXIS:
pca_result_p <- prcomp(clustering_input_p)
pca_result_p$rotation[, 1:2]  # only show PC1 and PC2


# PITCHERS_FA_JOIN: RENAMING THE CLUSTERS:
pitchers_fa_join <- pitchers_fa_join %>%
  mutate(Cluster_Name = case_when(
    Cluster == 1 ~ "Power Arms / Volatitlity Specialists",
    Cluster == 2 ~ "Replacement Level", 
    Cluster == 3 ~ "Durable, Effective Starting Pitchers",))


# PITCHERS_FA_JOIN: MAKE REPLACEMENT LEVEL CLUSTER THE BASELINE:
pitchers_fa_join$Cluster_Name <- relevel(factor(pitchers_fa_join$Cluster_Name), ref = "Replacement Level")


# PITCHERS_FA_JOIN: STANDARDIZING THE VARIABLES:
pitchers_standardized <- pitchers_fa_join %>%
  mutate(across(c("WAR", "ERA", "xFIP", "K.9", "BB.9", "IP", "vFA..pi.", "Age"), scale))
standardized_vars_p <- scale(pitchers_fa_join[, c("WAR", "ERA", "xFIP", "K.9", "BB.9", "IP", "vFA..pi.", "Age")])
standardized_df_p <- as.data.frame(standardized_vars_p)
standardized_df_p$Cluster_Name <- pitchers_fa_join$Cluster_Name


# PITCHERS_FA_JOIN: MODEL WITH VARIABLES: AAV AND YEARS
model_clean_aav_p <- lm(AAV ~ WAR + ERA + xFIP + K.9 + BB.9 + IP + vFA..pi. + Age, data = pitchers_standardized)
model_clean_years_p <- lm(Years ~ WAR + ERA + xFIP + K.9 + BB.9 + IP + vFA..pi. + Age, data = pitchers_standardized)


# PITCHERS_FA_JOIN: PRECITED AAV AND YEARS COLUMNS:
pitchers_fa_join <- pitchers_fa_join %>%
  mutate(
    Predicted_AAV_M_P = predict(model_clean_aav_p),
    Predicted_Years_P = predict(model_clean_years_p))


# PITCHERS_FA_JOIN: PREDICTED_TOTAL_SALARY COLUMN:
pitchers_fa_join <- pitchers_fa_join %>%
  mutate(Predicted_Total_Salary_P = Predicted_AAV_M_P * Predicted_Years_P)


# PITCHERS_FA_JOIN: PREDICTED, AAV, TOTAL_SAALRY, YEARS TO MILLIONS:
pitchers_fa_join <- pitchers_fa_join %>%
  mutate(
    Predicted_AAV_M_P = round(Predicted_AAV_M_P / 1e6, 2),
    Predicted_Years_P = round(Predicted_Years_P),  # whole number
    Predicted_Total_Salary_P = round(Predicted_Total_Salary_P / 1e6, 2))
pitchers_fa_join <- pitchers_fa_join %>%
  mutate(
    Total.Salary = round(Total.Salary / 1e6, 2),
    AAV = round(AAV / 1e6, 2))


# PITCHERS_FA_JOIN: SUMMARY OF MODELS:
summary(model_clean_aav_p)
summary(model_clean_years_p)


# PITCHERS_FA_JOIN: PLOT: WHAT VARIABLES AFFECT AAV:
tidy(model_clean_aav_p) %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = reorder(term, estimate), y = estimate / 1e6, fill = estimate > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Key Pitcher Profiles Driving Free Agent AAV",
    x = "Predictor",
    y = "Effect on AAV ($ Millions)"
  ) +
  scale_fill_manual(values = c("red", "green")) +
  scale_y_continuous(labels = scales::label_dollar(scale = 1, suffix = "M")) +
  theme_minimal()


# PITCHERS_FA_JOIN: PLOT: WHAT VARIABLES AFFECT YEARS:
tidy(model_clean_years_p) %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = reorder(term, estimate), y = estimate, fill = estimate > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Key Pitcher Profiles Driving Free Agent Contract Length",
    x = "Predictor",
    y = "Effect on Contract Length (Years)"
  ) +
  scale_fill_manual(values = c("red", "green")) +
  theme_minimal()


# PITCHERS_FA_JOIN: PLOT: AAV DIFFERENCES BY CLUSTER:
ggplot(pitchers_fa_join, aes(x = Cluster_Name, y = Predicted_AAV_M_P, fill = Cluster_Name)) +
  geom_boxplot() +
  labs(title = "Average Annual Value (AAV) by Player Segment",
       x = "Player Segment", y = "AAV ($ Millions)") +
  theme_minimal() +
  scale_y_continuous(labels = scales::label_dollar(suffix = "M")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# PITCHERS_FA_JOIN: PLOT: YEARS DIFFERENCES BY CLUSTER:
ggplot(pitchers_fa_join, aes(x = Cluster_Name, y = Predicted_Years_P, fill = Cluster_Name)) +
  geom_boxplot() +
  labs(
    title = "Contract Length (Years) by Player Segment",
    x = "Player Segment",
    y = "Contract Length (Years)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# PITCHERS_FA_JOIN: PLOT: TOTAL_SALARY DIFFERENCES BY CLUSTER:
ggplot(pitchers_fa_join, aes(x = Cluster_Name, y = Total.Salary, fill = Cluster_Name)) +
  geom_boxplot() +
  labs(
    title = "Total Salary by Player Segment",
    x = "Player Segment",
    y = "Total Salary ($ Millions)"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::label_dollar(suffix = "M")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# PITCHERS_FA_JOIN: TABLE: EACH CLUSTER'S AVG AAV:
pitchers_fa_join %>%
  group_by(Cluster_Name) %>%
  summarise(
    Avg_AAV_M = round(mean(Predicted_AAV_M_P, na.rm = TRUE)),
    Count = n())


# PITCHERS_FA_JOIN: TABLE: EACH CLUSTER'S AVG YEARS:
pitchers_fa_join %>%
  group_by(Cluster_Name) %>%
  summarise(
    Avg_Years = round(mean(Predicted_Years_P, na.rm = TRUE)),
    Count = n())


# PITCHERS_FA_JOIN: TABLE: EACH CLUSTER'S AVG TOTAL_SALARY:
pitchers_fa_join %>%
  group_by(Cluster_Name) %>%
  summarise(
    Avg_Total_Salary_M = round(mean(Total.Salary, na.rm = TRUE)),
    Count = n())


# PITCHERS_FA_JOIN: TABLE: AAV, PREDICTED VS ACTUAL:
pitchers_fa_join %>%
  mutate(
    AAV_M = round(AAV),  # assumes AAV is in dollars
    Predicted_AAV_M = round(Predicted_AAV_M_P)
  ) %>%
  mutate(
    Diff = round(Predicted_AAV_M_P - AAV_M, 2)
  ) %>%
  select(Name_clean, AAV_M, Predicted_AAV_M_P, Diff)


# PITCHERS_FA_JOIN: TABLE: YEARS, PREDICTED VS ACTUAL:
pitchers_fa_join %>%
  mutate(
    Years = round(Years),
    Predicted_Years = round(Predicted_Years_P)
  ) %>%
  mutate(
    Diff_Years = Predicted_Years - Years
  ) %>%
  select(Name_clean, Years, Predicted_Years, Diff_Years)


# PITCHERS_FA_JOIN: TABLE: TOTAL_SALARY, PREDICTED VS ACTUAL:
pitchers_fa_join %>%
  mutate(
    Total_Salary_M = round(Total.Salary),
    Predicted_Total_Salary_M = round(Predicted_Total_Salary_P, 2)
  ) %>%
  mutate(
    Diff_Total_Salary = round(Predicted_Total_Salary_M - Total_Salary_M, 2)
  ) %>%
  select(Name_clean, Total_Salary_M, Predicted_Total_Salary_M, Diff_Total_Salary)


# PITCHERS_FA_JOIN: PLOT: AAV, PREDICTED VS ACTUAL: (MODEL ACCURACY)
ggplot(pitchers_fa_join, aes(x = AAV, y = Predicted_AAV_M_P, color = Cluster_Name)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Linear Regression",
    x = "Actual AAV (Millions)", 
    y = "Predicted AAV (Millions)"
  ) +
  scale_x_continuous(labels = label_number(suffix = "M")) +
  scale_y_continuous(labels = label_number(suffix = "M")) +
  theme_minimal()


# PITCHERS_FA_JOIN: PLOT: YEARS, PREDICTED VS ACTUAL: (MODEL ACCURACY)
ggplot(pitchers_fa_join, aes(x = Years, y = Predicted_Years_P, color = Cluster_Name)) +
  geom_point() +
  geom_segment(aes(x = 0, y = 0, xend = 8, yend = 8), linetype = "dashed", color = "red") +
  labs(
    title = "Linear Regression",
    x = "Actual Contract Length (Years)",
    y = "Predicted Contract Length (Years)"
  ) +
  xlim(0, NA) +
  ylim(0, NA) +
  theme_minimal()


# PITCHERS_FA_JOIN: PLOT: TOTAL_SALARY, PREDICTED VS ACTUAL: (MODEL ACCURACY)
ggplot(pitchers_fa_join, aes(x = Total.Salary, y = Predicted_Total_Salary_P, color = Cluster_Name)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Actual vs. Predicted Total Salary by Cluster",
    x = "Actual Total Salary ($ Millions)",
    y = "Predicted Total Salary ($ Millions)"
  ) +
  scale_x_continuous(labels = scales::label_dollar(scale = 1, suffix = "M")) +
  scale_y_continuous(labels = scales::label_dollar(scale = 1, suffix = "M")) +
  theme_minimal()


# PITCHERS_FA_JOIN: TABLE: OVERPAID TO UNDERPAID PLAYERS:
view_table_p <- pitchers_fa_join %>%
  mutate(
    AAV = round(AAV, 2),
    Predicted_AAV_M_P = round(Predicted_AAV_M_P, 2),
    Diff_M = round(Predicted_AAV_M_P - AAV, 2)
  ) %>%
  arrange(Diff_M) %>%
  select(
    Player = Name_clean,
    Year,
    `Actual AAV (M)` = AAV,
    `Predicted AAV (M)` = Predicted_AAV_M_P,
    `Difference (M)` = Diff_M
  )

View(view_table_p)


# PITCHERS_FA_JOIN: REARRANGE COLUMNS:
pitchers_fa_join <- pitchers_fa_join %>%
  select(Name_clean, Age, Pos, Year, Years, Predicted_Years_P, Total.Salary,
         Predicted_Total_Salary_P, AAV, Predicted_AAV_M_P, Cluster_Name, Walk.Year.WAR, WAR,
         W, L, SV, IP, K.9, BB.9, BABIP, LOB., GB., HR.FB, vFA..pi., ERA, FIP, xFIP, WAR, PC1, PC2, Cluster)
pitchers_fa_join <- pitchers_fa_join %>%
  select(-Signing.Team)


# FA_2026_HITTERS_DF:
fa_2026_hitters <- read.csv("2026_hitters_fa.csv")


# FA_2026_HITTERS: RENAME COLUMN:
fa_2026_hitters <- fa_2026_hitters %>%
  rename(Name_clean = `Name_clean`)


# FA_2026_HITTERS: DROP COLUMNS:
fa_2026_hitters <- fa_2026_hitters %>%
  select(Name_clean, Age)
fa_2026_hitters <- fa_2026_hitters %>%
  filter(!is.na(Age) & Name_clean != "")


# FA_2026_HITTERS: UNCAPITALIZE AND REMOVE NAME ACCENTS:
fa_2026_hitters <- fa_2026_hitters %>%
  mutate(Name_clean = tolower(Name_clean))
fa_2026_hitters <- fa_2026_hitters %>%
  mutate(Name_clean = stri_trans_general(Name_clean, "Latin-ASCII"))


# JOIN FA_2026_HITTERS AND HITTERS_CLEAN_2022_2025:
fa_2026_hitters_stats <- fa_2026_hitters %>%
  left_join(hitters_clean_2022_2025, by = "Name_clean")


# FA_HITTERS_2026: GET RID OF REMAINING NA PLAYERS:
fa_2026_hitters_stats <- fa_2026_hitters_stats %>%
  filter(!is.na(WAR))


# FA_HITTERS_2026: CLUSTER ANALYSIS: MATCH VARIABLE SET:
clustering_vars <- c("WAR", "AVG", "OBP", "SLG", "HR", "RBI", "R", "BB.", "K.", "SB", "ISO", "wOBA", "Off", "Def", "BsR")


# FA_HITTERS_2026: CLUSTER ANALYSIS: SCALE 2026 FA DATA USING VARIABLES:
clustering_input_2026 <- fa_2026_hitters_stats %>%
  select(all_of(clustering_vars)) %>%
  scale()


# FA_HITTERS_2026: CLUSTER ANALYSIS: CALCULATE DISTANCES TO CLUSTER CENTERS:
distances <- as.matrix(dist(rbind(kmeans_final$centers, clustering_input_2026)))


# FA_HITTERS_2026: CLUSTER ANALYSIS: EXTRACT RELEVANT DISTANCES:
n_centers <- nrow(kmeans_final$centers)
new_obs_distances <- distances[(n_centers + 1):nrow(distances), 1:n_centers]


# FA_HITTERS_2026: CLUSTER ANALYSISL ASSIGN CLUSTERS BASED ON CLOSEST CENTER:
fa_2026_hitters_stats$Cluster <- apply(new_obs_distances, 1, which.min)
fa_2026_hitters_stats$Cluster <- as.factor(fa_2026_hitters_stats$Cluster)


# FA_HITTERS_2026: CLUSTER ANALYSIS: ASSIGN READABLE NAMES TO CLUSTERS:
cluster_map <- c(
  "1" = "Good Hitting, Bad Speed, Def, Ath",
  "2" = "Average Starters",
  "3" = "Speed & Defense Specialists",
  "4" = "Replacement Level",
  "5" = "Very Good Hitters")
fa_2026_hitters_stats$Cluster_Name <- cluster_map[as.character(fa_2026_hitters_stats$Cluster)]


# FA_2026_HITTERS_STATS: STANDARDIZING ALL PREDICTING VARIABLES:
fa_2026_standardized <- fa_2026_hitters_stats %>%
  mutate(across(c(WAR, SLG, HR, Age, BsR, `K.`), scale))
standardized_vars_2026 <- scale(fa_2026_hitters_stats[, c("WAR", "SLG", "HR", "Age", "BsR", "K.")])
standardized_df_2026 <- as.data.frame(standardized_vars_2026)
standardized_df_2026$Cluster_Name <- fa_2026_hitters_stats$Cluster_Name


# FA_2026_HITTERS_STATS: MODELS
fa_2026_hitters_stats$Predicted_AAV <- predict(model_clean_aav, newdata = fa_2026_standardized)
fa_2026_hitters_stats$Predicted_Years <- predict(model_clean_years, newdata = fa_2026_standardized)


# FA_2026_HITTERS_STATS: ROUND PREDICTED YEARS:
fa_2026_hitters_stats$Predicted_Years <- round(predict(model_clean_years, newdata = fa_2026_standardized))


# FA_2026_HITTERS_STATS: TOTAL.SALARY COLUMN:
fa_2026_hitters_stats <- fa_2026_hitters_stats %>%
  mutate(Predicted_Total_Salarly = Predicted_AAV * Predicted_Years)


# FA_2026_HITTERS_STATS: CONVERT COLUMNS TO MILLIONS:
fa_2026_hitters_stats$Predicted_AAV <- round(fa_2026_hitters_stats$Predicted_AAV / 1e6, 2)
fa_2026_hitters_stats$Predicted_Total_Salarly <- round(fa_2026_hitters_stats$Predicted_Total_Salarly / 1e6, 2)


# FA_2026_HITTERS_STATS: PREDICTED YEARS >= 1:
fa_2026_hitters_stats$Predicted_Years <- pmax(fa_2026_hitters_stats$Predicted_Years, 1)


# FA_2026_HITTERS_STATS: REARRANGE COLUMNS:
fa_2026_hitters_stats <- fa_2026_hitters_stats %>%
  select(Name_clean, Age, Predicted_Years, Predicted_Total_Salarly,
         Predicted_AAV, Cluster_Name, WAR, PA, AVG, OBP, SLG, HR, RBI, 
         R, BB., K., SB, ISO, wOBA, Off, Def, BsR, Cluster)


# FA_2026_PITCHERS DF:
fa_2026_pitchers <- read.csv("2026_pitchers_fa.csv")


#FA_2026_PITCHERS: RENAMING COLUMNS:
fa_2026_pitchers <- fa_2026_pitchers %>%
  rename('Name_clean' = `PLAYER..162.`)
fa_2026_pitchers <- fa_2026_pitchers %>%
  rename('Age' = `AGE`)


# FA_2026_PITCHERS: REVISE COLUMNS:
fa_2026_pitchers <- fa_2026_pitchers %>%
  select(Name_clean, Age)
fa_2026_hitters <- fa_2026_hitters %>%
  filter(!is.na(Age) & Name_clean != "")
fa_2026_pitchers <- fa_2026_pitchers %>%
  filter(!is.na(Age) & !is.na(Name_clean) & str_trim(Name_clean) != "")


# FA_2026_PITCHERS: UNCAPITALIZE NAMES:
fa_2026_pitchers <- fa_2026_pitchers %>%
  mutate(Name_clean = tolower(Name_clean))


# FA_2026_PITCHERS: REMOVE ACCENTS:
fa_2026_pitchers <- fa_2026_pitchers %>%
  mutate(Name_clean = stri_trans_general(Name_clean, "Latin-ASCII"))


#FA_2026_PITCHERS: REMOVE NA PLAYERS IN VFA..PI:
fa_2026_pitchers_stats <- fa_2026_pitchers_stats %>%
  filter(!is.na(vFA..pi.))


# JOIN FA_2026_PITCHERS AND PITCHERS_CLEAN_2022_2026:
fa_2026_pitchers_stats <- fa_2026_pitchers %>%
  left_join(pitchers_clean_2022_2025, by = "Name_clean")


# FA_2026_PITCHERS_STATS: GET RID OF REMAINING NA PLAYERS:
fa_2026_pitchers_stats <- fa_2026_pitchers_stats %>%
  filter(!is.na(WAR))


# FA_2026_PITCHING_STATS: GET RID OF DUPLICATE NAMES:
fa_2026_pitchers_stats <- fa_2026_pitchers_stats [!duplicated(fa_2026_pitchers_stats$Name_clean),]


# FA_2026_PITCHERS_STATS: CLUSTER ANALYSIS: SELECT VARIABLES:
clustering_vars_p <- c("WAR", "IP", "ERA", "FIP", "xFIP", "W", "L", "K.9", "BB.9",
                       "vFA..pi.", "BABIP", "LOB.", "GB.", "HR.FB") 


# FA_2026_PITCHERS_STATS: CLUSTER ANALYSIS: SCALE 2026 FA DATA USING VARIABLES:
clustering_input_2026_p <- fa_2026_pitchers_stats %>%
  select(all_of(clustering_vars_p)) %>%
  scale()


# FA_2026_PITCHERS_STATS: CLUSTER ANALYSIS: CALCULATE DISTANCES TO CLUSTER CENTERS:
distances_p <- as.matrix(dist(rbind(kmeans_final_p_2$centers, clustering_input_2026_p)))


# FA_2026_PITCHERS_STATS: CLUSTER ANALYSIS: EXTRACT RELEVANT DISTANCES:
n_centers_p <- nrow(kmeans_final_p_2$centers)
new_obs_distances_p <- distances_p[(n_centers_p + 1):nrow(distances_p), 1:n_centers_p]


# FA_2026_PITCHERS_STATS: CLUSTER ANALYSIS: ASSIGN CLUSTERS BASED ON CLOSEST CENTER:
fa_2026_pitchers_stats$Cluster <- apply(new_obs_distances_p, 1, which.min)
fa_2026_pitchers_stats$Cluster <- as.factor(fa_2026_pitchers_stats$Cluster)


# FA_2026_PITCHERS_STATS: CLUSTER ANALYSIS: ASSIGN READABLE NAMES TO CLUSTERS:
cluster_map_p <- c(
  "1: Replacement Level", 
  "2: Power Arms / Volatitlity Specialists", 
  "3: Durable, Effective Starting Pitchers")
fa_2026_pitchers_stats$Cluster_Name <- cluster_map_p[as.character(fa_2026_pitchers_stats$Cluster)]


# FA_2026_PITCHER_STATS: CLUSTER ANALYSIS: SWITCH CLUSTER GROUPS 2 AND 3
fa_2026_pitchers_stats$Cluster <- as.numeric(as.character(fa_2026_pitchers_stats$Cluster))
fa_2026_pitchers_stats$Cluster <- case_when(
  fa_2026_pitchers_stats$Cluster == 2 ~ 3,
  fa_2026_pitchers_stats$Cluster == 3 ~ 2,
  TRUE ~ fa_2026_pitchers_stats$Cluster)


# FA_2026_PITCHERS_STATS: CLUSTER_NAME COLUMN:
fa_2026_pitchers_stats <- fa_2026_pitchers_stats %>%
  mutate(Cluster_Name = case_when(
    Cluster == 1 ~ "Replacement Level",
    Cluster == 2 ~ "Power Arms / Volatitlity Specialists", 
    Cluster == 3 ~ "Durable, Effective Starting Pitchers",))


# FA_2026_PITCHERS_STATS: STANDARDIZING ALL PREDICTING VARIABLES:
fa_2026_standardized_p <- fa_2026_pitchers_stats %>%
  mutate(across(c(WAR, ERA, xFIP, K.9, BB.9, IP, vFA..pi., Age), scale))
standardized_vars_2026_p <- scale(fa_2026_pitchers_stats[, c("WAR", "ERA", "xFIP", "K.9", "BB.9", "IP", "vFA..pi.", "Age")])
standardized_df_2026_p <- as.data.frame(standardized_vars_2026_p)
standardized_df_2026_p$Cluster_Name <- fa_2026_pitchers_stats$Cluster_Name


# FA_2026_PITCHERS_STATS: MAKING REPLACEMENT LEVEL CLUSTER THE BASELINE:
fa_2026_pitchers_stats$Cluster_Name <- relevel(factor(fa_2026_pitchers_stats$Cluster_Name), ref = "Replacement Level")


# FA_2026_PITCHERS_STATS: MODELS:
fa_2026_pitchers_stats$Predicted_AAV <- predict(model_clean_aav_p, newdata = fa_2026_standardized_p)
fa_2026_pitchers_stats$Predicted_Years <- predict(model_clean_years_p, newdata = fa_2026_standardized_p)


# FA_2026_PITCHERS_STATS: ROUND PREDICTED YEARS:
fa_2026_pitchers_stats$Predicted_Years <- round(predict(model_clean_years_p, newdata = fa_2026_standardized_p))


# FA_2026_PITCHERS_STATS: TOTAL.SALARY COLUMN:
fa_2026_pitchers_stats <- fa_2026_pitchers_stats %>%
  mutate(Predicted_Total_Salarly = Predicted_AAV * Predicted_Years)


# FA_2026_PITCHERS_STATS: CONVERT TO MILLIONS:
fa_2026_pitchers_stats$Predicted_AAV <- round(fa_2026_pitchers_stats$Predicted_AAV / 1e6, 2)
fa_2026_pitchers_stats$Predicted_Total_Salarly <- round(fa_2026_pitchers_stats$Predicted_Total_Salarly / 1e6, 2)


# FA_2026_PITCHERS_STATS: PREDICTED YEARS >= 1:
fa_2026_pitchers_stats$Predicted_Years <- pmax(fa_2026_pitchers_stats$Predicted_Years, 1)


# FA_2026_PITCHERS_STATS: REARRANGE COLUMNS:
fa_2026_pitchers_stats <- fa_2026_pitchers_stats %>%
  select(Name_clean, Age, Predicted_Years, Predicted_Total_Salarly,
         Predicted_AAV, Cluster_Name, WAR, ERA, FIP, xFIP, IP, K.9, BB.9, 
         vFA..pi., W, L, SV, BABIP, LOB., GB., Cluster)


# --------------------- END OF MAIN PROJECT -----------------------


# RANDOMFOREST: SPLIT DATA INTO TRAIN/TEST:
set.seed(123)
train_index <- sample(1:nrow(hitters_fa_join), 0.8 * nrow(hitters_fa_join))
train <- hitters_fa_join[train_index, ]
test <- hitters_fa_join[-train_index, ]


# RANDOMFOREST MODEL:
rf_model <- randomForest(AAV ~ WAR + SLG + HR + Age + BsR + K. + Cluster_Name,
  data = train,
    ntree = 500,
      mtry = 3,
        importance = TRUE)


# RANDOMFOREST MODEL ELAVUATION:
predictions <- predict(rf_model, newdata = test)
mse <- mean((predictions - test$AAV)^2)
print(mse)
rss <- sum((predictions - test$AAV)^2)
tss <- sum((test$AAV - mean(test$AAV))^2)
r_squared <- 1 - rss/tss
print(r_squared)


# RANDOMFOREST MODEL IMPACT:
importance(rf_model)
varImpPlot(rf_model)


colnames(hitters_fa_join)[colnames(hitters_fa_join) == "Offensive Production"] <- "PC1"
colnames(hitters_fa_join)[colnames(hitters_fa_join) == "Speed, Defense, Athleticism"] <- "PC2"


# RANDOMFOREST MODEL_1_AAV:
set.seed(123)
rf_model_aav <- randomForest(AAV ~ Walk.Year.WAR + WAR + PA + AVG + OBP + SLG + HR + RBI + 
  R + BB. + K. + SB + ISO + wOBA + Off + Def + BsR + Cluster + PC1 + PC2,
    data = train,
      ntree = 500,
        mtry = 5,
          importance = TRUE)


# RANDOMFOREST MODEL_1_AAV ELAVUATION:
predictions_1 <- predict(rf_model_aav, newdata = test)
mse_1 <- mean((predictions_1 - test$AAV)^2)
print(mse_1)
rss_1 <- sum((predictions_1 - test$AAV)^2)
tss_1 <- sum((test$AAV - mean(test$AAV))^2)
r_squared_1 <- 1 - rss_1/tss_1
print(r_squared_1)


# RANDOMFOREST MODEL_1_AAV: ADJUSTED R-SQUARED: (0.72)
n_1 <- nrow(test) 
k_1 <- 18          
adj_r_squared_1 <- 1 - ((1 - r_squared_1) * (n_1 - 1) / (n_1 - k_1 - 1))
print(adj_r_squared_1)


# RANDROMFOREST MODEL_1_AAV: CORRELATION MATRIX:
# Get numeric predictors used in rf_model_1
vars_used <- all.vars(formula(rf_model_1))[-1]
numeric_vars <- vars_used[sapply(train[, vars_used], is.numeric)]
# Correlation matrix
cor_matrix <- cor(train[, numeric_vars], use = "complete.obs")
# Plot
dev.new()
corrplot(cor_matrix, method = "color", tl.cex = 0.7)


# RANDOMFOREST MODEL_2:
rf_model_2 <- randomForest(AAV ~ WAR + PA + AVG + OBP + SLG + 
  HR + K. + SB + wOBA + Def + BsR + Cluster,
     data = train,
       ntree = 500,
          mtry = 5,
            importance = TRUE)


# RANDOMFOREST MODEL_2 EVALUATION: (0.60)
# Predict on test set
predictions_2 <- predict(rf_model_2, newdata = test)
# Calculate R-squared
rss_2 <- sum((predictions_2 - test$AAV)^2)
tss_2 <- sum((test$AAV - mean(test$AAV))^2)
r_squared_2 <- 1 - rss_2 / tss_2
# Adjusted R-squared
n_2 <- nrow(test)
k_2 <- 12  # Number of predictors used in rf_model_2
adj_r_squared_2 <- 1 - ((1 - r_squared_2) * (n_2 - 1) / (n_2 - k_2 - 1))
# Output
print(r_squared_2)
print(adj_r_squared_2)


# ----- WE CAN SEE RF_MODEL_1 IS THE MNOST ACCURATE, WE WILL USE THAT MOVING FORWARD: -----


# FEATURE ENGINEERING: CREATING NEW METRICS:
  # WAR per PA:
train$WAR_per_PA <- train$WAR / train$PA
test$WAR_per_PA <- test$WAR / test$PA
  # SLG to OBP ratio:
train$SLG_OBP_ratio <- train$SLG / train$OBP
test$SLG_OBP_ratio <- test$SLG / test$OBP
  # Composite Athleticism Score:
train$Athleticism_Score <- (train$SB + train$BsR + train$`Speed, Defense, Athleticism`) / 3
test$Athleticism_Score <- (test$SB + test$BsR + test$`Speed, Defense, Athleticism`) / 3
  # Age Buckets:
train$Age_Group <- cut(train$Age, breaks = c(0, 25, 28, 31, 34, 40),
                       labels = c("Young", "Peak1", "Peak2", "Decline", "Old"))
test$Age_Group <- cut(test$Age, breaks = c(0, 25, 28, 31, 34, 40),
                      labels = c("Young", "Peak1", "Peak2", "Decline", "Old"))
  # Position Grouping:
premier_positions <- c("SS", "C", "CF")
train$Premier_Position <- ifelse(train$Pos %in% premier_positions, 1, 0)
test$Premier_Position  <- ifelse(test$Pos %in% premier_positions, 1, 0)
train$Premier_Position <- as.factor(train$Premier_Position)
test$Premier_Position  <- as.factor(test$Premier_Position)


# RF_Model_1_FE: (70):
train_clean <- na.omit(train)
rf_model_1_fe <- randomForest( AAV ~ Walk.Year.WAR + WAR + PA + AVG + OBP + SLG + HR + 
  RBI + R + BB. + K. + SB + ISO + wOBA + Off + Def + BsR + Cluster + Premier_Position +
    Age_Group,
  data = train_clean,
  ntree = 500,
  mtry = 5,
  importance = TRUE)


# RF_Model_1_FE: PREDICT:
pred_rf_fe <- predict(rf_model_1_fe, newdata = test)


# RF_Model_1_FE: ADJUSTED R-SQUARED (70):
rss_fe <- sum((pred_rf_fe - test$AAV)^2)
tss_fe <- sum((test$AAV - mean(test$AAV))^2)
r_squared_fe <- 1 - rss_fe / tss_fe
n_fe <- nrow(test)
k_fe <- 21  # 20 original + 3 engineered features
adj_r_squared_fe <- 1 - ((1 - r_squared_fe) * (n_fe - 1) / (n_fe - k_fe - 1))
print(adj_r_squared_fe)


# RANDOMFOREST MODEL_1_YEARS:
set.seed(123)
rf_model_years <- randomForest(Years ~ Walk.Year.WAR + WAR + PA + AVG + OBP + SLG + HR + RBI + 
  R + BB. + K. + SB + ISO + wOBA + Off + Def + BsR + Cluster + PC1 + PC2,
    data = train,
      ntree = 500,
        mtry = 5,
          importance = TRUE)


# RANDOMFOREST MODEL_1_YEARS: ELAVUATION:
predictions_1_years <- predict(rf_model_years, newdata = test)
mse_1_years <- mean((predictions_1_years - test$Years)^2)
print(mse_1_years)
rss_1_years <- sum((predictions_1_years - test$Years)^2)
tss_1_years <- sum((test$Years - mean(test$Years))^2)
r_squared_1_years <- 1 - rss_1_years/tss_1_years
print(r_squared_1_years)


# RANDOMFOREST MODEL_1_YEARS: ADJUSTED R-SQUARED: (0.74)
n_1_years <- nrow(test) 
k_1_years <- 18          
adj_r_squared_1_years <- 1 - ((1 - r_squared_1_years) * (n_1_years - 1) / (n_1_years - k_1_years - 1))
print(adj_r_squared_1_years)


# HITTERS_FA_JOIN: NEW COLUMNS:
hitters_fa_join$Predicted_AAV_rf <- predict(rf_model_aav, newdata = hitters_fa_join)
hitters_fa_join$Predicted_Years_rf <- predict(rf_model_years, newdata = hitters_fa_join)


# HITTERS_FA_JOIN: ROUND PREDICTED YEARS:
hitters_fa_join$Predicted_Years_rf <- round(predict(rf_model_years, newdata = hitters_fa_join))


# HITTERS_FA_JOIN: CONVERT COLUMNS TO MILLIONS:
hitters_fa_join$Predicted_AAV_rf <- round(hitters_fa_join$Predicted_AAV_rf, 2)


# HITTERS_FA_JOIN: TOTAL.SALARY COLUMN:
hitters_fa_join <- hitters_fa_join %>%
  mutate(Predicted_Total_Salary_rf = Predicted_AAV_rf * Predicted_Years_rf)


# HITTERS_FA_JOIN: SUMMARY OF MODELS:
print(rf_model_aav)
importance(rf_model_aav)
varImpPlot(rf_model_aav)
library(randomForest)
varImpPlot(rf_model_aav, 
           type = 1,  # or 2 for IncNodePurity
           main = "Variable Importance – Effect on AAV",
           pch = 16,
           col = "darkgreen")
print(rf_model_years)
print(rf_model_years)
importance(rf_model_years)
varImpPlot(rf_model_years,
           type = 1,
           main = "Variable Importance - Effect on Years",
           pch = 16,
           col = "darkgreen")


# HITTERS_FA_JOIN: BOX PLOT: AAV DIFFERENCES BY CLUSTER:
ggplot(hitters_fa_join, aes(x = Cluster_Name, y = Predicted_AAV_rf, fill = Cluster_Name)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$", suffix = "M")) +
  labs(title = "Predicted AAV by Player Segment (Random Forest Model)",
       x = "Player Segment",
       y = "Predicted AAV") +
  theme_minimal() +
  theme(legend.position = "none")
hitters_fa_join %>%
  group_by(Cluster_Name) %>%
  summarise(mean_rf = mean(Predicted_AAV_rf, na.rm = TRUE),
            count = n())



# HITTERS_FA_JOIN: BOX PLOT: YEARS DIFFERENCES BY CLUSTER:
ggplot(hitters_fa_join, aes(x = Cluster_Name, y = Predicted_Years_rf, fill = Cluster_Name)) +
  geom_boxplot() +
  scale_y_continuous(breaks = 0:10) +
  labs(title = "Predicted Contract Years by Player Segment (Random Forest Model)",
       x = "Player Segment",
       y = "Predicted Years") +
  theme_minimal() +
  theme(legend.position = "none")


# HITTERS_FA_JOIN: BOX PLOT: TOTAL.SALARY DIFFERENCES BY CLUSTER:
ggplot(hitters_fa_join, aes(x = Cluster_Name, y = Predicted_Total_Salary_rf, fill = Cluster_Name)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$", suffix = "M")) +
  labs(title = "Predicted Total Salary by Player Segment (Random Forest Model)",
       x = "Player Segment",
       y = "Predicted Total Salary") +
  theme_minimal() +
  theme(legend.position = "none")


# HITTERS_FA_JOIN: EACH CLUSTER'S AVG AAV:
hitters_fa_join %>%
  group_by(Cluster_Name) %>%
  summarise(
    Avg_AAV = round(mean(Predicted_AAV_rf, na.rm = TRUE)),
    Count = n())


# HITTERS_FA_JOIN: EACH CLUSTER'S AVG YEARS:
hitters_fa_join %>%
  group_by(Cluster_Name) %>%
  summarise(
    Avg_Years = round(mean(Predicted_Years_rf, na.rm = TRUE)),
    Count = n())


# HITTERS_FA_JOIN: EACH CLUSTER'S AVG TOTAL.SALARY:
hitters_fa_join %>%
  group_by(Cluster_Name) %>%
  summarise(
    Avg_Total_Salary = round(mean(Predicted_Total_Salary_rf, na.rm = TRUE)),
    Count = n())


# HITTERS_FA_JOIN: TABLE: AAV, PREDICTED VS ACTUAL:
hitters_fa_join %>%
  mutate(
    Diff_AAV = round(Predicted_AAV_rf - AAV, 20)
  ) %>%
  select(Name_clean, AAV, Predicted_AAV_rf, Diff_AAV)


# HITTERS_FA_JOIN: TABLE: YEARS, PREDICTED VS ACTUAL:
hitters_fa_join %>%
  mutate(
    Diff_Years = round(Predicted_Years_rf - Years, 2)
  ) %>%
  select(Name_clean, Years, Predicted_Years_rf, Diff_Years)


# HITTERS_FA_JOIN: TABLE: TOTAL.SALARY, PREDICTED VS ACTUAL:
hitters_fa_join %>%
  mutate(
    Diff_Salary = round(Predicted_Total_Salary_rf - Total.Salary, 2)
  ) %>%
  select(Name_clean, Total.Salary, Predicted_Total_Salary_rf, Diff_Salary)


# HITTERS_FA_JOIN: PLOT: AAV MODEL ACCURACY:
ggplot(hitters_fa_join, aes(x = AAV, y = Predicted_AAV_rf, color = Cluster_Name)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_segment(aes(x = 0, y = 0, xend = max(AAV), yend = max(AAV)), 
               color = "red", linetype = "dashed") +
  scale_x_continuous(labels = scales::dollar_format(prefix = "$", suffix = "M")) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$", suffix = "M")) +
  labs(title = "Random Forest",
       x = "Actual AAV (Millions)",
       y = "Predicted AAV (Millions)",
       color = "Cluster_Name") +
  theme_minimal()


# HITTERS_FA_JOIN: PLOT: YEARS MODEL ACURACY:
ggplot(hitters_fa_join, aes(x = Years, y = Predicted_Years_rf, color = Cluster_Name)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = seq(0, 15, 2), limits = c(0, 15)) +
  scale_y_continuous(breaks = seq(0, 15, 2), limits = c(0, 15)) +
  labs(
    title = "Random Forest",
    x = "Actual Contract Length Years",
    y = "Predicted Contract Length Years",
    color = "Cluster_Name"
  ) +
  theme_minimal()


# HITTERS_FA_JOIN: TABLE: MOST OVERPAID TO UNDERPAID PLAYERS:
view_table_rf <- hitters_fa_join %>%
  mutate(
    Diff_M = round(Predicted_AAV_rf - AAV, 2)
  ) %>%
  arrange(Diff_M) %>%
  select(
    Player = Name_clean,
    Year,
    `Actual AAV (M)` = AAV,
    `Predicted AAV (M)` = Predicted_AAV_rf,
    `Diff (M)` = Diff_M
  )
View(view_table_rf)


# HITTERS_FA_JOIN: MODEL W/O WALK.YEAR.WAR
set.seed(123)
rf_model_aav_no_war <- randomForest(AAV ~ WAR + PA + AVG + OBP + SLG + HR + RBI + 
  R + BB. + K. + SB + ISO + wOBA + Off + Def + BsR + Cluster,
    data = train,
      ntree = 500,
        mtry = 5,
          importance = TRUE)


# HITTERS_FA_JOIN: MODEL_AAV_NO_WAR EVALUATION:
predictions_no_war <- predict(rf_model_aav_no_war, newdata = test)
mse_no_war <- mean((predictions_no_war - test$AAV)^2)
print(mse_no_war)
rss_no_war <- sum((predictions_no_war - test$AAV)^2)
tss_no_war <- sum((test$AAV - mean(test$AAV))^2)
r_squared_no_war <- 1 - rss_no_war/tss_no_war
print(r_squared_no_war)


# HITTERS_FA_JOIN: MODEL_AAV_NO_WAR ADJUSTED R-SQUARED: (0.54)
n_no_war <- nrow(test) 
k_no_war <- 17          
adj_r_squared_no_war <- 1 - ((1 - r_squared_no_war) * (n_no_war - 1) / (n_no_war - k_no_war - 1))
print(adj_r_squared_no_war)


# HITTERS_FA_JOIN: REARRANGE COLUMNS:
hitters_fa_join <- hitters_fa_join %>%
  select(Name_clean, Age, Pos, Year, AAV, Predicted_AAV_M, Predicted_AAV_rf, 
         Years, Predicted_Years, Predicted_Years_rf, Total.Salary, Predicted_Total_Salary,
         Predicted_Total_Salary_rf, Cluster_Name, Walk.Year.WAR, WAR,
         PA, AVG, OBP, SLG, HR, RBI, R, BB., K., SB, ISO, wOBA, Off, Def, BsR, 
         PC1, PC2, Cluster)


# HITTERS_FA_JOIN: COMPARING LINEAR AAV VS RF AAV:
linear_preds_aav_h <- predict(model_clean_aav, newdata = hitters_standardized)
rmse_linear_aav_h <- sqrt(mean((hitters_standardized$AAV - linear_preds_aav_h)^2))
# Already computed for RF (use test and predictions_1_aav)
rmse_rf_aav_h <- sqrt(mean((test$AAV - predictions_1)^2))
# Compare RMSE
c(RMSE_Linear_AAV_Hitters = rmse_linear_aav_h, RMSE_RF_AAV_Hitters = rmse_rf_aav_h)
# Compared to the linear model, the Random Forest model for predicting hitter AAV achieved a lower RMSE (p = 3.61e+00 vs. 9.36e+06), 
  # indicating a substantially improved model fit and predictive accuracy. 
    # The sharp reduction in error suggests the Random Forest model captures underlying relationships in the data that the linear model fails to represent.


# HITTERS_FA_JOIN: COMPARING LINEAR YEARS VS RF YEARS:
# Linear model (if trained on standardized data)
linear_preds <- predict(model_clean_years, newdata = hitters_standardized)
rmse_linear <- sqrt(mean((hitters_standardized$Years - linear_preds)^2))
# Already computed for RF:
rmse_rf <- sqrt(mean((test$Years - predictions_1_years)^2))
c(RMSE_Linear = rmse_linear, RMSE_RF = rmse_rf)
"Compared to the linear model, the Random Forest model achieved a significantly higher adjusted R-squared on the test set (0.73 vs. 0.52) 
  # 7and a substantially lower RMSE (0.77 vs. 1.25), indicating stronger generalization and predictive accuracy when forecasting free agent contract lengths."


# PITCHERS_FA_JOIN: RANDOMFOREST AAV: SPLIT DATA INTO TRAIN/TEST:
set.seed(123456)
train_index_p <- sample(1:nrow(pitchers_fa_join), 0.8 * nrow(pitchers_fa_join))
train_p <- pitchers_fa_join[train_index_p, ]
test_p <- pitchers_fa_join[-train_index_p, ]


# PITCHERS_FA_JOIN: RANDOMFOREST MODEL AAV: (19 VARIABLES)
set.seed(123)
rf_model_aav_p19 <- randomForest(AAV ~ Walk.Year.WAR + WAR + W + L + SV + IP + K.9 + BB.9 + 
    BABIP + LOB. + GB. + HR.FB + vFA..pi. + ERA + FIP + xFIP + PC1 + PC2 + Cluster,
      data = train_p,
        ntree = 500,
          mtry = 3,
            importance = TRUE)


# PITCHERS_FA_JOIN: RANDOMFOREST MODEL AAV: (19 VARIABLES): EVALUATION
predictions_aav_p19 <- predict(rf_model_aav_p19, newdata = test_p)
mse_aav_p19 <- mean((predictions_aav_p19 - test_p$AAV)^2)
print(mse_aav_p19)
rss_aav_p19 <- sum((predictions_aav_p19 - test_p$AAV)^2)
tss_aav_p19 <- sum((test_p$AAV - mean(test_p$AAV))^2)
r_squared_aav_p19 <- 1 - rss_aav_p19/tss_aav_p19
print(r_squared_aav_p19)


# PITCHERS_FA_JOIN: RANDOMFOREST MODEL AAV: (19 VARIABLES): (0.76)
n_aav_p19 <- nrow(test_p)
k_aav_p19 <- 19  # number of predictors used
adj_r_squared_aav_p19 <- 1 - ((1 - r_squared_aav_p19) * (n_aav_p19 - 1) / (n_aav_p19 - k_aav_p19 - 1))
print(adj_r_squared_aav_p19)


# PITCHERS_FA_JOIN: RANDOMFOREST MODEL YEARS:(8 VARIABLES):
set.seed(123)
rf_model_years_p8 <- randomForest(Years ~ WAR + ERA + xFIP + K.9 + BB.9 + IP + vFA..pi. + Age,
  data = train_p,
    ntree = 500,
      mtry = 3,
        importance = TRUE)


# PITCHERS_FA_JOIN: RANDOMFOREST MODEL YEARS: (8 VARIABLES): EVALUATION
predictions_years_p8 <- predict(rf_model_years_p8, newdata = test_p)
mse_years_p8 <- mean((predictions_years_p8 - test_p$Years)^2)
print(mse_years_p8)
rss_years_p8 <- sum((predictions_years_p8 - test_p$Years)^2)
tss_years_p8 <- sum((test_p$Years - mean(test_p$Years))^2)
r_squared_years_p8 <- 1 - rss_years_p8/tss_years_p8
print(r_squared_years_p8)


# PITCHERS_FA_JOIN: RANDOMFOREST MODEL YEARS: (8 VARIABLES): (0.29)
n_years_p8 <- nrow(test_p)
k_years_p8 <- 8  # number of predictors used
adj_r_squared_years_p8 <- 1 - ((1 - r_squared_years_p8) * (n_years_p8 - 1) / (n_years_p8 - k_years_p8 - 1))
print(adj_r_squared_years_p8)


# PITCHERS_FA_JOIN: RANDOMFOREST MODEL YEARS: (19 VARIABLES)
set.seed(123)
rf_model_years_p19 <- randomForest(Years ~ Walk.Year.WAR + WAR + W + L + SV + IP + K.9 + BB.9 + 
  BABIP + LOB. + GB. + HR.FB + vFA..pi. + ERA + FIP + xFIP + PC1 + PC2 + Cluster,
    data = train_p,
      ntree = 500,
        mtry = 3,
          importance = TRUE)


# PITCHERS_FA_JOIN: RANDOMFOREST MODEL YEARS: (19 VARIABLES): EVALUATION
predictions_years_p19 <- predict(rf_model_years_p19, newdata = test_p)
mse_years_p19 <- mean((predictions_years_p19 - test_p$Years)^2)
print(mse_years_p19)
rss_years_p19 <- sum((predictions_years_p19 - test_p$Years)^2)
tss_years_p19 <- sum((test_p$Years - mean(test_p$Years))^2)
r_squared_years_p19 <- 1 - rss_years_p19/tss_years_p19
print(r_squared_years_p19)


# PITCHERS_FA_JOIN: RANDOMFOREST MODEL YEARS: (19 VARIABLES): (0.35)
n_years_p19 <- nrow(test_p)
k_years_p19 <- 9  # number of predictors used
adj_r_squared_years_p19 <- 1 - ((1 - r_squared_years_p19) * (n_years_p19 - 1) / (n_years_p19 - k_years_p19 - 1))
print(adj_r_squared_years_p19)


# PITCHERS_FA_JOIN: NEW COLUMNS:
pitchers_fa_join$Predicted_AAV_rf <- predict(rf_model_aav_p19, newdata = pitchers_fa_join)
pitchers_fa_join$Predicted_Years_rf <- predict(rf_model_years_p19, newdata = pitchers_fa_join)


# PITCHERS_FA_JOIN: ROUND PREDICTED YEARS:
pitchers_fa_join$Predicted_Years_rf <- round(predict(rf_model_years_p19, newdata = pitchers_fa_join))


# PITCHERS_FA_JOIN: CONVERT COLUMNS TO MILLIONS:
pitchers_fa_join$Predicted_AAV_rf <- round(pitchers_fa_join$Predicted_AAV_rf, 2)


# PITCHERS_FA_JOIN: TOTAL.SALARY COLUMN:
pitchers_fa_join <- pitchers_fa_join %>%
  mutate(Predicted_Total_Salary_rf = Predicted_AAV_rf * Predicted_Years_rf)


# PITCHERS_FA_JOIN: SUMMARY OF MODELS:
print(rf_model_aav_p19)
importance(rf_model_aav_p19)
varImpPlot(rf_model_aav_p19)
varImpPlot(rf_model_aav_p19, 
           type = 1,  # or 2 for IncNodePurity
           main = "Variable Importance – Effect on AAV",
           pch = 16,
           col = "darkgreen")
print(rf_model_years)
print(rf_model_years)
importance(rf_model_years_p19)
varImpPlot(rf_model_years_p19,
           type = 1,
           main = "Variable Importance - Effect on Years",
           pch = 16,
           col = "darkgreen")


# PITCHERS_FA_JOIN: BOX PLOT: AAV DIFFERENCES BY CLUSTER:
ggplot(pitchers_fa_join, aes(x = Cluster_Name, y = Predicted_AAV_rf, fill = Cluster_Name)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$", suffix = "M")) +
  labs(title = "Predicted AAV by Player Segment (Random Forest Model)",
       x = "Player Segment",
       y = "Predicted AAV") +
  theme_minimal() +
  theme(legend.position = "none")


# PITCHERS_FA_JOIN: BOX PLOT: YEARS DIFFERENCES BY CLUSTER:
ggplot(pitchers_fa_join, aes(x = Cluster_Name, y = Predicted_Years_rf, fill = Cluster_Name)) +
  geom_boxplot() +
  scale_y_continuous(breaks = 0:10) +
  labs(title = "Predicted Contract Years by Player Segment (Random Forest Model)",
       x = "Player Segment",
       y = "Predicted Years") +
  theme_minimal() +
  theme(legend.position = "none")


# PITCHERS_FA_JOIN: BOX PLOT: TOTAL.SALARY DIFFERENCES BY CLUSTER:
ggplot(pitchers_fa_join, aes(x = Cluster_Name, y = Predicted_Total_Salary_rf, fill = Cluster_Name)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$", suffix = "M")) +
  labs(title = "Predicted Total Salary by Player Segment (Random Forest Model)",
       x = "Player Segment",
       y = "Predicted Total Salary") +
  theme_minimal() +
  theme(legend.position = "none")


# PITCHERS_FA_JOIN: EACH CLUSTER'S AVG AAV:
pitchers_fa_join %>%
  group_by(Cluster_Name) %>%
  summarise(
    Avg_AAV = round(mean(Predicted_AAV_rf, na.rm = TRUE)),
    Count = n())


# PITCHERS_FA_JOIN: EACH CLUSTER'S AVG YEARS:
pitchers_fa_join %>%
  group_by(Cluster_Name) %>%
  summarise(
    Avg_Years = round(mean(Predicted_Years_rf, na.rm = TRUE)),
    Count = n())


# PITCHERS_FA_JOIN: EACH CLUSTER'S AVG TOTAL.SALARY:
pitchers_fa_join %>%
  group_by(Cluster_Name) %>%
  summarise(
    Avg_Total_Salary = round(mean(Predicted_Total_Salary_rf, na.rm = TRUE)),
    Count = n())


# PITCHERS_FA_JOIN: TABLE: AAV, PREDICTED VS ACTUAL:
pitchers_fa_join %>%
  mutate(
    Diff_AAV = round(Predicted_AAV_rf - AAV, 20)
  ) %>%
  select(Name_clean, AAV, Predicted_AAV_rf, Diff_AAV)


# PITCHERS_FA_JOIN: TABLE: YEARS, PREDICTED VS ACTUAL:
pitchers_fa_join %>%
  mutate(
    Diff_Years = round(Predicted_Years_rf - Years, 2)
  ) %>%
  select(Name_clean, Years, Predicted_Years_rf, Diff_Years)


# PITCHERS_FA_JOIN: TABLE: TOTAL.SALARY, PREDICTED VS ACTUAL:
pitchers_fa_join %>%
  mutate(
    Diff_Salary = round(Predicted_Total_Salary_rf - Total.Salary, 2)
  ) %>%
  select(Name_clean, Total.Salary, Predicted_Total_Salary_rf, Diff_Salary)


# PITCHERS_FA_JOIN: PLOT: AAV MODEL ACCURACY:
ggplot(pitchers_fa_join, aes(x = AAV, y = Predicted_AAV_rf, color = Cluster_Name)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_segment(aes(x = 0, y = 0, xend = max(AAV), yend = max(AAV)), 
               color = "red", linetype = "dashed") +
  scale_x_continuous(labels = scales::dollar_format(prefix = "$", suffix = "M")) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$", suffix = "M")) +
  labs(title = "Random Forest",
       x = "Actual AAV (Millions)",
       y = "Predicted AAV (Millions)",
       color = "Cluster_Name") +
  theme_minimal()


# PITCHERS_FA_JOIN: PLOT: YEARS MODEL ACURACY:
ggplot(pitchers_fa_join, aes(x = Years, y = Predicted_Years_rf, color = Cluster_Name)) +
  geom_point(size = 3, alpha = 0.85) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", linewidth = 1) +
  scale_x_continuous(breaks = seq(0, 8, by = 1), limits = c(0, 8), expand = expansion(mult = c(0, 0.05))) +
  scale_y_continuous(breaks = seq(0, 8, by = 1), limits = c(0, 8), expand = expansion(mult = c(0, 0.05))) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Random Forest",
    x = "Actual Contract Years",
    y = "Predicted Contract Years",
    color = "Player Cluster"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    legend.position = "right",
    panel.grid.minor = element_blank())
pitchers_fa_join$Cluster_Name <- factor(
  pitchers_fa_join$Cluster_Name,
  levels = c(
    "Replacement Level",
    "Durable, Effective Starting Pitchers",
    "Power Arms / Volatility Specialists"
  )
)


# PITCHERS_FA_JOIN: TABLE: MOST OVERPAID TO UNDERPAID PLAYERS:
view_table_rf_p <- pitchers_fa_join %>%
  mutate(
    Diff_M = round(Predicted_AAV_rf - AAV, 2)
  ) %>%
  arrange(Diff_M) %>%
  select(
    Player = Name_clean,
    Year,
    `Actual AAV (M)` = AAV,
    `Predicted AAV (M)` = Predicted_AAV_rf,
    `Diff (M)` = Diff_M
  )
View(view_table_rf_p)


# PITCHERS_FA_JOIN: REARRANGE COLUMNS:
pitchers_fa_join <- pitchers_fa_join %>%
  select(Name_clean, Age, Pos, Year, AAV, Predicted_AAV_M_P, Predicted_AAV_rf,
         Years, Predicted_Years_P, Predicted_Years_rf, Total.Salary, Predicted_Total_Salary_P,
         Predicted_Total_Salary_rf, Cluster_Name, Walk.Year.WAR, WAR, W, L, SV, IP, K.9,
         BB.9, BABIP, LOB., GB., HR.FB, vFA..pi., ERA, FIP, xFIP, PC1, PC2, Cluster)


# PITCHERS_FA_JOIN: COMPARING LINEAR AAV VS RF AAV:
linear_preds_aav_p <- predict(model_clean_aav_p, newdata = pitchers_standardized)
rmse_linear_aav_p <- sqrt(mean((pitchers_standardized$AAV - linear_preds_aav_p)^2))
# Already computed for RF (use test_aav_p and predictions_aav_p8)
rmse_rf_aav_p <- sqrt(mean((test_p$AAV - predictions_aav_p19)^2))
# Compare RMSE
c(RMSE_Linear_AAV_Pitchers = rmse_linear_aav_p, RMSE_RF_AAV_Pitchers = rmse_rf_aav_p)
# Compared to the linear model, the Random Forest model for predicting pitcher AAV achieved a lower RMSE (p = 3.93e+06 vs. 4.86e+06), 
  # suggesting improved model fit and predictive accuracy. However, the difference is relatively modest, indicating that both models perform 
    # similarly on this task


# PITCHERS_FA_JOIN: COMPARING LINEAR YEARS VS RF YEARS:
# Linear model (if trained on standardized data)
linear_preds_p <- predict(model_clean_years_p, newdata = pitchers_standardized)
rmse_linear_p <- sqrt(mean((pitchers_standardized$Years - linear_preds_p)^2))
# Already computed for RF (use test_p and predictions_years_p)
rmse_rf_p <- sqrt(mean((test_p$Years - predictions_years_p19)^2))
# Compare RMSE
c(RMSE_Linear_Pitchers = rmse_linear_p, RMSE_RF_Pitchers = rmse_rf_p)


# FA_2026_HITTERS_STATS: READING IN WAR:
War_2025 <- read.csv("fangraphs-leaderboards copy 2.csv")
war_2025_clean <- War_2025[, c("Name", "Team", "WAR", "G")]


# FA_2026_HITTERS_STATS: STANDARDIZE NAMES:
fa_2026_hitters_stats$Name_clean <- str_trim(fa_2026_hitters_stats$Name_clean)
war_2025_clean$Name <- str_trim(war_2025_clean$Name)


# FA_2026_HITTERS_STATS: MERGE WAR:
colnames(war_2025_clean)[colnames(war_2025_clean) == "Name"] <- "Name_clean"
fa_2026_hitters_stats$Name_clean <- tolower(trimws(fa_2026_hitters_stats$Name_clean))
war_2025_clean$Name_clean <- tolower(trimws(war_2025_clean$Name_clean))
fa_2026_hitters_stats <- merge(fa_2026_hitters_stats, 
                               war_2025_clean[, c("Name_clean", "WAR")], 
                               by = "Name_clean", 
                               all.x = TRUE)
names(fa_2026_hitters_stats)[names(fa_2026_hitters_stats) == "WAR.y"] <- "Walk.Year.War"
fa_2026_hitters_stats <- merge(fa_2026_hitters_stats, 
                               war_2025_clean[, c("Name_clean", "G")], 
                               by = "Name_clean", 
                               all.x = TRUE)
names(fa_2026_hitters_stats)
names(fa_2026_hitters_stats) <- make.names(names(fa_2026_hitters_stats), unique = TRUE)
fa_2026_hitters_stats <- fa_2026_hitters_stats %>%
  select(-G.x, -G.y, -Walk.Year.War.1) %>%
  rename(WAR = WAR.x)
fa_2026_hitters_stats <- fa_2026_hitters_stats %>% select(-WAR.x.1)


# FA_2026_HITTERS_STATS: ADD IN AND ESTIMATE PC1 AND PC2:
fa_2026_hitters_stats <- fa_2026_hitters_stats %>%
  left_join(hitters_fa_join %>% select(Name_clean, PC1, PC2), by = "Name_clean")
fa_2026_hitters_stats <- fa_2026_hitters_stats %>%
  mutate(
    PC1 = ifelse(round(PC1, 7) == 0.9523307, NA, PC1),
    PC2 = ifelse(round(PC2, 7) == 0.1684912, NA, PC2)
  )
# estimate PC1 and PC2 based on cluster groups:
# Group by Cluster_Name and fill missing PC1 and PC2 with group-wise medians
fa_2026_hitters_stats <- fa_2026_hitters_stats %>%
  group_by(Cluster_Name) %>%
  mutate(
    PC1 = ifelse(is.na(PC1), median(PC1, na.rm = TRUE), PC1),
    PC2 = ifelse(is.na(PC2), median(PC2, na.rm = TRUE), PC2)
  ) %>%
  ungroup()


# FA_2026_HITTERS_STATS: COLUMN RENAMING
fa_2026_hitters_stats <- subset(fa_2026_hitters_stats, select = -c(Team, WAR.y, Walk.Year.War, G))
# elimate na players:
fa_2026_hitters_stats <- fa_2026_hitters_stats[!is.na(fa_2026_hitters_stats$Walk.Year.War), ]
fa_2026_hitters_stats <- subset(fa_2026_hitters_stats, select = -c(Projected_Walk.Year.WAR, G.y, G.x))
names(fa_2026_hitters_stats)[names(fa_2026_hitters_stats) == "Walk.Year.War"] <- "War_to_date"
names(fa_2026_hitters_stats)[names(fa_2026_hitters_stats) == "WAR.x"] <- "WAR"


# FA_2026_HITTERS_STATS: ESTIMATE FULL SEASON WAR:
fa_2026_hitters_stats$Walk.Year.WAR <- (fa_2026_hitters_stats$Walk.Year.War / fa_2026_hitters_stats$G) * 162
names(fa_2026_hitters_stats)[names(fa_2026_hitters_stats) == "Walk.Year.War"] <- "Walk.Year.WAR"


# FA_2026_HITTERS_STATS:
fa_2026_hitters_stats$Predicted_AAV_rf <- predict(rf_model_aav, newdata = fa_2026_hitters_stats)
fa_2026_hitters_stats$Predicted_Years_rf <- predict(rf_model_years, newdata = fa_2026_hitters_stats)


# FA_2026_HITTERS_STATS: ROUND PREDICTED_EYARS_RF:
fa_2026_hitters_stats$Predicted_Years_rf <- round(fa_2026_hitters_stats$Predicted_Years_rf)
fa_2026_hitters_stats <- fa_2026_hitters_stats %>%
  mutate(across(c(Predicted_AAV, Predicted_AAV_rf, Predicted_Years, Predicted_Years_rf,
                  Total_Salary, Total_Salary_rf), ~ round(., 2)))


# FA_2026_HITTERS_STATS: TOTAL SALARY COLUMNS:
fa_2026_hitters_stats$Total_Salary <- fa_2026_hitters_stats$Predicted_AAV * fa_2026_hitters_stats$Predicted_Years
fa_2026_hitters_stats$Total_Salary_rf <- fa_2026_hitters_stats$Predicted_AAV_rf * fa_2026_hitters_stats$Predicted_Years_rf


# FA_2026_HITTERS_STATS: REARRANGE COLUMNS:
fa_2026_hitters_stats <- fa_2026_hitters_stats %>%
  select(Name_clean, Age, Predicted_AAV, Predicted_AAV_rf, Predicted_Years,
         Predicted_Years_rf, Total_Salary, Total_Salary_rf, Cluster_Name, Walk.Year.WAR, WAR, PA, AVG, OBP, SLG, HR, RBI, 
         R, BB., K., SB, ISO, wOBA, Off, Def, BsR, Cluster, PC1, PC2)


# FA_2026_PITCHERS_STATS: READING IN WAR:
War_2025_p <- read.csv("fangraphs-leaderboards-2.csv")


# FA_2026_PITCHERS_STATS: CLEAN AND STANDARDIZE
War_2025_p$Name <- str_trim(War_2025_p$Name)
fa_2026_pitchers_stats$Name_clean <- tolower(trimws(fa_2026_pitchers_stats$Name_clean))
War_2025_p$Name_clean <- tolower(trimws(War_2025_p$Name))


# FA_2026_PITCHERS_STATS: SAVE SUBSET AS NEW OBJECT:
war_2025_p_clean_subset <- War_2025_p[, c("Name_clean", "WAR", "G")]

# FA_2026_PITCHERS_STATS: Merge
fa_2026_pitchers_stats <- merge(fa_2026_pitchers_stats,
                                war_2025_p_clean_subset,
                                by = "Name_clean", all.x = TRUE)
names(fa_2026_pitchers_stats)[names(fa_2026_pitchers_stats) == "WAR.x"] <- "WAR"
names(fa_2026_pitchers_stats)[names(fa_2026_pitchers_stats) == "WAR.y"] <- "2025.WAR.td"


# FA_2026_PITCHERS_STATS: REMOVE NA VALUES: 
fa_2026_pitchers_stats <- fa_2026_pitchers_stats[!is.na(fa_2026_pitchers_stats$`2025.WAR.td`) & !is.na(fa_2026_pitchers_stats$G), ]


# FA_2026_PITCHERS_STATS: WALK.YEAR.WAR ESTIMATE:
fa_2026_pitchers_stats$Walk.Year.WAR <- fa_2026_pitchers_stats$`2025.WAR.td` / 0.45


# FA_2026_PITCHERS_STATS: MERGE PC1 AND PC2:
fa_2026_pitchers_stats <- merge(
  fa_2026_pitchers_stats,
  pitchers_fa_join[, c("Name_clean", "PC1", "PC2")],
  by = "Name_clean",
  all.x = TRUE)


# FA_2026_PITCHERS_STATS: ESTIMATE PC1 AND PC2 VALUES:
fa_2026_pitchers_stats <- fa_2026_pitchers_stats %>%
  group_by(Cluster_Name) %>%
  mutate(
    PC1 = ifelse(is.na(PC1), median(PC1, na.rm = TRUE), PC1),
    PC2 = ifelse(is.na(PC2), median(PC2, na.rm = TRUE), PC2)
  ) %>%
  ungroup()


# FA_2026_PITCHERS_STATS: READ IN HR.FB DATA:
fa_2026_pitchers_stats <- merge(
  fa_2026_pitchers_stats,
  pitchers_fa_join[, c("Name_clean", "HR.FB")],
  by = "Name_clean",
  all.x = TRUE)

# FA_2026_PITCHERS_STATS: ESTIMATE HR.FB:
fa_2026_pitchers_stats <- fa_2026_pitchers_stats %>%
  group_by(Cluster_Name) %>%
  mutate(
    HR.FB = ifelse(is.na(HR.FB), median(HR.FB, na.rm = TRUE), HR.FB),
  ) %>%
  ungroup()


# FA_2026_PITCHERS: NEW COLUMNS:
fa_2026_pitchers_stats$Predicted_AAV_rf <- predict(rf_model_aav_p19, newdata = fa_2026_pitchers_stats)
fa_2026_pitchers_stats$Predicted_Years_rf <- predict(rf_model_years_p19, newdata = fa_2026_pitchers_stats)


# FA_2026_PITCHERS_STATS: ROUND PREDICTED_YEARS_RF:
fa_2026_pitchers_stats$Predicted_Years_rf <- round(fa_2026_pitchers_stats$Predicted_Years_rf)


# FA_2026_PITCHERS_STATS: TOTAL SALARY COLUMNS:
fa_2026_pitchers_stats$Total_Salary <- fa_2026_pitchers_stats$Predicted_AAV * fa_2026_pitchers_stats$Predicted_Years
fa_2026_pitchers_stats$Total_Salary_rf <- fa_2026_pitchers_stats$Predicted_AAV_rf * fa_2026_pitchers_stats$Predicted_Years_rf


# FA_2026_PITCHERS_STATS: ROUND VALUES:
fa_2026_pitchers_stats <- fa_2026_pitchers_stats %>%
  mutate(
    Predicted_AAV = round(Predicted_AAV, 2),
    Predicted_AAV_rf = round(Predicted_AAV_rf, 2),
    Predicted_Years = round(Predicted_Years, 0),
    Predicted_Years_rf = round(Predicted_Years_rf, 0),
    Total_Salary = round(Total_Salary, 2),
    Total_Salary_rf = round(Total_Salary_rf, 2))


# FA_2026_PITCHERS_STATS: REARRANGE COLUMNS:
fa_2026_pitchers_stats <- fa_2026_pitchers_stats %>%
  select(Name_clean, Age, Predicted_AAV, Predicted_AAV_rf, Predicted_Years, Predicted_Years_rf,
         Total_Salary, Total_Salary_rf, Cluster_Name, WAR, ERA, FIP, xFIP, IP, K.9, BB.9, 
         vFA..pi., W, L, SV, BABIP, LOB., GB., Cluster)
fa_2026_pitchers_stats <- fa_2026_pitchers_stats %>%
  distinct(Name_clean, .keep_all = TRUE)


save.image(file = "FULLCOMPLETEPROJECT.RData")
save.image(file = "FinalProject.RData2.Rdata")
write.csv(FULLCOMPLETEPROJECT.RData, "FULLCOMPLETER.DATA.csv", row.names = FALSE)
load("FinalProject.RData2.Rdata")


















































