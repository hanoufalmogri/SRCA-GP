library(dplyr)
library(emmeans)
library(sandwich)
library(lmtest)
library(car)
library(ggplot2)
library(tidyr)
library(lubridate)
library(effectsize)
library(data.table)
library(MASS)
library(car)
library(psych)
library(stringr)
library(forcats)
library(rstatix)
library(tidyr)
library(factoextra)
library(cluster)

path_csv ='/Users/alhanoufalmogri/Desktop/projects/Graduation Project/GP Data/TRY 2 CSV.csv'

DT = fread(path_csv)


col_response_time = "ResponseTime"               
col_report_source = "SourceName"               
col_region        = "RegionName"                 
col_dispatch      = "PQADispatchLevel"           
col_vehicle       = "VehicleTypeName"         
col_location      = "LocationTypeName"   
col_incident_type = "IncidentTypeName" 
col_incident_time = "IncidentCreationStartTime" 
col_paient_age    = "PatientAge"
col_callhandle    = "CallHandlingTime"
col_los           = "LOS"
col_Hour          = "Hour" 
col_WeekDay       = "WeekDay" 

if (col_incident_time %in% names(DT)) {
  DT[, IncidentCreation := dmy_hms(get(col_incident_time), quiet = TRUE)]
  DT[, Hour   := hour(IncidentCreation)]
  DT[, WeekDay  := weekdays(IncidentCreation)]  
} else {
  DT[, `:=`(Hour = NA_integer_, WeekDay = NA)]
} 
## to extract temporal variables 

dRriyadh         = (DT %>% filter(RegionName == "Riyadh"))
dMakkah          = (DT %>% filter(RegionName == "Makkah"))
dEastren         = (DT %>% filter(RegionName == "Eastren"))
dMadinah         = (DT %>% filter(RegionName == "Madinah"))
dAseer           = (DT %>% filter(RegionName == "Aseer"))
dQassim          = (DT %>% filter(RegionName == "Qassim"))
dJazan           = (DT %>% filter(RegionName == "Jazan"))
dTabuk           = (DT %>% filter(RegionName == "Tabuk"))
dHail            = (DT %>% filter(RegionName == "Hail"))
dAlbahah         = (DT %>% filter(RegionName == "Albahah"))
dNajran          = (DT %>% filter(RegionName == "Najran"))
dAljowf          = (DT %>% filter(RegionName == "Aljowf"))
dNorthernBorders = (DT %>% filter(RegionName == "NorthernBorders"))


######################### mean of each province ######################### 

(mean(DT$ResponseTime))/60
(mean(dRriyadh$ResponseTime))/60
(mean(dMakkah$ResponseTime))/60
(mean(dEastren$ResponseTime))/60
(mean(dMadinah$ResponseTime))/60
(mean(dAseer$ResponseTime))/60
(mean(dQassim$ResponseTime))/60
(mean(dJazan$ResponseTime))/60
(mean(dTabuk$ResponseTime))/60
(mean(dHail$ResponseTime))/60
(mean(dAlbahah$ResponseTime))/60
(mean(dNajran$ResponseTime))/60
(mean(dAljowf$ResponseTime))/60
(mean(dNorthernBorders$ResponseTime))/60

######################### trasform RT from seconds to minutes ######################### 
dRriyadh = dRriyadh %>%
  mutate(ResponseTime_min = (ResponseTime/60))

######################### Discriptive Statistics ######################### 
# Summary stats
summary(dRriyadh$ResponseTime_min)
sd(dRriyadh$ResponseTime_min, na.rm = TRUE)

# Histogram 
hist(dRriyadh$ResponseTime_min,
     breaks = 1000,               
     col = "salmon",
     border = "white",
     main = "Distribution of Response Times (Riyadh, Jan–Aug 2024)",
     xlab = "Response Time (minutes)",
     xlim = c(0, quantile(dRriyadh$ResponseTime_min, 0.99, na.rm = TRUE))  
)



#Boxplot
par(mar = c(5, 5, 4, 2))          
boxplot(dRriyadh$ResponseTime_min,
        horizontal = TRUE,
        col = "salmon",
        main = "Boxplot of Response Times (Riyadh, Jan–Aug 2024)",
        xlab = "Response Time (minutes)",
        width = 2,                
        outline = TRUE)


report_stats = dRriyadh %>%
  group_by(SourceName) %>%
  summarise(
    N = n(),
    Mean = mean(ResponseTime_min, na.rm = TRUE),
    Median = median(ResponseTime_min, na.rm = TRUE),
    SD = sd(ResponseTime_min, na.rm = TRUE),
    Q1 = quantile(ResponseTime_min, 0.25, na.rm = TRUE),
    Q3 = quantile(ResponseTime_min, 0.75, na.rm = TRUE)
  )

report_stats

location_stats = dRriyadh %>%
  group_by(LocationTypeName) %>%
  summarise(
    N = n(),
    Mean = mean(ResponseTime_min, na.rm = TRUE),
    Median = median(ResponseTime_min, na.rm = TRUE),
    SD = sd(ResponseTime_min, na.rm = TRUE),
    Q1 = quantile(ResponseTime_min, 0.25, na.rm = TRUE),
    Q3 = quantile(ResponseTime_min, 0.75, na.rm = TRUE)
  )

location_stats

# Boxplot by Report Source
boxplot(ResponseTime_min ~ SourceName, data = dRriyadh,
        col = "salmon",
        main = "Response Times by Report Source (Riyadh, Jan–Aug 2024)",
        ylab = "Response Time (minutes)",
        xlab = "Report Source",
        las = 2)   # rotate labels for readability



ggplot(report_stats, aes(x = SourceName, y = Mean, fill = SourceName)) +
  geom_col() +
  geom_errorbar(aes(ymin = Q1, ymax = Q3), width = 0.2) +  
  labs(title = "Mean Response Time by Report Source",
       y = "Response Time (minutes)", x = "Report Source") +
  theme_minimal()

dispatch_stats <- dRriyadh %>%
  group_by(PQADispatchLevel) %>%
  summarise(
    N = n(),
    Mean = mean(ResponseTime_min, na.rm = TRUE),
    Median = median(ResponseTime_min, na.rm = TRUE),
    SD = sd(ResponseTime_min, na.rm = TRUE),
    Q1 = quantile(ResponseTime_min, 0.25, na.rm = TRUE),
    Q3 = quantile(ResponseTime_min, 0.75, na.rm = TRUE)
  ) %>%
  arrange(Mean)   

dispatch_stats

ggplot(dispatch_stats, aes(x = reorder(PQADispatchLevel, Mean), y = Mean, fill = PQADispatchLevel)) +
  geom_col() +
  geom_errorbar(aes(ymin = Q1, ymax = Q3), width = 0.2) +
  labs(title = "Mean Response Time by Dispatch Level",
       y = "Response Time (minutes)", x = "Dispatch Level") +
  theme_minimal()


vehicle_stats = dRriyadh %>%
  group_by(VehicleTypeName) %>%
  summarise(
    N = n(),
    Mean = mean(ResponseTime_min, na.rm = TRUE),
    Median = median(ResponseTime_min, na.rm = TRUE),
    SD = sd(ResponseTime_min, na.rm = TRUE),
    Q1 = quantile(ResponseTime_min, 0.25, na.rm = TRUE),
    Q3 = quantile(ResponseTime_min, 0.75, na.rm = TRUE)
  ) %>%
  arrange(Mean)

vehicle_stats




ggplot(vehicle_stats, aes(x = reorder(VehicleTypeName, Mean), y = Mean, fill = VehicleTypeName)) +
  geom_col() +
  geom_errorbar(aes(ymin = Q1, ymax = Q3), width = 0.2) +
  labs(title = "Mean Response Time by Vehicle Type",
       y = "Response Time (minutes)", x = "Vehicle Type") +
  theme_minimal()


# Summary by hour
hour_stats = dRriyadh %>%
  group_by(Hour) %>%
  summarise(
    N = n(),
    Mean = mean(ResponseTime/60, na.rm = TRUE),
    Median = median(ResponseTime/60, na.rm = TRUE)
  )

hour_stats


weekday_stats = dRriyadh %>%
  group_by(WeekDay) %>%
  summarise(
    N = n(),
    Mean = mean(ResponseTime/60, na.rm = TRUE),
    Median = median(ResponseTime/60, na.rm = TRUE)
  )

weekday_stats

ggplot(weekday_stats, aes(x = WeekDay, y = Mean, fill = WeekDay)) +
  geom_col() +
  labs(title="Mean Response Time by Weekday",
       x="Day of Week", y="Response Time (minutes)") +
  theme_minimal()

dRriyadh$Month = format(dRriyadh$IncidentCreation, "%B")

month_stats = dRriyadh %>%
  group_by(Month) %>%
  summarise(
    N = n(),
    Mean = mean(ResponseTime/60, na.rm = TRUE),
    Median = median(ResponseTime/60, na.rm = TRUE)
  )


month_stats

ggplot(month_stats, aes(x = Month, y = Mean, group=1)) +
  geom_line(color="salmon") +
  geom_point() +
  labs(title="Mean Response Time by Month (Riyadh, Jan–Aug 2024)",
       x="Month", y="Response Time (minutes)") +
  theme_minimal()


incident_stats = dRriyadh %>%
  group_by(IncidentTypeName) %>%
  summarise(
    N = n(),
    Mean = mean(ResponseTime_min, na.rm = TRUE),
    Median = median(ResponseTime_min, na.rm = TRUE),
    SD = sd(ResponseTime_min, na.rm = TRUE),
    Q1 = quantile(ResponseTime_min, 0.25, na.rm = TRUE),
    Q3 = quantile(ResponseTime_min, 0.75, na.rm = TRUE)
  ) %>%
  arrange(Mean)

incident_stats



# after this line we will deal with the data cleaning 

dRriyadh = subset(dRriyadh, !SourceName %in% c("almusaf alelcrtoni", "Almusaf Alelcrtoni"))

#we can see from the histogram that the data become cleaner when we applide the IQR meathod so we will use it 


# Convert numeric variables
to_numeric_safe <- function(x) as.numeric(ifelse(x %in% c("NULL", "", "NA"), NA, x))

dRriyadh$CallHandlingTime <- to_numeric_safe(dRriyadh$CallHandlingTime)
dRriyadh$LOS <- to_numeric_safe(dRriyadh$LOS)
dRriyadh$PatientAge <- to_numeric_safe(dRriyadh$PatientAge)

# Convert timestamps to proper POSIXct format
dRriyadh$IncidentCreationStartTime <- as.POSIXct(dRriyadh$IncidentCreationStartTime, 
                                                 format="%d-%m-%Y %H:%M:%S")
dRriyadh$SendToDispatcherTime <- as.POSIXct(dRriyadh$SendToDispatcherTime, 
                                            format="%d-%m-%Y %H:%M:%S")
dRriyadh$AcceptRefuseTime <- as.POSIXct(dRriyadh$AcceptRefuseTime, 
                                        format="%d-%m-%Y %H:%M:%S")



dRriyadh = dRriyadh %>%
   mutate (District = na_if(District, ""))

dRriyadh$SourceName = as.factor(dRriyadh$SourceName)
dRriyadh$IncidentTypeName = as.factor(dRriyadh$IncidentTypeName)
dRriyadh$LocationTypeName = as.factor(dRriyadh$LocationTypeName)
dRriyadh$VehicleTypeName = as.factor(dRriyadh$VehicleTypeName)
dRriyadh$IncidentPriority = as.factor(dRriyadh$IncidentPriority)
dRriyadh$PQADispatchLevel = as.factor(dRriyadh$PQADispatchLevel)
dRriyadh$WeekDay = as.factor(dRriyadh$WeekDay)


table(dRriyadh$RegionName); dRriyadh$RegionName <- NULL  


miss_sum = sapply(dRriyadh, function(x) sum(is.na(x)))
print(miss_sum[miss_sum > 0][order(-miss_sum[miss_sum > 0])])

cat("\nGroup sizes (watch for very small groups):\n")
print(table(dRriyadh$SourceName))
print(table(dRriyadh$LocationTypeName))
print(table(dRriyadh$IncidentPriority))
print(table(dRriyadh$PQADispatchLevel))
print(head(sort(table(dRriyadh$IncidentTypeName), decreasing = TRUE), 10))

#since omega cases considerd small i will murge it with echo under the name ECHO
dRriyadh = dRriyadh %>%
  mutate(PQADispatchLevel = fct_collapse(PQADispatchLevel,
                                         ECHO = c("ECHO", "OMEGA")))
print(table(dRriyadh$PQADispatchLevel))



######################### Transformation ######################### 
bc = boxcox(lm(ResponseTime_min ~ 1, data = dRriyadh), 
             lambda = seq(-2, 2, 0.1))


lambda_best = bc$x[which.max(bc$y)]
lambda_best

dRriyadh = dRriyadh %>%
  mutate(transRT =
           if(lambda_best == 0) log(ResponseTime_min)
         else (ResponseTime_min^lambda_best - 1) / lambda_best)


hist(dRriyadh$transRT,
     breaks = 100,                
     col = "salmon",
     border = "black",
     main = "Distribution of Response Times (Riyadh, Jan–Aug 2024) After Transformation",
     xlab = "Response Time After Transformation",
)  
qqnorm(dRriyadh$transRT); qqline(dRriyadh$transRT, col="salmon")

shapiro.test(sample(dRriyadh$ResponseTime_min, 50)) # data before transformation sample of 50, not normal

shapiro.test(sample(dRriyadh$transRT, 50)) #when we take a random sample of 50 data is normal

describe(dRriyadh$transRT)[, c("skew", "kurtosis")] ##Acceptable skewness and kurtos

ggplot(dRriyadh, aes(y = dRriyadh$transRT)) +
  geom_boxplot(fill="salmon") +
  labs(title="Response Time Outliers", y="Minutes")

summary(dRriyadh$transRT) # mean almost equal to median 

#or - trnasform the sec insted of the min but we will have the exact same results

dRriyadh <- dRriyadh %>%
  mutate(transRTsec =
           if(lambda_best == 0) log(ResponseTime)
         else (ResponseTime^lambda_best - 1) / lambda_best)

hist(dRriyadh$transRTsec, breaks = 60)
qqnorm(dRriyadh$transRTsec); qqline(dRriyadh$transRTsec, col="salmon")

shapiro.test(sample(dRriyadh$ResponseTime, 50)) # data before transformation sample of 50, not normal
shapiro.test(sample(dRriyadh$transRTsec, 50)) #when we take a random sample of 50 data is normal
describe(dRriyadh$transRTsec)[, c("skew", "kurtosis")] ##Acceptable skewness and kurtosis
summary(dRriyadh$transRTsec) # mean almost equal to median 




######################### leveneTest ######################### 

leveneTest(transRT ~ PQADispatchLevel , data = dRriyadh)
leveneTest(transRT ~ SourceName , data = dRriyadh)
leveneTest(transRT ~ VehicleTypeName , data = dRriyadh)
leveneTest(transRT ~ LocationTypeName , data = dRriyadh)
leveneTest(transRT ~ IncidentTypeName , data = dRriyadh)
leveneTest(transRT ~ WeekDay , data = dRriyadh)

bartlett.test(transRT ~ PQADispatchLevel , data = dRriyadh)
bartlett.test(transRT ~ SourceName , data = dRriyadh)
##try bartlet##

#Violated but fine if we use robust methods "HC3"

######################### One way ANOVA ######################### 

oneWaySource = oneway.test(transRT ~ SourceName, data = dRriyadh, var.equal = FALSE)
oneWayDisp = oneway.test(transRT ~ PQADispatchLevel, data = dRriyadh, var.equal = FALSE)
oneWayVehi = oneway.test(transRT ~ VehicleTypeName, data = dRriyadh, var.equal = FALSE)
oneWayLoc = oneway.test(transRT ~ LocationTypeName, data = dRriyadh, var.equal = FALSE)
oneWayIns = oneway.test(transRT ~ dRriyadh$IncidentTypeName, data = dRriyadh, var.equal = FALSE)
oneWayDay = oneway.test(transRT ~ dRriyadh$WeekDay, data = dRriyadh, var.equal = FALSE)

print(oneWaySource); print(oneWayDisp); print(oneWayVehi);print(oneWayLoc);print(oneWayIns);print(oneWayDay)

# ---- Games–Howell post-hoc Under {rstatix} Package ----

ghSource = games_howell_test(dRriyadh, transRT ~ SourceName)
ghDisp   = games_howell_test(dRriyadh, transRT ~ PQADispatchLevel)
ghVehi   = games_howell_test(dRriyadh, transRT ~ VehicleTypeName)
ghLoc    = games_howell_test(dRriyadh, transRT ~ LocationTypeName)
ghIns    = games_howell_test(dRriyadh, transRT ~ IncidentTypeName)
ghDay    = games_howell_test(dRriyadh, transRT ~ WeekDay)

print(ghSource); print(ghDisp); print(ghVehi);print(ghLoc);print(ghIns);print(ghDay)

sig_incident = ghIns %>%
  filter(p.adj < 0.05) %>%
  arrange(p.adj)
write.csv(sig_incident, "SignificantIncidentTypeGH.csv", row.names = FALSE)

sig_day = ghDay %>%
  filter(p.adj < 0.05) %>%
  arrange(p.adj)
write.csv(sig_day, "SignificantWeekDayGH.csv", row.names = FALSE)

sig_Loc = ghLoc %>%
  filter(p.adj < 0.05) %>%
  arrange(p.adj)
write.csv(sig_Loc, "SignificantLocType.csv", row.names = FALSE)

sig_Vehi = ghVehi %>%
  filter(p.adj < 0.05) %>%
  arrange(p.adj)
write.csv(sig_Vehi, "SignificantVehiGH.csv", row.names = FALSE)

sig_Disp = ghDisp %>%
  filter(p.adj < 0.05) %>%
  arrange(p.adj)
write.csv(sig_Disp, "SignificantDispGH.csv", row.names = FALSE)

sig_Source = ghSource %>%
  filter(p.adj < 0.05) %>%
  arrange(p.adj)
write.csv(sig_Source, "SignificantSourceGH.csv", row.names = FALSE)

write.csv(ghSource, "sourceTypeGH.csv", row.names = FALSE)
write.csv(ghDisp, "dispTypeGH.csv", row.names = FALSE)
write.csv(ghVehi, "vehiTypeGH.csv", row.names = FALSE)
write.csv(ghLoc, "locTypeGH.csv", row.names = FALSE)
write.csv(ghIns, "inscTypeGH.csv", row.names = FALSE)
write.csv(ghDay, "dayTypeGH.csv", row.names = FALSE)
######################### heatmap (indent type and) ######################### 


all_types = sort(unique(dRriyadh$IncidentTypeName))

heat_df = ghIns %>%
  mutate(
    value = ifelse(p.adj < 0.05, estimate, NA_real_)
  ) %>%
  select(group1, group2, value)

heat_sym = bind_rows(
  heat_df,
  heat_df %>%
    transmute(group1 = group2,
              group2 = group1,
              value = -value)
) %>%
  
  complete(group1 = all_types, group2 = all_types) %>%
  mutate(
    value = ifelse(group1 == group2, 0, value)
  )


heat_sym = heat_sym %>%
  mutate(
    group1 = factor(group1, levels = all_types),
    group2 = factor(group2, levels = all_types)
  )


ggplot(heat_sym, aes(x = group1, y = group2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(
    name = "Mean diff\n(transRT)",
    na.value = "grey90",
    limits = max(abs(heat_sym$value), na.rm = TRUE) * c(-1, 1)
  ) +
  labs(
    title = "Games–Howell Pairwise Differences for Incident Types",
    x = "Incident type (group1)",
    y = "Incident type (group2)"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )




topN = 20

gh_incident_top =  ghIns %>%
  filter(p.adj < 0.05) %>%
  mutate(abs_est = abs(estimate),
         comp = paste(group1, "–", group2)) %>%
  arrange(desc(abs_est)) %>%
  slice_head(n = topN) %>%
  arrange(estimate) %>%
  mutate(comp = fct_inorder(comp))
gh_incident_top$direction <- ifelse(gh_incident_top$estimate > 0,
                                    "Slower",
                                    "Faster")

ggplot(gh_incident_top,
       aes(x = estimate, y = comp, color = direction)) +
  
  geom_vline(xintercept = 0, linetype = "dashed") +
  
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.15) +
  
  geom_point(size = 3) +
  
  scale_color_manual(
    values = c(
      "Slower" = "salmon",   
      "Faster" = "#457B9D"    
    )
  ) +
  
  labs(
    title = paste0("Top ", topN, " Largest Incident-Type Differences (Games–Howell)"),
    x = "Mean difference in transRT (group1 − group2)",
    y = "Incident type pair",
    color = "Direction"
  ) +
  
  theme_minimal(base_size = 10)

######################### Effect size ######################### 


omegaSource = omega_squared(oneWaySource)
omegaDis = omega_squared(oneWayDisp)
omegaVech = omega_squared(oneWayVehi)
omegaLoc = omega_squared(oneWayLoc)
omegaIns = omega_squared(oneWayIns)
omegaDay = omega_squared(oneWayDay)


os = data.frame(Factor="Source",   omega=omegaSource$Omega2,  CI=toString(omegaSource$CI))
od = data.frame(Factor="Dispatch", omega=omegaDis$Omega2,    CI=toString(omegaDis$CI))
ov = data.frame(Factor="Vehicle",  omega=omegaVech$Omega2,    CI=toString(omegaVech$CI))
ol = data.frame(Factor="Location", omega=omegaLoc$Omega2,     CI=toString(omegaLoc$CI))
oi = data.frame(Factor="Incident", omega=omegaIns$Omega2,     CI=toString(omegaIns$CI))
ow = data.frame(Factor="Weekday",  omega=omegaDay$Omega2,     CI=toString(omegaDay$CI))

omega_table = rbind(os, od, ov, ol, oi, ow)
omega_table[order(-omega_table$omega), ]



######################### primary model(MANOVA) ######################### 

FullModel = lm(transRT ~ SourceName + PQADispatchLevel +
                  LocationTypeName + IncidentTypeName ,
                data = dRriyadh)
modelAnov = Anova(FullModel, type = 3, white.adjust = TRUE)
modelCoef = coeftest(FullModel, vcov = vcovHC(FullModel, type = "HC3"))

write.csv(modelCoef, "modelCoef.csv", row.names = TRUE)
write.csv(modelAnov, "modelAnov.csv", row.names = TRUE)

######################### baseline of the model ######################### 

levels(dRriyadh$PQADispatchLevel)
levels(dRriyadh$IncidentTypeName)
levels(dRriyadh$LocationTypeName)
levels(dRriyadh$SourceName)

#the baseline is the first leval show up,
#the baseline of our model => transRTsec = ALPHA + A + Home + Asefne = 7.3826371, which is almost27 min

######################### histogram ######################### 


df = data.frame(
  Incident = c(
    "Traffic Collision - B",
    "Loss of consciousness / Fainting - C",
    "Respiratory Problems - D",
    "Fall - E",
    "Convulsions / Seizures - F",
    "Drowned / Diving accident - FF",
    "Abdominal pain - G",
    "Assault / Sexual Assault / Taser - H",
    "Diabetes Problems - I",
    "Chest pain / Chest tightness (not related to injury) - J",
    "Injuries (specified) - K",
    "Cardiac or Respiratory arrest / Death - N",
    "Burns / Explosion - O",
    "Pregnancy / Childbirth / Miscarriage - U",
    "Stabbing / Gunshot wound / Penetrating injury - Y",
    "Suffocation - Z"
  ),
  Effect = c(-0.0566, -0.0818, -0.0455, 0.06, -0.0757, -0.1339,
             -0.0331, -0.0303, -0.0776, -0.0445, 0.04867,
             -0.1500, -0.2234, -0.0636, -0.0927, -0.1831)
)

df = df %>% arrange(desc(Effect))
df$Incident = factor(df$Incident, levels = df$Incident)

ggplot(df, aes(x = Incident, y = Effect, fill = Effect)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_fill_gradient2(low = "#1a9850", mid = "white", high = "#d73027", midpoint = 0) +
  labs( x = NULL, y = NULL) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.y = element_text(size = 11),
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none"
  )


######################### Clustering  ######################### 


incProfiles = dRriyadh %>%
  group_by(IncidentTypeName) %>%
  summarise(
    mean_transRT   = mean(transRT, na.rm = TRUE),
    median_transRT = median(transRT, na.rm = TRUE),
    sd_transRT     = sd(transRT, na.rm = TRUE),
    iqr_transRT    = IQR(transRT, na.rm = TRUE),
    n_cases        = dplyr::n()
  )

incMat = incProfiles %>%
  as.data.frame()

rownames(incMat) = incMat$IncidentTypeName
incMat$IncidentTypeName = NULL

incScaled <- scale(incMat) # to Standardize variables

eucDist = dist(incScaled, method = "euclidean" )
hcWard = hclust(eucDist, method ="ward.D2" )

nodePar = list(lab.cex = 0.6, pch =c(19,19),cex = 0.5, col = "salmon")

plot(as.dendrogram(hcWard),hang = -1, nodePar=nodePar ,horiz=TRUE , main = "Clustering dendrogram (ward linkage)")

set.seed(1)

fviz_nbclust(
  incScaled,
  FUNcluster = kmeans,
  method = "silhouette",
  linecolor = "salmon"
  
) +
  labs(
    title = "Optimal Number of Clusters (Silhouette Method)",
    x = "Number of clusters (K)",
    y = "Average silhouette width",
  )

set.seed(1)
k = 4

kmInc = kmeans(incScaled, centers = k, nstart = 25)

kmInc$size

kmInc$centers

incProfiles$cluster = factor(kmInc$cluster)
head(incProfiles)

sil= silhouette (kmInc$cluster,eucDist)
fviz_silhouette(sil, palette = c("defulte"), ggtheme = theme_classic())
