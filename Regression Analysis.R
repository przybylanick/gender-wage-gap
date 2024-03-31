# Packages
install.packages('readxl')
install.packages('writexl')
install.packages('ggplot2')
install.packages('dplyr')
install.packages('car')
install.packages('stargazer')
install.packages('lmtest')

# Decimals
options(scipen = 999)

# Import data

library(readxl)
PSID_Data <- read_excel("F:/Downloads/PSID Data.xlsx")
View(PSID_Data)

# Create ID variable for obs

PSID_Data$ID <- PSID_Data$intnum68*1000+PSID_Data$pernum68

# Clean data

PSID_Data_clean <- data.frame("ln(RealHrWage)" = PSID_Data$lnrealwg, "ID" = PSID_Data$ID, "Year" = PSID_Data$wave, "Female" = PSID_Data$sex - 1, 
                              "Age" = PSID_Data$age, "White" = PSID_Data$white, "Tenure" = PSID_Data$yrsexp, "YrsSch" = PSID_Data$schupd, 
                              "HighDeg" = PSID_Data$degree, "BachDeg" = PSID_Data$ba, "AdvDeg" = PSID_Data$adv, "Industry" = PSID_Data$ind2, 
                              "Occupation" = PSID_Data$occ2, "Region" = PSID_Data$region)
View(PSID_Data_clean)

# Change Industry and Occupation to string

PSID_Data_clean$Industry <- as.character(PSID_Data_clean$Industry)
PSID_Data_clean$Occupation <- as.character(PSID_Data_clean$Occupation)
PSID_Data_clean$Region <- as.character(PSID_Data_clean$Region)

# Checking region values
PSID_Data_region <- data.frame("Region" = PSID_Data$region, "NorthEast" = PSID_Data$northeast, 
                              "NorthCentral" = PSID_Data$northcentral, "South" = PSID_Data$south, "West" = PSID_Data$west)
View(PSID_Data_region)
# 1 = northeast, 2 = north central, 3 = south, 4 = west

# Save data as xlsx in 145 folder

library(writexl)
write_xlsx(PSID_Data_clean, "C:/Users/przyb/OneDrive/ECON 145/PSID_Data_clean.xlsx")

# Import clean data

library(readxl)
PSID_Data_clean <- read_excel("PSID_Data_clean.xlsx")
View(PSID_Data_clean)

# save data as RData
save(PSID_Data_clean, file = "C:/Users/przyb/OneDrive/ECON 145/PSID_Data_clean.RData")

# load data
load("C:/Users/przyb/OneDrive/ECON 145/cleanDFs.RData")

# Find amount of observations
nrow(PSID_Data_clean$ID)
# [1] 33398

### First regression
PSIDreg <- lm(ln.RealHrWage. ~ Female + Age + White + Tenure, data = PSID_Data_clean)
summary(PSIDreg)

# Test correlation between Age and Tenure
cor(PSID_Data_clean$Age, PSID_Data_clean$Tenure)
# [1] 0.8734386

# Drop Age
PSIDreg2 <- lm(ln.RealHrWage. ~ Female + White + Tenure, data = PSID_Data_clean)
summary(PSIDreg2)

# Replace Age drop Tenure
PSIDreg3 <- lm(ln.RealHrWage. ~ Female + White + Age, data = PSID_Data_clean)
summary(PSIDreg3)

# Regress wage on age to check sign of Age coefficient 
PSIDreg4 <- lm(ln.RealHrWage. ~ Age, data = PSID_Data_clean)
summary(PSIDreg4)

# Plot log wage against age
library(ggplot2)
plot1 <- ggplot(PSID_Data_clean, aes(Age, ln.RealHrWage.)) + geom_point()
plot1

# Age not statistically significant, highly correlated with Tenure

# Finding binned averaged
library(dplyr)
PSIDclean2 <- PSID_Data_clean %>% 
  group_by(Tenure) %>%
  mutate(binnedAvg = mean(ln.RealHrWage.)) %>%
  ungroup()

plot2 <- ggplot(PSIDclean2, aes(Tenure, binnedAvg)) + geom_point()
plot2

# Curvature shows non-linearity between Tenure and Wage, so consider Tenure^2

# Create Tenure-squared variable

PSID_Data_clean$TenureSq <- PSID_Data_clean$Tenure^2

# Add TenureSq to regression

PSIDregTenSq <- lm(ln.RealHrWage. ~ Female + White + Tenure + TenureSq, data = PSID_Data_clean)
summary(PSIDregTenSq)

### Add fixed effects

# Add Industry
PSIDreg5 <- lm(ln.RealHrWage. ~ Female + White + Tenure + TenureSq + Industry, data = PSID_Data_clean)
summary(PSIDreg5)

# Add Occupation 
PSIDreg6 <- lm(ln.RealHrWage. ~ Female + White + Tenure + TenureSq + Industry + Occupation, data = PSID_Data_clean)
summary(PSIDreg6)

# Add BachDeg and AdvDeg
PSIDreg7 <- lm(ln.RealHrWage. ~ Female + White + Tenure + TenureSq + BachDeg + AdvDeg + Industry + Occupation, data = PSID_Data_clean)
summary(PSIDreg7)

# Add Region
PSIDreg8 <- lm(ln.RealHrWage. ~ Female + White + Tenure + TenureSq + BachDeg + AdvDeg + Industry + Occupation + Region, data = PSID_Data_clean)
summary(PSIDreg8)

# Interact Female with White
PSIDreg9 <- lm(ln.RealHrWage. ~ Female + White + Tenure + TenureSq + Female*White + BachDeg + AdvDeg + Industry + Occupation + Region, data = PSID_Data_clean)
summary(PSIDreg9)

#### Add interacting terms (no fixed effects)

PSIDreg12 <- lm(ln.RealHrWage. ~ Female + White + Tenure + TenureSq + BachDeg + AdvDeg + Female*(White + Tenure + TenureSq + BachDeg + AdvDeg), data = PSID_Data_clean)
summary(PSIDreg12)

#### Add back fixed effects

PSIDreg13 <- lm(ln.RealHrWage. ~ Female + White + Tenure + TenureSq + BachDeg + AdvDeg + Female*(White + Tenure + TenureSq + BachDeg + AdvDeg) + Industry + Occupation + Region, data = PSID_Data_clean)
summary(PSIDreg13)

# Create subset to test factor
PSID_Data_factor <- data.frame("ln(RealHrWage)" = PSID_Data$lnrealwg, "ID" = PSID_Data$ID, "Year" = PSID_Data$wave, "Female" = PSID_Data$sex - 1, "Age" = PSID_Data$age, 
                              "White" = PSID_Data$white, "Tenure" = PSID_Data$yrsexp, "YrsSch" = PSID_Data$schupd, "HighDeg" = PSID_Data$degree, 
                              "Industry" = PSID_Data$ind2, "Occupation" = PSID_Data$occ2)
PSID_Data_factor$Industry <- as.factor(PSID_Data_factor$Industry)
PSID_Data_factor$Occupation <- as.factor(PSID_Data_factor$Occupation)
PSID_Data_factor$Year <- as.factor(PSID_Data_factor$Year)

factorReg <- lm(ln.RealHrWage. ~ Year + Female + White + Tenure + Female*White + Industry + Occupation, data = PSID_Data_factor)
summary(factorReg)

# Confirmed same as character

# Desriptive statistics
summary(PSID_Data_clean$ln.RealHrWage.)
summary(PSID_Data_clean$Female)
summary(PSID_Data_clean$White)
summary(PSID_Data_clean$BachDeg)
summary(PSID_Data_clean$AdvDeg)
summary(PSID_Data_clean$Tenure)
summary(PSID_Data_clean$TenureSq)

# Correlation Matrix
cor_df <- data.frame("Female" = PSID_Data$sex - 1, "Age" = PSID_Data$age, "White" = PSID_Data$white, 
                     "Tenure" = PSID_Data$yrsexp, "BachDeg" = PSID_Data$ba, "AdvDeg" = PSID_Data$adv)
cor(cor_df)

# Check for multicollinearity 
MCreg <- lm(ln.RealHrWage. ~ Female + White + Tenure + BachDeg + AdvDeg + Industry + Occupation + Region, data = PSID_Data_clean)
library(car)
vif(MCreg)

# Check for heteroskedasticity
PSID_Data_clean$resi <- NA
PSID_Data_clean$resi <- PSIDreg2$residuals
library(ggplot2)
resiPlot <- ggplot(data = PSID_Data_clean, aes(y = resi, x = Tenure)) + geom_point(col = 'blue') + geom_abline(slope = 0)
resiPlot

# Breusch-Pagan Test for heteroskedasticity 
library(lmtest)
bptest(PSIDreg13)

# Durbin-Watson Test for autocorrelation
library(car)
durbinWatsonTest(PSIDreg13)

# Hausman test
install.packages("plm")
library(plm)
pdata <- pdata.frame(PSID_Data_clean, index = c("ID", "Year"))
femethod <- plm(ln.RealHrWage. ~ Female + White + Tenure + TenureSq + BachDeg + AdvDeg + Female*(White + Tenure + TenureSq + BachDeg + AdvDeg) + Industry + Occupation + Region, data = pdata, model = "within")
remethod <- plm(ln.RealHrWage. ~ Female + White + Tenure + TenureSq + BachDeg + AdvDeg + Female*(White + Tenure + TenureSq + BachDeg + AdvDeg) + Industry + Occupation + Region, data = pdata, model = "random")
phtest(femethod, remethod)

# Regression tables
library(stargazer)
stargazer(PSIDreg)
stargazer(PSIDregTenSq)
stargazer(PSIDreg8)
stargazer(PSIDreg8)
stargazer(PSIDreg12)
stargazer(PSIDreg, PSIDregTenSq, PSIDreg8, PSIDreg12, PSIDreg13)