###-------------------------------------------------------------------------------------
#  Load Coronavirus Covid19 data
###-------------------------------------------------------------------------------------
dfCoronaData <- read.csv("data/covid_19_data.csv")
str(dfCoronaData)
unique(dfCoronaData$Country.Region)


###-------------------------------------------------------------------------------------
#  Understand Covid19 data
###-------------------------------------------------------------------------------------
# Let us extract only regions/countries of interest
regionsOfInterest <- c("Mainland China","US","France","Germany","Spain", 
                       "Italy","UK","Indonesia","Iran","Pakistan","India")
mask <- dfCoronaData$Country.Region %in% regionsOfInterest
dfCovid19 <- dfCoronaData[mask, ]

# Setting date column as Date type
library(lubridate)
dfCovid19$ObservationDate <- lubridate::mdy(dfCovid19$ObservationDate)
dfCovid19$ObservationDate_MonthYear <- format(dfCovid19$ObservationDate, format="%m-%y")

# Create required df for SIR model by dates
library(dplyr)
dfAggregated = dfCovid19 %>% 
  group_by(ObservationDate) %>% 
  summarize(InfectedAll=sum(Confirmed), 
            RecoveredAll=sum(Recovered),
            DeadAll=sum(Deaths))
dfAggregated$RemovedAll <- dfAggregated$DeadAll + dfAggregated$RecoveredAll

# Let us plot overall dfAggregated of Infected, Recovered & Dead
plot(x = dfAggregated$ObservationDate, y = dfAggregated$InfectedAll, 
     xlab = "dfAggregated", ylab = "Cumulative count", col = "red")
points(x = dfAggregated$ObservationDate, y = dfAggregated$RecoveredAll, col = "green")
points(x = dfAggregated$ObservationDate, y = dfAggregated$DeadAll, col = "black")
title(main = "dfAggregated of all infected (red), recovered (green) and dead (black)")

# Let us observe infection, recovery and death rates
# Infection rate
dt <- as.numeric(diff(dfAggregated$ObservationDate))
dI <- diff(dfAggregated$InfectedAll)
dIdt <- dI / dt

# Recovery rate
dR <- diff(dfAggregated$RecoveredAll)
dRdt <- dR / dt

# Death rate
dD <- diff(dfAggregated$DeadAll)
dDdt <- dD / dt

# Plot
plot(dfAggregated$ObservationDate[-1], dIdt, xlab = "Timeline", ylab = "Rate", col = "red")
points(dfAggregated$ObservationDate[-1], dRdt, col="green")
points(dfAggregated$ObservationDate[-1], dDdt, col="black")
title(main = "Timeline of infection (red), and recovery (green) & death rates (black)")

# Create required df for SIR model by Country and dates
dfSIR <- dfCovid19 %>% 
  dplyr::group_by(Country.Region,
                  ObservationDate) %>%
  dplyr::summarise(Infected    = max(Confirmed),
                   Dead        = max(Deaths),
                   Recovered   = max(Recovered))
dfSIR$Removed <- dfSIR$Dead + dfSIR$Recovered

# Plot dfAggregated for each country
library(ggplot2)
ggplot(dfSIR, aes(ObservationDate, log10(Infected), colour = Country.Region)) + 
  geom_line(size = 2, alpha = 0.9) +
  scale_x_date(date_labels = "%b %d", date_breaks = "7 days") + 
  scale_colour_manual(values=c("red", "blue","green","orange","black",
                               "yellow","brown","pink","cyan","magenta","grey")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Plot dfAggregated of countries when cases were less than 1000
mask <- dfSIR$Infected < 1000
dfCasesLessThan1000 <- dfSIR[mask,]
ggplot(dfCasesLessThan1000, aes(ObservationDate, Infected, colour = Country.Region)) +
  labs(title =  "Covid19 Confirmed cases per country") +
  geom_line(size = 2, alpha = 0.9) +
  scale_x_date(date_labels = "%b %d", date_breaks = "7 days") + 
  scale_colour_manual(values=c("red", "blue","green","orange","black",
                               "yellow","brown","pink","cyan","magenta","gray")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Comparing Pakistan with other countries: dfAggregated of first 1000 cases
compCountries <- c("Pakistan","Italy","US","Germany","Spain")
dfPak <- dfSIR[dfSIR$Country.Region %in% compCountries,]
dfPak$Country.Region <- as.character(dfPak$Country.Region)
dfPak$ID <- seq(1, nrow(dfPak),1)  # creating ID column
dfPak <- dfPak[dfPak$Infected < 1000, ]
str(dfPak)
ggplot(dfPak, aes(ID, Infected, colour = Country.Region)) + 
  geom_line(size = 2, alpha = 0.9) +
  scale_colour_manual(values=c("red", "blue","green","black","yellow")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Comparing Pakistan with other countries: Days to first 1000 cases
df1 <- dfCovid19[dfCovid19$Confirmed < 1000, ] %>% 
  dplyr::group_by(Country.Region) %>%
  dplyr::summarise(DaysTo1000Cases = n_distinct(ObservationDate))
ggplot(data = df1, mapping = aes(x = as.factor(Country.Region), y = DaysTo1000Cases)) +
  geom_bar(stat = "identity") +
  labs(x = "Countries")


# Comparing Pakistan with other countries: Days to first 10 deaths
df2 <- dfSIR[dfSIR$Dead < 10, ] %>% 
  dplyr::group_by(Country.Region) %>%
  dplyr::summarise(DaysTo10Deaths = n_distinct(ObservationDate))
ggplot(data = df2, mapping = aes(x = as.factor(Country.Region), y = DaysTo10Deaths)) +
  geom_bar(stat = "identity") +
  labs(x = "Countries")

# Clean memory
rm(list=setdiff(ls(), c("dfCoronaData", "dfCovid19", "dfAggregated", "dfSIR")))


###-------------------------------------------------------------------------------------
#  S.I.R Model for Covid19 Outbreak
###-------------------------------------------------------------------------------------
# Source: https://www.youtube.com/watch?v=NKMHhm2Zbkw
#         https://www.kaggle.com/lisphilar/covid-19-data-with-sir-model#Improvement-of-math-model
# S = Susceptibles
# I = Infectives
# R = Removed (Recovered or Died)
# 
# Following are the assumptions of the SIR Model to remain contant
# 1. Population (N) remains contant (ignoring birth date & death rate of population)
# 2. Infection rate (r) is proportional to contact S & I
# 3. Recovery rate (a) is constant (I to R rate is contant)
#
# SIR Model can be stated as 3 differential equations:
# dS = -rSI 
# dI = +rSI - aI
# dR = +aI          (where dS, dI, dR are rate of change of S, I, R w.r.t to Time t)
#
# Variables of SIR model  = S, I, R
# Parameters of SIR model = N, r, a
#
# Initial condition of SIR Model:
# S = S0
# I = I0
# R = 0
# d(S+I+R) = 0 
# S + I + R = S0 + I0     ---- equation (1)
#
# Now, what questions can be answered? Here are 3 important questions:
# 1. Will the disease spread?
#  -- As, S < S0 
#  -- So, dI < I(r*S0 - a) ==> if S0 > a/r or S0 > 1/q (where say q = r/a) then the disease spreads!
#  -- And let's also say R0 = (r*S0)/a and if R0 > 1 then the disease spreads!
#  -- For COVID19, R0 is estimated more like 3 to 4
#  -- Obviously COVID19 will spread is sadly spreading!
#
# 2. What will be the number of infectives (Imax) at any given time?
#  -- dI/dS = (rsI - aI)/(-rsI) = -1 + a/rS = -1 + 1/(q*S) where Imax can be found when S = 1/q
#  -- ==> I + S - (1/q * ln(S)) = I0 + S0 - (1/q * ln(S0))     ---- equation (2)
#  -- Subsituting S = 1/q gives:    Imax = I0 + S0 - (1/q * (1 + ln(q * S0))) 
#  -- Let us say, f(q) = (1/q * (1 + ln(q * S0))) 
#  -- And finally, Imax = I0 + S0 - f(q)
#
# 3. How many people in total will end up catching the disease?
#  -- Actually we are looking for R at the end (R_end) of pendemic which can be given using equation (1):
#  -- R_end = I0 + S0 - S_end     ---- equation (3) 
#  -- where S_end is unknown & can be found from equation (2) as following
#  -- S_end - (1/q * ln(S_end)) = I0 + S0 - (1/q * ln(S0)) , which we can simply call as g(q)
#  -- Therefore, R_end = I0 + S0 - g(q)
#
# SIR Model for Germany
df <- dfSIR[dfSIR$Country.Region == "Germany",]
N <- 100000 # Assumption of 1 million population
I <- df$Infected - df$Recovered
R <- df$Recovered 
S <- N - I - R

# In order to estimate parameter (r & a) values, we have to fit linear regression models for each
# Fit a linear model to estimate value of r
x <- I * R / N
x <- x[-1]
y <- diff(S) / as.numeric(diff(df$ObservationDate))
lreg1 <- lm(y ~ x + 0)
print(summary(lreg1))
# As p value is very low so we can accept the hypothesis and take coefficient value as value for r
r <- -as.numeric(coef(lreg1))

# Fit a linear model to estimate value of b
y <- diff(R) / as.numeric(diff(df$ObservationDate))
lreg2 <- lm(y ~ x + 0)
print(summary(lreg2))
# As p value is very low so we can accept the hypothesis and take coefficient value as value for b
b <- as.numeric(coef(lreg2))

# Execute SIR
n.sim = 30 # Number of days to prognose
position = length(df$ObservationDate)
S = c(S, rep(0, n.sim))
I = c(I, rep(0, n.sim))
R = c(R, rep(0, n.sim))

# Expand the dates column
dates = c(df$ObservationDate, rep(0, n.sim))

for(m in position+1:n.sim) {
  dS = -r*S[m-1]*I[m-1] / N
  S[m] = max(min(S[m-1] + dS, N), 0)
  
  dR = b*I[m-1]
  R[m] = min(R[m-1] + dR, N)
  
  dI = r*S[m-1]*I[m-1] / N - b*I[m-1]
  I[m] = max(I[m-1] + dI, 0)
  
  dates[m] = dates[m-1] + 1
}

# Collect results from SIR Model
dfResults <- data.frame(
  Dates = dates,
  Susceptibles = S,
  Infecteds = I,
  Removed = R
)

# Plot the results
library(reshape2)
dfMelted <- reshape2::melt(dfResults, id="Dates")
ggplot(data=dfMelted,
       aes(x=Dates, y=value, colour=variable)) +
  geom_line()



