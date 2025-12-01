#Install needed packages if necessary
library(tidyverse)
library(sjPlot)

dat <- read.csv("FinalData.csv")

### Regression Models
v0 <- lm(ViolentRate ~ GraduationRates + PovertyRate + 
           MobilityRate + heterogeneity6, data = dat)
tab_model(v0)

v1 <- lm(ViolentRate ~ CCMR.Grads + PovertyRate + 
           MobilityRate + heterogeneity6, data = dat)
tab_model(v1)

v2 <- lm(ViolentRate ~ CCMR.College + PovertyRate + 
           MobilityRate + heterogeneity6, data = dat)
tab_model(v2)

v3 <- lm(ViolentRate ~ TSI.Both + DualCredit + APBI + Associates + 
           OnRamps + PovertyRate + MobilityRate + heterogeneity6, 
         data = dat)
tab_model(v3)

p0 <- lm(PropertyRate ~ GraduationRates + PovertyRate + 
           MobilityRate + heterogeneity6, data = dat)
tab_model(p0)

p1 <- lm(PropertyRate ~ CCMR.Grads + PovertyRate + 
           MobilityRate + heterogeneity6, data = dat)
tab_model(p1)

p2 <- lm(PropertyRate ~ CCMR.College + PovertyRate + 
           MobilityRate + heterogeneity6, data = dat)
tab_model(p2)

p3 <- lm(PropertyRate ~ TSI.Both + DualCredit + APBI + Associates + 
           OnRamps + PovertyRate + MobilityRate + heterogeneity6, 
         data = dat)
tab_model(p3)

### Example Vizualization
ggplot(dat, aes(x = CCMR.Grads, y = ViolentRate)) + 
  geom_point(shape = 23, size = 2.25, fill = "#05f9ff", col = "#00241B") + 
  geom_smooth(method = "loess", col = "#ed1313") + 
  labs(x = "% of Graduates meeting at least 1 CCMR Criteria", 
       y = "Violent Crime Rates per 1000") + 
  theme(text = element_text(family = "serif", size = 12.5))
# Notice how I use original colors and I specify font and sizes. This is my own
## personal signature for regression plots. You can make your own, and use that
## for your own plots. Ethan can basically use my style, but change the color
## depending on the plot variable. Up to him.


