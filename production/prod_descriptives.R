elsoc_w03$ess

summary(elsoc_w03$ess)

elsoc_w03$inc10h.imp
elsoc_w03$edcine2


library(ggplot2)
library(ggridges)
library(dplyr)

# Example data
set.seed(123)
df <- data.frame(
  group = rep(c("A", "B", "C"), each = 100),
  value = c(rnorm(100), rnorm(100, mean = 2), rnorm(100, mean = 4))
)

# Create density plot by group
load(here::here("input/data/proc/elsoc_w03.RData"))
elsoc_w03$edcine2 <-
  factor(elsoc_w03$edcine2,levels = levels(elsoc_w03$edcine2),
         labels = c("Incomplete Primary or lower","Primary & Lower secondary",
                    "Upper secondary ","Short-cycle tertiary","Tertiary or higher"))


df_desc<- elsoc_w03 %>%
  dplyr::select(ess,inc10h.imp,edcine2,egpisko05,
                sexo,edad,ppolcat,class6,class3) %>% 
  na.omit()




ggplot(elsoc_w03, aes(x = ess, fill =class6)) +
  geom_density(alpha = 0.5,adjust = 2) +
  theme_minimal()

# Opened polygons

ggplot(df_desc, aes(x = ess, y = class6, 
                    group = class6,
                    fill = class6)) + 
  geom_density_ridges(bandwidth = 0.5,alpha= 0.5) + 
  geom_vline(xintercept = mean(df_desc$ess)) + 
  geom_vline(xintercept = median(df_desc$ess),color="red")

ggplot(df_desc, aes(x = ess, y = class3, 
                    group = class3,
                    fill = class3)) + 
  geom_density_ridges(bandwidth = 0.5,alpha= 0.5) + 
  geom_vline(xintercept = mean(df_desc$ess)) + 
  geom_vline(xintercept = median(df_desc$ess),color="red")


ggplot(df_desc, aes(x = ess, y = class6, 
                    group = class6,
                    fill = class6)) + 
  geom_violin(trim = F,alpha= 0.5,draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_jitter(height = 0.1, width = 0.2)
  # geom_boxplot(alpha= 0.5)

  # geom_vline(xintercept = mean(df_desc$ess)) + 
  # geom_vline(xintercept = median(df_desc$ess),color="red")


ggplot(df_desc, aes(x = ess, y = class3, 
                    group = class3,
                    fill = class3)) + 
  geom_violin(trim = F,alpha= 0.5,draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_jitter(height = 0.1, width = 0.2)+
geom_boxplot(alpha= 0.5)

# geom_vline(xintercept = mean(df_desc$ess)) + 
# geom_vline(xintercept = median(df_desc$ess),color="red")




ggplot(df_desc, aes(x = ess, y = egpisko06, 
                    group = egpisko06,
                    fill = egpisko06)) + 
  geom_density_ridges(bandwidth = 0.6,alpha= 0.5)



ggplot(df_desc, aes(x = ess, y = egpisko07, 
                      group = egpisko07,
                      fill = egpisko07)) + 
  geom_density_ridges(bandwidth = 0.6,alpha= 0.5) +
  geom_vline(xintercept = mean(df_desc$ess))


ggplot(df_desc, aes(x = ess, y = (inc10h.imp), 
                    group = factor(inc10h.imp),
                    fill = factor(inc10h.imp))) + 
  geom_density_ridges(bandwidth = 0.6,alpha= 0.5) + 
  geom_vline(xintercept = mean(df_desc$ess))


ggplot(df_desc, aes(x = ess, y = (edcine2), 
                    group = factor(edcine2),
                    fill = factor(edcine2))) + 
  geom_density_ridges(bandwidth = 0.6,alpha= 0.5) +
  geom_vline(xintercept = mean(df_desc$ess))

sjmisc::frq(elsoc_w03$egpisko07)


ess<- lm(ess ~inc10h.imp+edcine2+egpisko06+edad+sexo+ppolcat, data= elsoc_w03)


perc<-lm(lngap_perc ~inc10h.imp+edcine2+egpisko06+edad+sexo+ppolcat, data= elsoc_w03)

texreg::screenreg(list(ess,perc))


sjPlot::plot_model(ess,terms = "egpisko06",type = "pred")
sjPlot::plot_model(ess,terms = "edcine2",type = "pred")

sjPlot::plot_model(ess,terms = "inc10h.imp",type = "pred")


