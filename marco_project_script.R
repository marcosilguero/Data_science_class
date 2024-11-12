setwd("C:/Users/ahsil/OneDrive/Desktop/R_class_2024/Marco_project")
getwd()

list.files()
Marco_project <- read.csv("SLN_Feild.csv")
str(Marco_project)
#############################
# Flowering/Seed production, compared to disturbed/undisturbed 

#Site (Disturbed/Undisturbed), Flowering (0/1), Seed_Production (0/1)

t_test <- t.test(Flowering ~ Disturbed_Undisturbed, data = Marco_project)


p_value <- t_test$p.value

library(ggplot2)



ggplot(Marco_project, aes(x = Disturbed_Undisturbed, y = Flowering)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Flowering by Site Type", x = "Site Type", y = "Flowering") +
  annotate("text", x = 1.5, y = max(Marco_project$Flowering) * 0.95, 
           label = paste("p-value =", round(p_value, 4)), size = 5, color = "red") +
  theme_minimal()

#Not the best graph


ggplot(Marco_project, aes(x = Disturbed_Undisturbed, y = Flowering)) +
  geom_violin(fill = "lightblue") +
  labs(title = "Flowering by Site Type", x = "Site Type", y = "Flowering") +
  annotate("text", x = 1.5, y = max(Marco_project$Flowering) * 0.95, 
           label = paste("p-value =", round(p_value, 4)), size = 5, color = "red") +
  theme_minimal()

# Flowering is binary based so these plots aere not the best


ggplot(Marco_project, aes(x = Disturbed_Undisturbed, fill = factor(Flowering))) +
  geom_bar(position = "dodge") +
  labs(title = "Count of Flowering by Site Type", x = "Site Type", y = "Count of Plants",
       fill = "Flowering (0 = No, 1 = Yes)") +
  theme_minimal()

#############################
#
# gsw/dusturbed_Undisturbed
#
############################


t_test <- t.test(gsw ~ Disturbed_Undisturbed, data = Marco_project)
t_test
summary(t_test)
p_value <- t_test$p.value


ggplot(Marco_project, aes(x = Disturbed_Undisturbed, y = gsw)) +
  geom_boxplot(fill = "lightblue") +
  geom_point(color = "darkblue", alpha = 0.6) +
  labs(title = "gsw by Site Type", x = "Site Type", y = "gsw") +
  annotate("text", x = 1.5, y = max(Marco_project$gsw) * 0.95, 
           label = paste("p-value =", round(p_value, 4)), size = 5, color = "red") +
  theme_minimal()
#############################
#
#Soil moisture 
#

t_test <- t.test(Soil.Moisture ~ Insect_damage, data = Marco_project)
t_test
summary(t_test)
p_value <- t_test$p.value

ggplot(Marco_project, aes(x = Insect_damage, y = Soil.Moisture)) +
  geom_boxplot(fill = "lightblue") +
  geom_point(color = "darkblue", alpha = 0.6) +
  labs(title = " Soil moisture by Insect damage", x = "Site Type", y = "Soil Moisture") +
  annotate("text", x = 1.5, y = max(Marco_project$Soil.Moisture) * 0.95, 
           label = paste("p-value =", round(p_value, 4)), size = 5, color = "red") +
  theme_minimal()
#####################################
#
#Soil moisture with flowering and seed production
#
#################################


anova_model <- aov(Flowering ~ Soil.Moisture, data = Marco_project)
summary(anova_model)



ggplot(Marco_project, aes(x = factor(Flowering), y = Soil.Moisture)) +
  geom_boxplot(fill = "lightblue") +
  geom_point(color = "darkblue", alpha = 0.6) +
  labs(title = "Soil Moisture by Flowering", x = "Flowering (0 = No, 1 = Yes)", y = "Soil Moisture") +
  theme_minimal()


anova_model <- aov(Seed.production ~ Soil.Moisture, data = Marco_project)

ggplot(Marco_project, aes(x = factor(Flowering), y = Soil.Moisture)) +
  geom_boxplot(fill = "lightblue") +
  geom_point(color = "darkblue", alpha = 0.6) +
  labs(title = "Soil Moisture by Seed Production", x = "Seeds (0 = No, 1 = Yes)", y = "Soil Moisture") +
  theme_minimal()
##############################
#
#Gsw and soil moisture regression line 

ggplot(Marco_project, aes(x = Soil.Moisture, y = gsw)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "loess", color = "red") +
  labs(title = "Stomatal Conductance (gsw) vs Soil Moisture", x = "Soil Moisture", y = "Stomatal Conductance (gsw)") +
  theme_minimal()
