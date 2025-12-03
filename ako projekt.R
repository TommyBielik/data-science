# ---- EDA + logit----
library(titanic)
library(tidyverse)

str(titanic_train)
data("titanic_train")
titanic_clean <- titanic_train %>%
  select(- PassengerId, -Name , -Ticket, -Cabin)

# chybajuce hodnoty
summary(titanic_clean)
colSums(is.na(titanic_clean))

#vek podla triedy
age_class <- titanic_clean %>%
  group_by(Pclass)%>%
  summarise(median(Age , na.rm = TRUE))
age_class

#nahradenie chybajucich hodnot vo veku
titanic_clean <- titanic_clean%>%
  mutate(Age = ifelse(is.na(Age)& Pclass == 1 , 37,
                      ifelse(is.na(Age) & Pclass == 2 , 29,
                             ifelse(is.na(Age) & Pclass == 3 , 24, Age))))
summary(titanic_clean)
view(titanic_clean)

#prezivsi vs pohlavie
ggplot(titanic_clean, aes(x = Sex, fill = factor(Survived))) + 
  geom_bar(position = "dodge") + 
  labs(title = "Survived vs Gender")

#prezivsi vs vek
ggplot(titanic_clean, aes(x = Age, fill = factor(Survived))) + 
  geom_histogram(position = "dodge") + 
  labs(title = "Survived vs Age")

# prezivsi vs trieda
ggplot(titanic_clean, aes(x = Pclass, fill = factor(Survived))) + 
  geom_bar(position = "dodge") + 
  labs(title = "Survived vs Class")

# podiely prezivsich  medzi triedami
table(titanic_clean$Pclass , titanic_clean$Survived)
prop.table(table(titanic_clean$Pclass , titanic_clean$Survived) , margin = 1
)

#uprava premennych na faktory
str(titanic_clean)

titanic_clean$Survived <- factor(titanic_clean$Survived)
titanic_clean$Pclass <- factor(titanic_clean$Pclass)
titanic_clean$Sex <- factor(titanic_clean$Sex)
titanic_clean$SibSp <- factor(titanic_clean$SibSp)
titanic_clean$Parch <- factor(titanic_clean$Parch)
titanic_clean$Embarked <- factor(titanic_clean$Embarked)

# logit model
log_model <- glm(formula = Survived ~ ., family = binomial("logit") , 
                 data = titanic_clean)
summary(log_model)

# rozdelenie dat

library(rsample)

set.seed(101)
data_split <- initial_split(titanic_clean, prop = 0.7)
data_split
train_data <- training(data_split)
test_data <- testing(data_split)

#odhad treningoveho modelu
log_model <- glm(formula = Survived ~ Pclass + Sex + Age + SibSp + Fare, family = binomial("logit") ,  
                 data = test_data)
summary(log_model)

#odhad na testovacom modeli
fin_log_model <- glm(formula = Survived ~ Pclass + Sex + Age + SibSp + Fare, family = binomial("logit") , 
       data = test_data)
summary(fin_log_model)
#ulozenie prognozovanych hodnot
fit_probab <- predict(fin_log_model , newdata = test_data)
fit_result <- (fit_probab > 0.5,1,0)

# percentualny podiel spravnych odpovedi
fitted_err <- mean(fit_result = test_data$Survived)
fitted_acc <- 1 - fitted_err

#zostrojenie confusion matrix
table(fit_result , test_data$Survived)

s# PROJEKT:
# HTML a kod HTML
#prezentácia :7 až 10 minút, prezentovanie HTML
#teória, prečo take y a xka
# korelácia zav
# ak su chybajuce hodnoty, tak kluc ako ich doplnime
# akú ma prognosticku schopnost 
# co samotna regresia povedala

