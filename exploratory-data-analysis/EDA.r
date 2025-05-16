library(ggplot2)
library(dplyr)
library(readr)
library(ggcorrplot)
library(tidyverse)
library(GGally)
library(maps)
library(scales)
library(corrplot)
library(reshape2)
library(usmap)
library(usmapdata)
library(ggridges)

data <- read_csv("data/usa_01.csv")

#======================================================================
# Дескриптивний аналіз

### 1.Статево-вікова піраміда 
## Підготовка даних для статево-вікової піраміди
pyramid_data <- data %>%
 mutate(SEX = factor(SEX, levels = c(1, 2), labels = c("Чоловіки", "Жінки"))) %>%
 group_by(AGE, SEX) %>%
 summarise(count = n()) %>%
 ungroup() %>%
 mutate(count = ifelse(SEX == "Чоловіки", -count, count))


## Побудова статево-вікової піраміди
ggplot(pyramid_data, aes(x = AGE, y = count, fill = SEX)) +
 geom_bar(stat = "identity", width = 0.92) +
 coord_flip() +
 scale_y_continuous(labels = abs, breaks = scales::pretty_breaks(n = 10)) +
 labs(title = "Статево-вікова піраміда",
      x = "Вік",
      y = "Кількість, осіб",
      fill = "Стать") +
 scale_fill_manual(values = c("Чоловіки" = "#08a4bf", "Жінки" = "#de3e8e")) +
 theme(plot.title = element_text(size = 18, face = "bold"),  # Розмір і стиль заголовка
   axis.title = element_text(size = 16),                     # Розмір підписів осей
   axis.text = element_text(size = 14),                      # Розмір тексту підписів осей
   legend.title = element_text(size = 14, face = "bold"),    # Розмір заголовка легенди
   legend.text = element_text(size = 12))                    # Розмір тексту в легенді

ggsave("pics/age_pyramid.png", height = 8, width = 7)



## 2.1 Вусата діаграма доходу
ggplot(data, aes(y = INCTOT)) +
 geom_boxplot(fill = "lightgreen") +
 labs(title = "Розподіл загального доходу", y = "Загальний дохід") +
 theme(plot.title = element_text(size = 18, face = "bold"), 
   axis.title = element_text(size = 16),                     
   axis.text = element_text(size = 14),                     
   legend.title = element_text(size = 14, face = "bold"),    
   legend.text = element_text(size = 12))                    

ggsave("pics/boxPlotINCTOT.png",  height = 7, width = 8)



## 2.2 Прологарифмуємо пункт 2.1
ggplot(data, aes(y = log(INCTOT))) +
 geom_boxplot(fill = "#6ca89b") +
 labs(title = "Розподіл загального доходу", y = "Логарифм доходу") +
 theme(plot.title = element_text(size = 18, face = "bold"),  
   axis.title = element_text(size = 16),        
   axis.text = element_text(size = 14),                   
   legend.title = element_text(size = 14, face = "bold"),    
   legend.text = element_text(size = 12))                    

ggsave("pics/boxPlotLogINCTOT.png",  height = 7, width = 8)



# 3. Мапа США розподілу кількості людей у кожному штаті  
state_codes <- data.frame(
  STATEFIP = c(01, 02, 04, 05, 06, 08, 09, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56),
  state_name = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")
)

state_avg_inctot <- data %>%
  group_by(STATEFIP) %>%
  summarise(count_people = n()) %>%
  inner_join(state_codes, by = "STATEFIP") %>%
  mutate(fips = sprintf("%02d", STATEFIP))

# Отримуємо центроїди штатів
centroid_labels <- usmapdata::centroid_labels("states")

# Об'єднуємо дані про середній вік із центроїдами штатів
data_labels <- merge(centroid_labels, state_avg_inctot, by.x = "fips", by.y = "fips")

# Побудова карти США з кольоровим відображенням середнього віку
plot_usmap(data = state_avg_inctot, values = "count_people", regions = "states", include = state_avg_inctot$fips) +
  scale_fill_continuous(low = "#2EB8D1", high = "#992ED1", 
                        name = "Кількість людей", na.value = "grey50",
                        label = scales::comma) +
  labs(title = "Кількість людей у кожному штаті США", x = "", y = "") +
  
  # Додаємо підписи штатів
  geom_sf_text(data = data_labels, aes(label = abbr), color = "#000000", size = 4) +
  theme(legend.position = "right",
        plot.title = element_text(size = 30),
        axis.title = element_text(size = 25),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18))
ggsave("pics/mapUSA_countPeople.png", height = 9, width = 15)             



## 4.bar plot зайнятості
worker_status <- c(
  "1 - Працевлаштований",
  "2 - Безробітний",
  "3 - Поза робочою силою"
)

# Стовпчикова діаграма про розподіл зайнятості
ggplot(data, aes(x = factor(EMPSTAT), fill = factor(EMPSTAT))) +
  geom_bar() +
  scale_fill_discrete(labels = factor(worker_status)) +
  labs(title = "Розподіл зайнятості",
       x = "Статус зайнятості",
       y = "Кількість людей",
       fill = "Статус зайнятості") +
  theme(plot.title = element_text(size = 30),
        axis.title = element_text(size = 25),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 20))

ggsave("pics/barPlotEMPSTAT.png", height = 8, width = 15)        



## 5.Гістограма тижневої кількості відпрацьованих годин
data_f <- data %>%
 mutate(WKSWORK2 = factor(WKSWORK2, 
                          levels = c(1, 2, 3, 4, 5, 6), 
                          labels = c("1-13", "14-26", "27-39", "40-47", "48-49", "50-52")))

p <- ggplot(data_f, aes(x = factor(WKSWORK2))) +  # Перетворюємо WKSWORK2 у факторну змінну
 geom_bar(fill = "purple", color = "black") +
 labs(title = "Кількість відпрацьованих тижнів рік", 
      x = "Тижні", 
      y = "Кількість, осіб") +
 theme(
   plot.title = element_text(size = 18, face = "bold"),   
   axis.title = element_text(size = 14),                
   axis.text = element_text(size = 10),                 
   legend.title = element_text(size = 14, face = "bold"), 
   legend.text = element_text(size = 12))                

ggsave("pics/barPlotWKSWORK2.png", plot = p, height = 6, width = 9)



## 6.1 Вусата діаграма доходу за класом працівника
data_f <- data %>%
 mutate(CLASSWKR = factor(CLASSWKR, 
                          levels = c(1, 2), 
                          labels = c("Самозайняті", "Працюють за заробітну плату")))

ggplot(data_f, aes(x = factor(CLASSWKR), y = INCWAGE)) +
 geom_boxplot(fill = "orange") +
 labs(title = "Розподіл доходу за класом працівника", x = "Клас працівника", y = "Загальний дохід") +
 theme(plot.title = element_text(size = 18, face = "bold"),  
    axis.title = element_text(size = 14),                  
    axis.text = element_text(size = 10),                   
    legend.title = element_text(size = 14, face = "bold"), 
    legend.text = element_text(size = 12))                 

ggsave("pics/boxPlotCLASSWKR_INCWAGE.png", height = 6, width = 9)



### 6.2 Прологарифмуємо пункт 6.1
data_f <- data %>% 
  filter(INCWAGE > 0) %>%
  mutate(CLASSWKR = factor(CLASSWKR, 
                           levels = c(1, 2), 
                           labels = c("Самозайняті", "Працюють за заробітну плату")))

ggplot(data_f, aes(x = factor(CLASSWKR), y = log(INCWAGE))) +
  geom_boxplot(fill = "orange") +
  labs(title = "Розподіл доходу за класом працівника", x = "Клас працівника", y = "Загальний дохід") +
  theme(plot.title = element_text(size = 18, face = "bold"),   
     axis.title = element_text(size = 14),                     
     axis.text = element_text(size = 10),                     
     legend.title = element_text(size = 14, face = "bold"),   
     legend.text = element_text(size = 12))                

ggsave("pics/boxPlotCLASSWKR_LogINCWAGE.png", height = 6, width = 7)



#=======================================================================
# Розвідковий аналіз даних (EDA)

#=======================================================================
# Питанння №1
# Як рівень освіти людини (EDUC, EDUCD) впливає на її річний дохід (INCTOT)?
#
# Стовпчикова діаграма середнього доходу за рівнем освіти
# Рисунок №1

education_labels <- c(
  "0 - Немає освіти або N/A",
  "1 - Дитячий садок до 4 класу",
  "2 - 5–8 класи",
  "3 - 9 клас",
  "4 - 10 клас",
  "5 - 11 клас",
  "6 - 12 клас",
  "7 - 1 рік коледжу",
  "8 - 2 роки коледжу",
  "10 - 4 роки коледжу",
  "11 - 5+ років коледжу"
)

# Стовпчикова діаграма середнього доходу за рівнем освіти
data_summary <- data %>%
  group_by(EDUC) %>%
  summarise(Mean_Income = mean(INCTOT, na.rm = TRUE))


ggplot(data_summary, aes(x = factor(EDUC), y = Mean_Income, fill = factor(EDUC))) +
  geom_col(alpha = 0.7) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_discrete( labels = factor(education_labels)) +
  labs(title = "Середній річний дохід за рівнем освіти",
       x = "Рівень освіти",
       y = "Середній річний дохід",
       fill = "Рівень освіти") +
  theme(plot.title = element_text(size = 30),
        axis.title = element_text(size = 25),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 20))

ggsave("pics/barPlotMeanINCTOT_EDUC.png", height = 8, width = 15)  



#=========================================================================
# Мапа середніх доходів США
# Рисунок №2

state_codes <- data.frame(
  STATEFIP = c(01, 02, 04, 05, 06, 08, 09, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56),
  state_name = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")
)

state_avg_inctot <- data %>%
  group_by(STATEFIP) %>%
  summarise(mean_inctot = mean(INCTOT, na.rm = TRUE)) %>%
  inner_join(state_codes, by = "STATEFIP") %>%
  mutate(fips = sprintf("%02d", STATEFIP))

# Отримуємо центроїди штатів
centroid_labels <- usmapdata::centroid_labels("states")

# Об'єднуємо дані про середній вік із центроїдами штатів
data_labels <- merge(centroid_labels, state_avg_inctot, by.x = "fips", by.y = "fips")

# Побудова карти США з кольоровим відображенням середнього віку
plot_usmap(data = state_avg_inctot, values = "mean_inctot", regions = "states", include = state_avg_inctot$fips) +
  scale_fill_continuous(low = "#2649D9", high = "#D92649", 
                        name = "Середній\nдохід", na.value = "grey50",
                        label = scales::comma) +
  labs(title = "Середній дохід кожного штату США", x = "", y = "") +
  
  # Додаємо підписи штатів
  geom_sf_text(data = data_labels, aes(label = abbr), color = "#ffffff", size = 4) +
  theme(legend.position = "right",
        plot.title = element_text(size = 30),
        axis.title = element_text(size = 25),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18))

ggsave("pics/mapUSA_INCTOT.png", height = 9, width = 15)    



#=======================================================================
# Мапа середнього рівня освіти
# Рисунок №3

state_codes <- data.frame(
  STATEFIP = c(01, 02, 04, 05, 06, 08, 09, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56),
  state_name = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")
)

state_avg_educ <- data %>%
  group_by(STATEFIP) %>%
  summarise(mean_educ = mean(EDUC, na.rm = TRUE)) %>%
  inner_join(state_codes, by = "STATEFIP") %>%
  mutate(fips = sprintf("%02d", STATEFIP))

# Отримуємо центроїди штатів
centroid_labels <- usmapdata::centroid_labels("states")

# Об'єднуємо дані про cередній рівень освіти із центроїдами штатів
data_labels <- merge(centroid_labels, state_avg_age, by.x = "fips", by.y = "fips")

# Побудова карти США з кольоровим відображенням середнього рівня освіти
plot_usmap(data = state_avg_educ, values = "mean_educ", regions = "states", include = state_avg_educ$fips) +
  scale_fill_continuous(low = "#2649D9", high = "#D92649", 
                        name = "Середній\nрівень\nосвіти", na.value = "grey50") +
  
  labs(title = "Середній рівень освіти кожного штату США", x = "", y = "") +
  
  # Додаємо підписи штатів
  geom_sf_text(data = data_labels, aes(label = abbr), color = "white", size = 4)+
  theme(legend.position = "right",
        plot.title = element_text(size = 30),
        axis.title = element_text(size = 25),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18))
ggsave("pics/mapUSA_EDUC.png", height = 9, width = 15)





#=======================================================================
# Діаграма росіювання
# Рисунок №4

education_labels <- c(
  "0 - Немає освіти або N/A",
  "1 - Дитячий садок до 4 класу",
  "2 - 5–8 класи",
  "3 - 9 клас",
  "4 - 10 клас",
  "5 - 11 клас",
  "6 - 12 клас",
  "7 - 1 рік коледжу",
  "8 - 2 роки коледжу",
  "10 - 4 роки коледжу",
  "11 - 5+ років коледжу"
)


ggplot(data, aes(x = factor(EDUC), y = INCTOT, color = factor(EDUC))) +
  geom_jitter(alpha = 0.5) +  
  scale_y_continuous(labels = scales::comma) +
  scale_color_discrete(labels = education_labels) +  
  labs(title = "Розподіл доходу за рівнем освіти",
       x = "Рівень освіти",
       y = "Річний дохід",
       color = "Рівень освіти") +
  theme(plot.title = element_text(size = 30),
        axis.title = element_text(size = 25),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 20))

ggsave("pics/pointPlotINCTOT_EDUC.png", height = 8, width = 15)  



#=======================================================================
# Середній борг (USD) за рівнем освіти (Cтовпчиковою діаграмою)
# Рисунок №5

debt_data <- data %>%
  filter(INCTOT < 0) %>%  # Відбираємо тільки борги
  group_by(EDUC) %>%
  summarise(Mean_Debt = mean(INCTOT, na.rm = TRUE))  # Обчислюємо се

education_labels_debt <- c(
  "0 - Немає освіти або N/A",
  "2 - 5–8 класи",
  "3 - 9 клас",
  "4 - 10 клас",
  "5 - 11 клас",
  "6 - 12 клас",
  "7 - 1 рік коледжу",
  "8 - 2 роки коледжу",
  "10 - 4 роки коледжу",
  "11 - 5+ років коледжу"
)

# середній борг (USD) за рівнем освіти (Cтовпчиковою діаграмою)
ggplot(debt_data, aes(x = factor(EDUC), y = abs(Mean_Debt), fill = factor(EDUC))) +
  geom_col(alpha = 0.8, show.legend = TRUE) +  
  scale_y_continuous(labels = scales::comma) +  
  scale_fill_discrete(labels = education_labels_debt) +  
  labs(title = "Середній борг за рівнем освіти",
       x = "Рівень освіти",
       y = "Середній борг (USD)",
       fill = "Рівень освіти") +
  theme_minimal() +
  theme(plot.title = element_text(size = 30),
        axis.title = element_text(size = 25),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 20))

ggsave("pics/barPlotDebt_EDUC.png", height = 8, width = 15)      

#===================================
#===================================
#===================================
# Питання №2
# Чи залежить зайнятість (EMPSTAT, EMPSTATD) від віку (AGE)?
#
# Стовпчикова діаграма про розподіл зайнятості
# Рисунок №1

worker_status <- c(
  "1 - Працевлаштований",
  "2 - Безробітний",
  "3 - Поза робочою силою"
)

# Стовпчикова діаграма про розподіл зайнятості
ggplot(data, aes(x = factor(EMPSTAT), fill = factor(EMPSTAT))) +
  geom_bar() +
  scale_fill_discrete(labels = factor(worker_status)) +
  labs(title = "Розподіл зайнятості",
       x = "Статус зайнятості",
       y = "Кількість людей",
       fill = "Статус зайнятості") +
  theme(plot.title = element_text(size = 30),
        axis.title = element_text(size = 25),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 20))

ggsave("pics/barPlotEMPSTAT.png", height = 8, width = 15)



#================================================================
# Діаграма площ зайність за віком
# Рисунок №2

employment_data <- data %>%
  group_by(AGE) %>%
  summarise(Employment_Rate = mean(EMPSTAT == 1, na.rm = TRUE))

# Стовпчикова діаграма зайність за віком
ggplot(employment_data, aes(x = AGE, y = Employment_Rate, fill = Employment_Rate)) +
  geom_area( fill="#69b3a2", alpha=0.4) +
  geom_line(color="#69b3a2", linewidth=2) + 
  scale_fill_gradient(low = "lightblue", high = "blue") +  
  labs(title = "Зайнятість за віком",
       x = "Вік",
       y = "Частка зайнятих") +
  theme(plot.margin = margin(, 1, , , "cm"),
        plot.title = element_text(size = 30),
        axis.title = element_text(size = 25),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 20))

ggsave("pics/areaPlotEMPSTAT_AGE.png", height = 8, width = 16)        



#=================================================================
# Стовпчикова діаграма зайнятості за віковими групами
# Рисунок №3

data_f <- data %>% 
    mutate(Age_Group = cut(AGE, breaks = c(seq(15, 80, 5), 100), include.lowest = TRUE, right = FALSE))

employment_by_age_group <- data_f %>%
  group_by(Age_Group) %>%
  summarise(Employment_Rate = mean(EMPSTAT == 1, na.rm = TRUE))

# Стовпчикова діаграма зайнятості за віковими групами
ggplot(employment_by_age_group, aes(x = Age_Group, y = Employment_Rate, fill = Age_Group)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Рівень зайнятості за віковими групами",       
       x = "Вікова група",
       y = "Частка зайнятих") +  
  theme(plot.title = element_text(size = 30),
        axis.title = element_text(size = 25),        
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 25),        
        legend.text = element_text(size = 20))

ggsave("pics/barPlotEMPSTAT_AGE_Group.png", height = 9, width = 15)             


#===================================
#===================================
#===================================
# Питання №3
# Як клас працівника (CLASSWKR, CLASSWKRD) впливає на кількість відпрацьованих тижнів за рік (WKSWORK2)?
#
# Стовпчикова діаграма Розподіл тижнів роботи
# Рисунок №1


work_distribution <- data %>%
  filter(CLASSWKR %in% c(1, 2)) %>%  
  group_by(CLASSWKR, WKSWORK2) %>%
  summarise(Count = n())

working_weeks <- c(
  "1 - 1-13 тижнів",
  "2 - 14-26 тижнів",
  "3 - 27-39 тижнів",
  "4 - 40-47 тижнів",
  "5 - 48-49 тижнів",
  "6 - 50-52 тижнів",
  "NA - Відсутнє значення"
)

# Стовпчикова діаграма Розподіл тижнів роботи
ggplot(work_distribution, aes(x = factor(WKSWORK2), y = Count, fill = factor(WKSWORK2))) +
  geom_col(alpha = 0.8) +
  facet_wrap(~CLASSWKR, labeller = as_labeller(c("1" = "Самозайнятий", "2" = "Найманий працівник"))) +
  scale_fill_discrete(labels = working_weeks) + 
  labs(title = "Розподіл тижнів роботи (WKSWORK2) за класом працівника",
       x = "Тижні роботи (категорії)",
       y = "Кількість працівників",
       fill = "Тижні роботи") +
  theme(plot.title = element_text(size = 30),
        axis.title = element_text(size = 25),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 20),
        strip.text = element_text(size = 25, face = "bold"),
        strip.background = element_rect(fill = "lightgrey", color = NA))

ggsave("pics/barPlotWKSWORK2.png", height = 8, width = 16)       



#================================================================
# Стовпчикова діаграма Розподіл тижнів роботи(Відсоток працівників)
# Рисунок №2


work_distribution <- data %>%
  filter(CLASSWKR %in% c(1, 2)) %>% 
  group_by(CLASSWKR, WKSWORK2) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Стовпчикова діаграма Розподіл тижнів роботи(Відсоток працівників)
ggplot(work_distribution, aes(x = factor(WKSWORK2), y = Percentage, fill = factor(WKSWORK2))) +
  geom_col(alpha = 0.8) +
  facet_wrap(~CLASSWKR, labeller = as_labeller(c("1" = "Самозайнятий", "2" = "Найманий працівник"))) +
  scale_fill_discrete(labels = working_weeks) + 
  labs(title = "Розподіл тижнів роботи (WKSWORK2) за класом працівника",
       x = "Тижні роботи (категорії)",
       y = "Відсоток працівників",
       fill = "Тижні роботи") +
  theme(plot.title = element_text(size = 30),
        axis.title = element_text(size = 25),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 20),
        strip.text = element_text(size = 25, face = "bold"),
        strip.background = element_rect(fill = "lightgrey"))

ggsave("pics/barPlotWKSWORK2_Perc.png", height = 8, width = 16)   



#===================================
#===================================
#===================================
# Питання №4
# Чи впливає наявність дітей у сім’ї (NCHILD) на рівень доходу (INCTOT, INCWAGE)?
#
# Cтовпчикова діаграма залежності середнього доходу від кількості дітей
# Рисунок №1

# Обчислення середнього доходу за кількістю дітей
income_mean <- data %>%
  group_by(NCHILD) %>%
  summarise(mean_income = mean(INCTOT, na.rm = TRUE))

# Побудова lollipop chart
ggplot(income_mean, aes(x = NCHILD, y = mean_income)) + 
  geom_segment(aes(x = NCHILD, xend = NCHILD, y = 0, yend = mean_income), 
               color = "#182c45", linewidth = 1.2) +
  geom_point(color = "#3b7ccc", size = 5) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) + 
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  labs(title = "Залежність середнього доходу від кількості дітей",
       x = "Кількість дітей",
       y = "Середній дохід") +
  theme_minimal() +
  theme(plot.title = element_text(size = 30),
        axis.title = element_text(size = 25),
        axis.text = element_text(size = 20))

# Збереження графіка
ggsave("pics/lollipopNCHILD_meanINCTOT.png", width = 12, height = 8)



#===================================
#===================================
#===================================
# Питання №5
# Як расова приналежність (RACE, RACED) корелює з рівнем зайнятості (EMPSTAT, EMPSTATD)?
#
# Cтовпчикова діаграма на якій буде зображено розподіл рас при різних рівнях зайнятості
# Рисунок №1

people_f <- data %>% 
  mutate(
    RACE = case_when(
      RACE == 1 ~ "White",
      RACE == 2 ~ "Black",
      RACE == 4 ~ "Asian",
      RACE == 5 ~ "Asian",
      RACE == 6 ~ "Asian",
      TRUE ~ "Other"
    ),
    EMPSTAT = factor(EMPSTAT, levels = c(1, 2, 3), labels = c("Працевлаштований", "Не працевлаштований", "Поза робочою силою"))
  )

ggplot(people_f, aes(x = RACE, fill = EMPSTAT)) +
  geom_bar(position = "dodge") +
  labs(title = "Кореляція раси та рівня зайнятості",
       x = "Раса",
       y = "Кількість людей",
       fill = "Статус зайнятості") +
  theme_minimal() + 
  theme(plot.title = element_text(size = 30),
        axis.title = element_text(size = 25),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18))
ggsave("pics/barPlot_RACE_EMPSTAT.png", width = 14, height = 8)



# Ridgeline графік розподілу доходу за расовими групами
# Рисунок №2


people_f <- data %>% 
  mutate(
    RACE = case_when(
      RACE == 1 ~ "White",
      RACE == 2 ~ "Black",
      RACE == 4 ~ "Asian",
      RACE == 5 ~ "Asian",
      RACE == 6 ~ "Asian",
      TRUE ~ "Other"
    ),
    EMPSTAT = factor(EMPSTAT, levels = c(1, 2, 3), 
                     labels = c("Працевлаштований", "Не працевлаштований", "Поза робочою силою"))
  ) %>%  
  filter(INCTOT > 0)

# Створюємо ridgeline графік
ggplot(people_f, aes(x = log(INCTOT), y = RACE, fill = RACE)) +
  geom_density_ridges(scale = 1.2, alpha = 0.7, linewidth = 1) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "Розподіл загального доходу (INCTOT) за расовими групами",
    x = "Логарифм доходу",
    y = "Расова група",
    fill = "Раса"
  ) +
  theme_minimal()  +
  theme(plot.title = element_text(size = 30),
        axis.title = element_text(size = 25),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18))

ggsave("pics/ridgelineRACE_INCTOT.png", width = 14, height = 8)



# Cтовпчикова діаграма на якій буде зображено розподіл рас при різних рівнях зайнятості (Відсоток зайнятих)
# Рисунок №3

people_f <- data %>% 
  mutate(
    RACE = case_when(
      RACE == 1 ~ "White",
      RACE == 2 ~ "Black",
      RACE == 4 ~ "Asian",
      RACE == 5 ~ "Asian",
      RACE == 6 ~ "Asian",
      TRUE ~ "Other"
    ),
    EMPSTAT = factor(EMPSTAT, levels = c(1, 2, 3), labels = c("Працевлаштований", "Не працевлаштований", "Поза робочою силою"))
  )


ggplot(people_f, aes(x = RACE, fill = EMPSTAT)) +
  geom_bar(position = "fill") +
  labs(title = "Кореляція раси та рівня зайнятості",
       x = "Раса",
       y = "Відсоток зайнятих",
       fill = "Статус зайнятості") +
  theme_minimal() + 
  theme(plot.title = element_text(size = 30),
        axis.title = element_text(size = 25),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18))

ggsave("pics/barPlotRACE_EMPSTAT_Perc.png", width = 14, height = 8)



#===================================
#===================================
#===================================
# Питання №6
# Чи впливає рівень освіти (EDUC, EDUCD) на тип зайнятості (CLASSWKR, CLASSWKRD)?
#
# Cтовпчикова діаграма розподілу типу зайнятості та рівня освіти
# Рисунок №1


education_labels <- c(
  "Немає освіти",
  "Дит. садок до 4 класу",
  "5–8 класи",
  "9 клас",
  "10 клас",
  "11 клас",
  "12 клас",
  "1 рік коледжу",
  "2 роки коледжу",
  "4 роки коледжу",
  "5+ років коледжу"
)

people_f <- data %>%
  mutate(
    CLASSWKR = factor(CLASSWKR, levels = c(0, 1, 2), labels = c("NA", "Самозайнятий", "Найманий\nпрацівник")),
    EDUC = factor(EDUC, levels = c(0:8, 10:11), labels = education_labels)
  )

ggplot(people_f, aes(x = EDUC, fill = CLASSWKR)) +
  geom_bar(position = "dodge") +
  labs(title = "Рівень освіти та тип зайнятості",
       x = "Рівень освіти",
       y = "Кількість",
       fill = "Тип роботи") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 30),
        axis.title = element_text(size = 25),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18))

ggsave("pics/barPlotCLASSWKR_EDUC.png", width = 14, height = 8)



#======================================================================================
# Cтовпчикова діаграма розподілу типу зайнятості та рівня освіти (Відсоток зайнятих)
# Рисунок №2

education_labels <- c(
  "Немає освіти",
  "Дит. садок до 4 класу",
  "5–8 класи",
  "9 клас",
  "10 клас",
  "11 клас",
  "12 клас",
  "1 рік коледжу",
  "2 роки коледжу",
  "4 роки коледжу",
  "5+ років коледжу"
)

people_f <- data %>%
  mutate(
    CLASSWKR = factor(CLASSWKR, levels = c(0, 1, 2), labels = c("NA", "Самозайнятий", "Найманий\nпрацівник")),
    EDUC = factor(EDUC, levels = c(0:8, 10:11), labels = education_labels)) %>%
  count(EDUC, CLASSWKR) %>% 
  group_by(EDUC) %>% 
  mutate(perc = n / sum(n))  # Обчислюємо частку в межах кожного рівня освіти
  
# Побудова відсоткового стовпчикового графіка
p6_percent <- ggplot(people_f, aes(x = EDUC, y = perc, fill = CLASSWKR)) +
  geom_col(position = "dodge") +  # Використовуємо 'geom_col', оскільки вже маємо підраховані значення
  scale_y_continuous(labels = scales::percent_format()) +  # Відображення у відсотках
  labs(title = "Рівень освіти та тип зайнятості",
       x = "Рівень освіти",
       y = "Відсоток",
       fill = "Тип роботи") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 30),
        axis.title = element_text(size = 25),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18))

# Збереження графіка
ggsave("pics/barPlotCLASSWKR_EDUC_Perc.png", width = 14, height = 8)




#===================================
#===================================
#===================================
# Питання №7
# Чи є залежність між віком (AGE) та рівнем доходу (INCTOT, INCWAGE)?
#
# Діаграма розсіювання
# Рисунок №1


ggplot(data, aes(x = AGE, y = INCTOT, shape = as.factor(SEX), color = as.factor(SEX))) +
  geom_point(alpha = 0.5) +
  scale_shape_manual(values = c(16, 17), labels = c("Жінка", "Чоловік")) +
  scale_size_manual(8) + 
  scale_color_manual(values = c("green", "blue"), labels = c("Жінка", "Чоловік")) +
  labs(title = "Залежність між віком та рівнем доходу",
       x = "Вік",
       y = "Загальний дохід",
       shape = "Стать",
       color = "Стать") + 
  guides(colour = guide_legend(override.aes = list(size=10))) + 
  theme(legend.key.size = unit(2, 'cm'),
        plot.title = element_text(size = 30),
        axis.title = element_text(size = 25),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18))

ggsave("pics/pointPlotAGE_INCTOT_SEX.png", width = 14, height = 8)



#======================================================================================
# Графік який залежить від середнього доходу від віку та статі особи
# Рисунок №2


data_f <- data %>%
  group_by(SEX, AGE)  %>%
  summarise(mean_income = mean(INCTOT, na.rm = TRUE))

ggplot(data_f, aes(x = AGE, y = mean_income, color = as.factor(SEX))) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_color_manual(values = c("green", "blue"), labels = c("Жінка", "Чоловік")) +
  labs(title = "Залежність середнього доходу від віку та статі",
       x = "Вік",
       y = "Середній дохід",
       color = "Стать") +
  theme(plot.title = element_text(size = 30),
        axis.title = element_text(size = 25),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18))
  
ggsave("pics/plotAGE_INCTOT_SEX.png", width = 14, height = 8)



#=============================================================================
# Мапа США розподілу віку
# Рисунок №3


state_codes <- data.frame(
  STATEFIP = c(01, 02, 04, 05, 06, 08, 09, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56),
  state_name = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")
)

state_avg_age <- data %>%
  group_by(STATEFIP) %>%
  summarise(mean_age = mean(AGE, na.rm = TRUE)) %>%
  inner_join(state_codes, by = "STATEFIP") %>%
  mutate(fips = sprintf("%02d", STATEFIP))

# Отримуємо центроїди штатів
centroid_labels <- usmapdata::centroid_labels("states")

# Об'єднуємо дані про середній вік із центроїдами штатів
data_labels <- merge(centroid_labels, state_avg_age, by.x = "fips", by.y = "fips")

# Побудова карти США з кольоровим відображенням середнього віку
plot_usmap(data = state_avg_age, values = "mean_age", regions = "states", include = state_avg_age$fips) +
  scale_fill_continuous(low = "black", high = "#67c983", 
                        name = "Середній вік", na.value = "grey50") +
  labs(title = "Середній вік кожного штату США", x = "", y = "") +
  
  # Додаємо підписи штатів
  geom_sf_text(data = data_labels, aes(label = abbr), color = "white", size = 4) +
  theme(legend.position = "right",
        plot.title = element_text(size = 30),
        axis.title = element_text(size = 25),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18))

ggsave("pics/mapUSA_AGE.png", height = 9, width = 15) 



#===================================
#===================================
#===================================
# Питання №8
# Як шлюбний статус (MARST) впливає на рівень доходу (INCTOT, INCWAGE)?
#
# ...
# Рисунок №1




#===================================
#===================================
#===================================
# Питання №9
# Чи впливає вид зайнятості (OCC) на загальний дохід (INCTOT, INCWAGE)?
#
# ...
# Рисунок №1


#===================================
#===================================
#===================================
# Питання №10
# Від чого залежить наявність медичної страховки?
#
# ...
# Рисунок №1