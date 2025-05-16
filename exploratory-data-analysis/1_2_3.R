library(ggplot2)
library(dplyr)
library(ggcorrplot)
library(tidyverse)
library(GGally)
library(qqplotr)

# Завантаження даних
data <- read_csv("data/usa_01.csv")

# Перевірка наявності змінних
str(data)

#=================================
#Питання 1
#Як рівень освіти людини (EDUC, EDUCD) впливає на її річний дохід (INCTOT, INCWAGE)?

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

# Cтовпчиковою діаграмою
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

# Висновок
# Люди з вищою освітою (4+ років коледжу) мають значно вищий середній річний дохід порівняно з тими, хто не має освіти або має лише середню освіту.
# Це підтверджується стовпчиковою діаграмою, яка демонструє поступове зростання середнього доходу разом із підвищенням рівня освіти.
#
# Особливо помітно, що після завершення середньої школи (12 клас) дохід починає суттєво зростати, а особливо — для тих, хто здобув вищу освіту (4+ років коледжу).
# Найбільший дохід мають люди, які здобули 5+ років коледжної освіти.
#==================================================

# Діаграма росіювання
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
# Висновок
# Люди з вищою освітою (4+ років коледжу) мають значно вищий середній річний дохід порівняно з тими, хто не має освіти або має лише середню освіту.
# Це підтверджується стовпчиковою діаграмою, яка демонструє поступове зростання середнього доходу разом із підвищенням рівня освіти.
#
# Діаграма розсіювання показує, що хоча середній дохід зростає з рівнем освіти, все ж є значна варіативність у доходах серед людей із однаковим рівнем освіти.
# Це означає, що крім освіти, на дохід впливають і інші фактори, такі як професія, досвід роботи, регіон проживання, індустрія тощо.'
#
# Низькі доходи у людей без освіти або з початковим рівнем освіти
# Категорії "немає освіти" та "дитячий садок – 4 клас" мають найнижчі доходи, що може свідчити про обмежені можливості на ринку праці.

#=============================================================================================


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

# Цей графік показує середній борг (USD) за рівнем освіти
# ВИСНОВОК
# Борг не зростає лінійно з рівнем освіти
# На відміну від доходу, борг має нерівномірний розподіл. Деякі категорії з нижчим рівнем освіти мають відносно високий середній борг.
#
# Найвищий середній борг мають люди з 9 класами освіти
# Це може свідчити про те, що у цій категорії є люди, які взяли кредити, але не продовжили навчання або не змогли знайти високооплачувану роботу.
#
# Люди з вищою освітою мають відносно середній рівень боргу
# Хоча багато людей з 4+ роками коледжу можуть брати освітні кредити, їх середній борг не найвищий. Це може свідчити про те, що з часом вони встигають погасити частину кредитів або мають доступ до інших джерел фінансування.
#
# Люди з низьким рівнем освіти також мають значний середній борг
# Це може бути пов’язано з тим, що такі особи, ймовірно, беруть кредити на інші потреби (житло, автомобіль тощо) і мають менше фінансових можливостей для їх погашення.

#===================================
#===================================
#===================================


# Чи залежить зайнятість (EMPSTAT, EMPSTATD) від віку (AGE)?

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

# Висновок
# Більшість людей є працевлаштованими
# Найбільша група — це працевлаштовані люди, що підтверджує загальну тенденцію, що більшість працездатного населення має роботу.
# Значна частка людей знаходиться поза робочою силою
# 
# Друга за величиною категорія — це особи, які не входять до складу робочої сили (студенти, пенсіонери, особи, які не шукають роботу тощо).
# Невеликий відсоток безробітних
# 
# Частка безробітних є порівняно малою, що свідчить про низький рівень безробіття в цьому вибірковому наборі даних.
#===============================

employment_data <- data %>%
  group_by(AGE) %>%
  summarise(Employment_Rate = mean(EMPSTAT == 1, na.rm = TRUE))

# Стовпчикова діаграма зайність за віком
ggplot(employment_data, aes(x = AGE, y = Employment_Rate, fill = Employment_Rate)) +
  geom_bar(stat = "identity", show.legend = FALSE) +  
  scale_fill_gradient(low = "lightblue", high = "blue") +  
  labs(title = "Зайнятість за віком",
       x = "Вік",
       y = "Частка зайнятих") +
  theme(plot.title = element_text(size = 30),
        axis.title = element_text(size = 25),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 20))

# Висновки
# Низька зайнятість у молодому віці (до 20-25 років)
# У цій віковій групі частка зайнятих є низькою, що пояснюється тим, що більшість людей ще навчається і не є активними учасниками ринку праці.
#
# Пік зайнятості припадає на 25-50 років
# У цьому віковому діапазоні частка зайнятих досягає максимальних значень (понад 80%). Це підтверджує, що це найбільш активний період кар’єри, коли люди мають найбільше можливостей для працевлаштування.
#
# Зниження зайнятості після 50 років
# Після 50 років спостерігається поступове зниження частки зайнятих. Це може бути пов’язано зі скороченням можливостей на ринку праці, переходом на менш інтенсивну роботу або наближенням до пенсійного віку.
#
# Різке падіння зайнятості після 60-65 років
# Це очікуваний тренд, оскільки більшість людей виходять на пенсію і залишають ринок праці.
#
#Молоді люди (до 25 років) рідко працюють через навчання.
# Найвища зайнятість спостерігається у працездатному віці (25-50 років).
# Після 50 років зайнятість поступово знижується, а після 65 років більшість людей залишає ринок праці.
#=======================

data <- data %>%
  mutate(Age_Group = cut(AGE, breaks = c(seq(15, 80, 5), 100), include.lowest = TRUE, right = FALSE))

employment_by_age_group <- data %>%
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

# Висновок
# Молодь (15-20 років) має низький рівень зайнятості (~40%)
# Це пояснюється тим, що багато молодих людей ще навчаються і лише частково залучені до ринку праці.
# 
# Пік зайнятості спостерігається у вікових групах 25-55 років (≈80%)
# Саме цей період є найбільш активним у плані працевлаштування, оскільки люди мають достатній рівень освіти, досвіду та можливостей для побудови кар’єри.
# 
# Після 55 років рівень зайнятості поступово знижується
# У вікових групах 55-60 та 60-65 років зайнятість зменшується, що, ймовірно, пов’язано з виходом на пенсію або переходом на менш інтенсивні форми роботи.
# 
# Різке зниження зайнятості після 65 років
# Після 65 років частка працюючих людей суттєво падає, і у вікових групах 70+ років зайнятість є мінімальною. Це очікуваний тренд, оскільки більшість людей у цьому віці вже не працюють.

#===================================
#===================================
#===================================

# Як клас працівника (CLASSWKR, CLASSWKRD) впливає на кількість відпрацьованих тижнів за рік (WKSWORK2)?

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
  "NA - Немає даних (або відсутнє значення)"
)

# Стовпчикова діаграма Розподіл тижнів роботи
ggplot(work_distribution, aes(x = factor(WKSWORK2), y = Count, fill = factor(WKSWORK2))) +
  geom_col(alpha = 0.8) +
  facet_wrap(~CLASSWKR, labeller = as_labeller(c("1" = "Самозайнятий(Self-employed)", "2" = "Найманий працівник(Works for wages)"))) +
  scale_fill_discrete(labels = working_weeks) + 
  labs(title = "Розподіл тижнів роботи (WKSWORK2) за класом працівника",
       x = "Тижні роботи (категорії)",
       y = "Кількість працівників",
       fill = "Тижні роботи") +
  theme_minimal() +
  theme(plot.title = element_text(size = 30),
        axis.title = element_text(size = 25),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 20),
        strip.text = element_text(size = 25, face = "bold"),
        strip.background = element_rect(fill = "lightgrey", color = NA))

# Висновок
# 
# Більшість працівників відпрацьовують 50-52 тижні на рік
# Найбільша частка людей працює повний рік, тобто 50-52 тижні. Це характерно як для найманих працівників, так і для самозайнятих осіб.
# 
# Невелика кількість людей працює менше 50 тижнів
# Існують працівники, які працюють від 1 до 49 тижнів, проте їхня кількість значно менша у порівнянні з тими, хто працює повний рік.
# 
# Самозайняті та наймані працівники мають подібний розподіл робочих тижнів
# Хоча графік не чітко розділяє ці дві групи, можна припустити, що незалежно від класу працівника, більшість людей працює повний рік.
# 
# Категорія "Немає даних" (NA) також має значну кількість людей
# Це може свідчити про пропуски в даних або про осіб, які тимчасово не працювали.
#
#Клас працівника не має суттєвого впливу на кількість відпрацьованих тижнів за рік. І самозайняті, і наймані працівники здебільшого працюють повний рік (50-52 тижні). Водночас є невелика група людей, яка працює менше року, але їхня кількість значно менша.
#============================

work_distribution <- data %>%
  filter(CLASSWKR %in% c(1, 2)) %>% 
  group_by(CLASSWKR, WKSWORK2) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Стовпчикова діаграма Розподіл тижнів роботи(Відсоток працівників)
ggplot(work_distribution, aes(x = factor(WKSWORK2), y = Percentage, fill = factor(WKSWORK2))) +
  geom_col(alpha = 0.8) +
  facet_wrap(~CLASSWKR, labeller = as_labeller(c("1" = "Self-employed", "2" = "Works for wages"))) +
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
# Висновок
#
# Більшість працівників (як найманих, так і самозайнятих) працюють 50-52 тижні на рік
# У кожній групі понад 60% осіб працюють повний рік, що є найбільш поширеним робочим графіком.
# 
# Самозайняті частіше працюють нестандартну кількість тижнів
# У категорії Self-employed (самозайняті) більша частка людей працює менш ніж 50 тижнів, особливо в проміжках 1-13, 14-26 і 27-39 тижнів.
# Це може бути пов’язано з гнучкістю графіку, сезонністю роботи або проєктною зайнятістю.
#
# Наймані працівники більш стабільно працюють повний рік
# Категорія Works for wages (наймані працівники) має трохи більший відсоток працівників, що відпрацювали 50-52 тижні порівняно із самозайнятими.
# Це пояснюється тим, що наймані працівники зазвичай працюють за контрактами або в штаті компаній, які передбачають стабільний графік.
#
# Дані з NA (відсутні значення)
# В обох групах є значна частка людей, у яких дані про кількість відпрацьованих тижнів відсутні. Це може свідчити про нерегулярну зайнятість або неповні записи в базі даних.

# Наймані працівники частіше працюють повний рік (50-52 тижні), ніж самозайняті.
# Самозайняті особи більш гнучкі у виборі робочого графіку і частіше працюють менше 50 тижнів на рік.
# В обох категоріях переважна більшість працює повний рік, але самозайняті мають більш розподілену кількість відпрацьованих тижнів, що свідчить про різноманітність робочих моделей.

# =========================================
# =========================================
# =========================================

# USA USA USA USA USA USA USA USA USA USA USA USA USA USA
# Map of state avg age
library(ggplot2)
library(dplyr)
library(usmap)
library(usmapdata)

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
  theme(legend.position = "right") +
  
  # Додаємо підписи штатів
  geom_sf_text(data = data_labels, aes(label = abbr), color = "white") +
  theme(legend.position = "right",
        plot.title = element_text(size = 30),
        axis.title = element_text(size = 25),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18))


#============================================================
# Map of state avg education

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
                        name = "Середній рівень освіти", na.value = "grey50") +
  
  labs(title = "Середній рівень освіти кожного штату США", x = "", y = "") +
  
  # Додаємо підписи штатів
  geom_sf_text(data = data_labels, aes(label = abbr), color = "white", size = 4)+
  theme(legend.position = "right",
        plot.title = element_text(size = 30),
        axis.title = element_text(size = 25),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18))
ggsave("plots123/mapUSA_INCTOT.png", height = 9, width = 15)

#=============================

data_not_clean <- read_csv("data/usa_00.csv")

#for did not clean data
#======================================
p <- ggplot(data_not_clean, aes(sample = INCTOT)) +
    stat_qq_point() + stat_qq_line() + stat_qq_band() +
    labs(title = "Квантильний графік (QQ Plot) доходів у США",
      x = "Квантилі нормального розподілу", y = "INCTOT, $") +
    theme(plot.title = element_text(size = 30),
        axis.title = element_text(size = 25),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 20))
ggsave("Квантильний_графік_INCTOT_did_not_clean.jpg",p)

p <- ggplot(data_not_clean, aes(sample = INCWAGE)) +
  stat_qq_point() + stat_qq_line() + stat_qq_band() +
  labs(title = "Квантильний графік зарплат у США",
    x = "Квантилі нормального розподілу", y = "INCWAGE, $") +
  theme(plot.title = element_text(size = 30),
        axis.title = element_text(size = 25),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 20))
ggsave("Квантильний_графік_INCWAGE_did_not_clean.jpg",p)
#======================================

#for cleaned data
#======================================
p <- ggplot(data, aes(sample = INCTOT)) +
  stat_qq_point() + stat_qq_line() + stat_qq_band() +
  labs(title = "Квантильний графік (QQ Plot) доходів у США",
       x = "Квантилі нормального розподілу", y = "INCTOT, $") +
  theme(plot.title = element_text(size = 30),
        axis.title = element_text(size = 25),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 20))
ggsave("Квантильний_графік_INCTOT_cleaned.jpg",p)


p <- ggplot(data, aes(sample = INCWAGE)) +
  stat_qq_point() + stat_qq_line() + stat_qq_band() +
  labs(title = "Квантильний графік зарплат у США",
       x = "Квантилі нормального розподілу", y = "INCWAGE, $") +
  theme(plot.title = element_text(size = 30),
        axis.title = element_text(size = 25),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 20))
ggsave("Квантильний_графік_INCWAGE_cleaned.jpg",p)

#======================================
people <- data %>%
  mutate(
    CLASSWKR = factor(CLASSWKR, levels = c(0, 1, 2), labels = c("NA", "Self-employed", "Works for wages")),
    EDUC = factor(EDUC, levels = 0:12, labels = c("N/A or no schooling", "Nursery school to grade 4", "Grade 5-8", "Grade 9", "Grade 10", "Grade 11", "Grade 12", "1 year of college", "2 years of college", "3 years of college", "4 years of college", "5+ years of college", "Missing"))
  )

ggplot(data, aes(x = AGE, y = INCTOT)) +
  geom_point(alpha = 0.5, color = "red") +
  labs(title = "Залежність між віком та рівнем доходу",
       x = "Вік",
       y = "Загальний дохід") +
  theme_minimal()

# 7.2 

 data %>%
  group_by(AGE, SEX) %>%
  summarise(mean_income = mean(INCTOT, na.rm = TRUE)) %>%
  ggplot(aes(x = AGE, y = mean_income, color = as.factor(SEX))) +
  geom_line(size = 1) +
  scale_color_manual(values = c("green", "blue")) +
  labs(title = "Залежність середнього доходу від віку та статі",
       x = "Вік",
       y = "Середній дохід",
       color = "Стать") +
  theme_minimal()
