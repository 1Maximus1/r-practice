library(ggplot2)
library(qqplotr)
library(magrittr)
library(dplyr) 
people <- read.csv("data/usa_00004.csv")

## Cпочатку подивимося на нашу центральну змінну
# p <- ggplot(people, aes(sample = INCTOT)) +
#     stat_qq_point() + stat_qq_line() + stat_qq_band() +
#     labs(x = "Квантилі нормального розподілу", y = "INCTOT, $") +
#     theme(axis.title = element_text(size = 20),
#     axis.text = element_text(size = 18))

#  Бачимо що має багато значення на 9999999
#  на сайті ipums так позначаються NA 
# ggsave("plotINCTOT_qqplot.png", plot = p, width = 10)


# people_f <- people %>%
#     filter(INCTOT > 2500000) %>%
#     arrange(AGE)

# print(head(people_f))

# people_f <- people %>%
#     filter(INCTOT > 2500000) %>%
#     arrange(desc(AGE))
# # Подивившись на дані можна побачити що значення "NA"
# # присвоюється до дітей віком від 0 до 14 року 
# print(head(people_f))

# Впевнимся в цьому та побудуємо графік
# p <- ggplot(people, aes(x = AGE, y = INCTOT)) + 
#   geom_point() + 
#   scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + 
#   scale_y_continuous(breaks = scales::pretty_breaks(n = 10))

# Бачимо що значення "NA" присвоюють дітям до 14 років
# ggsave("plotAGE_INCTOT.png", plot = p, width = 10)
## Оскільки основним предметом нашого дослідення є виявлення чинників які
## Впливають на зарплату доцільно видалити ці дані 
people_01 <- people %>% filter(!INCTOT > 2500000)

## Перевіримо чи є ще якість викиди у колонці INCTOT
# p <- ggplot(people_01, aes(sample = INCTOT)) +
#     stat_qq_point() + stat_qq_line() + stat_qq_band() +
#     labs(x = "Квантилі нормального розподілу", y = "INCTOT, $") +
#     theme(axis.title = element_text(size = 20),
#     axis.text = element_text(size = 18))
## Бачимо 3 потенційні викиди
# ggsave("plotINCTOT_qqplot_01.png", plot = p, width = 10)

# Перша персона є дуже підозрілою на викид бо її рід занять 
# вказаний як Керівник першої ланки виробничо-оперативних працівників
# інші двоє є судовим працівником та менеджером
# print(head(people_01 %>% arrange(desc(INCTOT))))

# тож доцільно видалити їх всіх заради більшої консисентності вибірки
people_02 <- people_01 %>% filter(!INCTOT >= 1337281)

# p <- ggplot(people_02, aes(sample = INCTOT)) +
#     stat_qq_point() + stat_qq_line() + stat_qq_band() +
#     labs(x = "Квантилі нормального розподілу", y = "INCTOT, $") +
#     theme(axis.title = element_text(size = 20),
#     axis.text = element_text(size = 18))
# # Бачимо 3 потенційні викиди
# ggsave("plotINCTOT_qqplot_02.png", plot = p, width = 10)

# QQ plot змінною INCWAGE
# p <- ggplot(people_02, aes(sample = INCWAGE)) +
#     stat_qq_point() + stat_qq_line() + stat_qq_band() +
#     labs(x = "Квантилі нормального розподілу", y = "INCWAGE, $") +
#     theme(axis.title = element_text(size = 20),
#     axis.text = element_text(size = 18))

# # бачимо що значення 999999 є кодом "NA"  
# ggsave("plotINCWAGE_qqplot_01.png", plot = p, width = 10)

# people_f <- people_02 %>%
#     filter(INCWAGE > 990000) %>%
#     arrange(AGE)

# print(head(people_f))


# people_f <- people_02 %>%
#     filter(INCWAGE > 990000) %>%
#     arrange(desc(AGE))

# print(head(people_f))
# Бачимо що значення NA присвоюється дітям 15 років
# Їх видалити також буде доцільно 
people_03 <- people_02 %>% filter(!INCWAGE >= 999998)

# # Перевіримо чи є ще якість викиди у колонці INCWAGE
# p <- ggplot(people_03, aes(sample = INCWAGE)) +
#     stat_qq_point() + stat_qq_line() + stat_qq_band() +
#     labs(x = "Квантилі нормального розподілу", y = "INCWAGE, $") +
#     theme(axis.title = element_text(size = 20),
#     axis.text = element_text(size = 18))

# # Маємо ще 2 можливих викиди
# ggsave("plotINCWAGE_qqplot_02.png", plot = p, width = 10)


# Ці дві персони виявилися фінансовим менеджером та CEO 
# тож можна їх не видаляти
# print(head(people_03 %>% arrange(desc(INCWAGE))))


# Перевіримо чи є ще якість викиди у колонці INCWAGE
# p <- ggplot(people_03, aes(sample = YRMARR)) +
#     stat_qq_point() + stat_qq_line() + stat_qq_band() +
#     labs(x = "Квантилі нормального розподілу", y = "YRMARR, років") +
#     theme(axis.title = element_text(size = 20),
#     axis.text = element_text(size = 18))

# # Маємо ще 2 можливих викиди
# ggsave("plotYRMARR_qqplot_01.png", plot = p, width = 10)

# print(max(people_03$YRMARR))

# p <- people_03 %>% summarize(across(everything(), ~ sum(is.na(.)))) %>%
# select(where(~ all(.) > 0))

## датасет не має жодного явно пропущеного значення
## отже всі вони є закодованими 
# print(p)

# Створимо вибірку з явним пропуском даних
people_04 <- people_03 %>% mutate(MORTGAGE = replace(MORTGAGE, which(MORTGAGE == 0L), NA))

people_04 <- people_04 %>% mutate(YRMARR = replace(YRMARR, which(YRMARR == 0L), NA))

people_04 <- people_04 %>% mutate(CITIZEN = replace(CITIZEN, which(CITIZEN == 0L), NA))

people_04 <- people_04 %>% mutate(DEGFIELD = replace(DEGFIELD, which(DEGFIELD == 0L), NA))

people_04 <- people_04 %>% mutate(CLASSWKR = replace(CLASSWKR, which(CLASSWKR == 0L), NA))

people_04 <- people_04 %>% mutate(WKSWORK2 = replace(WKSWORK2, which(WKSWORK2 == 0L), NA))

p <- people_04 %>% summarize(across(everything(), ~ sum(is.na(.)))) %>%
select(where(~ all(.) > 0))

# датасет не має жодного явно пропущеного значення
# отже всі вони є закодованими 
print(p)

write.csv(people_04,"data/usa_01.csv",
          row.names=FALSE)