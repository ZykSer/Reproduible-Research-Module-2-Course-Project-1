# Загрузка пакетов
library(dplyr)
library(ggplot2)

# Чтение данных
activity <- read.csv("activity.csv")

# Преобразование формата даты
activity$date <- as.Date(activity$date)

#num_step<-tapply(activity$steps,activity$date,FUN = function(x) sum(x, na.rm = TRUE))
#hist(num_step)
#mean_step<-tapply(activity$steps,activity$date,FUN = function(x) mean(x, na.rm = TRUE))
#median_step<-tapply(activity$steps,activity$date,FUN = function(x) median(x, na.rm = TRUE))
#print("Mean steps per day")
#print(mean_step)
#print("Meadian steps per day")
#print(median_step)

# Суммируем шаги по дате, исключая NA
steps_per_day <- activity %>%
  filter(!is.na(steps)) %>%
  group_by(date) %>%
  summarize(total_steps = sum(steps))

# Гистограмма
g<-ggplot(steps_per_day, aes(x = total_steps)) +
  geom_histogram(binwidth = 1000, fill = "blue", color = "black") +
  labs(title = "Total steps per day", x = "Steps", y = "Frequency")
print(g)

m1<-mean(steps_per_day$total_steps)
m2<-median(steps_per_day$total_steps)
print(m1)
print(m2)

# Средние шаги по интервалам
interval_avg <- activity %>%
  filter(!is.na(steps)) %>%
  group_by(interval) %>%
  summarize(avg_steps = mean(steps))

# График
g<-ggplot(interval_avg, aes(x = interval, y = avg_steps)) +
  geom_line(color = "red") +
  labs(title = "Average daily activity pattern", x = "Interval", y = "Average Steps")
print(g)

i1<- interval_avg[which.max(interval_avg$avg_steps), ]
print(i1)

s1<-sum(is.na(activity$steps))
print(s1)

# Создаем словарь средних значений
interval_means <- interval_avg$avg_steps
names(interval_means) <- interval_avg$interval

# Подставляем
activity_filled <- activity
activity_filled$steps[is.na(activity_filled$steps)] <-
  interval_means[as.character(activity_filled$interval[is.na(activity_filled$steps)])]

steps_per_day_filled <- activity_filled %>%
  group_by(date) %>%
  summarize(total_steps = sum(steps))

# Гистограмма
g<-ggplot(steps_per_day_filled, aes(x = total_steps)) +
  geom_histogram(binwidth = 1000, fill = "green", color = "black") +
  labs(title = "Total steps per day (after imputation)", x = "Steps", y = "Frequency")
print(g)
# Новые среднее и медиана
mean(steps_per_day_filled$total_steps)
median(steps_per_day_filled$total_steps)

activity_filled$day_type <- ifelse(weekdays(activity_filled$date) %in% c("Saturday", "Sunday"),
                                   "weekend", "weekday")

# Усреднение
avg_steps_daytype <- activity_filled %>%
  group_by(interval, day_type) %>%
  summarize(avg_steps = mean(steps), .groups = "drop")

# График
g<-ggplot(avg_steps_daytype, aes(x = interval, y = avg_steps)) +
  geom_line() +
  facet_wrap(~day_type, ncol = 1) +
  labs(title = "Average steps by interval: Weekday vs Weekend",
       x = "Interval", y = "Average steps")
print(g)



