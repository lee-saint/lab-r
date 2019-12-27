stu_cla <- read.csv("data/student_class_2.csv", encoding = "ANSI")
str(stu_cla)
head(stu_cla)
stu_cla_tot <- stu_cla %>% filter(loc == "합계")

ggplot(data = stu_cla_tot, mapping = aes(x = year, y = student_per_class)) + geom_line()

year_birth <- read.csv("data/year_birth.csv", encoding = "ANSI")
str(year_birth)
year_birth <- year_birth %>% rename("year" = "기간", "birth" = "출생", "death" = "사망")
year_birth
stu_cla_tot <- stu_cla_tot %>% select(-loc)
year_birth_stu_cla <- stu_cla_tot %>% left_join(year_birth, by = "year")

ggplot(data = year_birth, mapping = aes(x = year, y = birth)) + geom_line()

p <- ggplot(data = year_birth_stu_cla, mapping = aes(x = student_per_class, y = birth)) + geom_point()
ggplotly(p)

ggplot(data = year_birth_stu_cla, mapping = aes(x = year, group = 1)) +
  geom_line(mapping = aes(y = birth), color = "orange") +
  geom_line(mapping = aes(y = student_per_class), color = "darkblue")
