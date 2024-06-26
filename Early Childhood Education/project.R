library(tidyverse)
library(janitor)

prek_students_served <- read.csv(file = "2022-23-prekindergarten-students-served.csv")

prek_students_served <- prek_students_served %>% 
  clean_names() %>%
  rename(region = pre_k_region)

prek_students_served_clean <- prek_students_served %>% 
  mutate(
    total_served = as.numeric(total_served_by_all_programs_unduplicated_3s_4s),
    full_day_seats = as.numeric(total_full_day_seats_unduplicated_3s_4s),
    half_day_seats = as.numeric(total_half_day_seats_unduplicated_3s_4s)
)

prek_students_served_clean <- prek_students_served_clean %>% 
  drop_na(total_served, full_day_seats, half_day_seats)

selected_prek <- prek_students_served_clean %>% 
  select(code, name, county, region, total_served, full_day_seats, half_day_seats)

aggregated_data <- prek_students_served_clean %>%
  group_by(region) %>%
  summarize(
    total_enrollment = sum(total_served, na.rm = TRUE))


ggplot(aggregated_data, aes(x = reorder(region, -total_enrollment), y = total_enrollment, fill = region)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Total Enrollment in Early Education by Region",
       x = "Region",
       y = "Total Enrollment") +
  theme(legend.position = "none")

write_csv(aggregated_data, "aggregated_prek.csv")
  