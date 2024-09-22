last_days <- reshaped_data %>%
  group_by(ID) %>%
  arrange(ID, desc(Time)) %>%
  summarize(
    LastDay1 = nth(Time, 1),  # The most recent time
    LastDay2 = nth(Time, 2)   # The second most recent time
  )

last_two_days_before_harvest <- reshaped_data %>%
  left_join(last_days, by = "ID") %>%
  filter(Time %in% c(LastDay1, LastDay2)) %>%
  arrange(ID, desc(Time))

hourly_loss <- last_two_days_before_harvest %>%
  group_by(ID) %>%
  summarize(
    massdiff_g = last(sample) - first(sample),
    timediff = as.numeric(difftime(first(Time), last(Time), units = "hours")),
    mass_per_hr = massdiff_g / timediff
  ) %>%
  ungroup()

head(last_days)
head(last_two_days_before_harvest)

summary(hourly_loss)
head(hourly_loss)

ggplot(hourly_loss, aes(x = ID, y = mass_per_hr, color = ID)) +
  geom_point() +
  geom_line() +
  labs(title = "Hourly Water Loss Per Plant", x = "Plant ID", y = "Water Loss (g/hr)") +
  theme_minimal()

print(ggplot)

ggplot(hourly_loss, aes(x = ID, y = mass_per_hr, color = ID)) +
  geom_point(size = 3) +
  labs(title = "Hourly Water Loss Per Plant", x = "Plant ID", y = "Water Loss (g/hr)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(ggplot)

hourly_loss <- hourly_loss %>%
  left_join(Treatment_key, by = "ID") %>%
  group_by(Treatment) %>%
  summarize(mean_mass_per_hr = mean(mass_per_hr, na.rm = TRUE))

ggplot(hourly_loss, aes(x = Treatment, y = mean_mass_per_hr, fill = Treatment)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Hourly Water Loss by Treatment", x = "Treatment", y = "Mean Water Loss (g/hr)") +
  theme_minimal()

print(ggplot)





# Assuming Treatment_key has columns: ID, Treatment, Species
hourly_loss <- hourly_loss %>%
  left_join(Treatment_key, by = "ID")

# Convert the `LastDay1` to a Date if not already
hourly_loss$LastDay1 <- as.Date(hourly_loss$LastDay1)

ggplot(hourly_loss, aes(x = LastDay1, y = mass_per_hr, color = Species, group = interaction(Treatment, Species))) +
  geom_point() +
  geom_line() +
  labs(title = "Hourly Water Loss Grouped by Treatment and Species",
       x = "Date", y = "Water Loss (g/hr)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~Treatment)

print(ggplot)







Treatment_key <- read.csv("Treatment_key.csv")

hourly_loss <- hourly_loss %>%
  left_join(Treatment_key, by = "ID")

ggplot(hourly_loss, aes(x = as.Date(LastDay1), y = mass_per_hr, color = Species, group = interaction(Treatment, Species))) +
  geom_point() +
  geom_line() +
  labs(title = "Hourly Water Loss Grouped by Treatment and Species",
       x = "Date", y = "Water Loss (g/hr)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~Treatment)

print(ggplot)
