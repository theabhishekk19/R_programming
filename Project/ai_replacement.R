# Load Libraries
library(ggplot2)
library(dplyr)
library(corrplot)
library(GGally)
library(tidyr)

# Load Dataset
data <- read.csv("D:/ai_job_replacement_2020_2026_v2.csv")

# Create PDF file to save all plots
pdf("D:/AI_Job_Replacement_Plots.pdf", width = 12, height = 8)

# 1. Line Chart - Avg Automation Risk by Year
ggplot(data, aes(year, automation_risk_percent)) +
  stat_summary(fun = mean, geom = "line", color = "blue")

# 2. Line Chart - Avg Salary Change by Year
ggplot(data, aes(year, salary_change_percent)) +
  stat_summary(fun = mean, geom = "line", color = "red")

# 3. Multi-Line Chart - Risk Trend by Industry
ggplot(data, aes(year, automation_risk_percent, color = industry)) +
  stat_summary(fun = mean, geom = "line")

# 4. Multi-Line Chart - Salary Trend by Country
ggplot(data, aes(year, salary_change_percent, color = country)) +
  stat_summary(fun = mean, geom = "line")

# 5. Horizontal Bar - Top 10 Job Roles Highest Risk
data %>%
  group_by(job_role) %>%
  summarise(risk = mean(automation_risk_percent)) %>%
  slice_max(risk, n = 10) %>%
  ggplot(aes(reorder(job_role, risk), risk)) +
  geom_col(fill = "red") +
  coord_flip()

# 6. Grouped Bar - AI Adoption Level by Industry
ggplot(data, aes(industry, fill = ai_adoption_level)) +
  geom_bar(position = "dodge") +
  coord_flip()

# 7. Pie Chart - Risk Categories
risk_counts <- data %>%
  count(automation_risk_category)

ggplot(risk_counts, aes("", n, fill = automation_risk_category)) +
  geom_col(width = 1) +
  coord_polar("y")

# 8. Donut Chart - AI Adoption Levels
adopt <- data %>% count(ai_adoption_level)

ggplot(adopt, aes(2, n, fill = ai_adoption_level)) +
  geom_col(width = 1) +
  coord_polar("y") +
  xlim(0.5, 2.5)

# 9. Histogram - Salary Before AI
ggplot(data, aes(salary_before_usd)) +
  geom_histogram(fill = "blue", bins = 30)

# 10. Histogram - Salary Change %
ggplot(data, aes(salary_change_percent)) +
  geom_histogram(fill = "orange", bins = 30)

# 11. Density Plot - Automation Risk
ggplot(data, aes(automation_risk_percent)) +
  geom_density(fill = "green", alpha = 0.5)

# 12. Boxplot - Salary by Industry
ggplot(data, aes(industry, salary_before_usd, fill = industry)) +
  geom_boxplot() +
  coord_flip()

# 13. Boxplot - Salary Change by Risk Category
ggplot(data, aes(automation_risk_category,
                 salary_change_percent,
                 fill = automation_risk_category)) +
  geom_boxplot()

# 14. Scatter Plot - Salary Before vs After
ggplot(data, aes(salary_before_usd, salary_after_usd)) +
  geom_point(alpha = 0.5, color = "purple")

# 15. Bubble Chart
ggplot(data, aes(skill_gap_index,
                 reskilling_urgency_score,
                 size = ai_disruption_intensity,
                 color = industry)) +
  geom_point(alpha = 0.6)

# 16. Faceted Risk Trends by Industry
ggplot(data, aes(year, automation_risk_percent)) +
  stat_summary(fun = mean, geom = "line", color = "darkblue") +
  facet_wrap(~industry)

# Close PDF file
dev.off()