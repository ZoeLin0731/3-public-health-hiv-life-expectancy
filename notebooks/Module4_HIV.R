library(ggplot2)
library(tidyr)
library(tidyverse)
library(stats)
library(janitor)
library(plotly)
library(splines)
HIV_data <- read.csv("Table1.csv")
HIV_data <- clean_names(HIV_data)

HIV_data <- HIV_data %>% rename(people_living_hiv = estimated_number_of_people_living_with_hiv,
                                death_due_to_hiv = estimated_number_of_deaths_due_to_hiv,
                                women_living_hiv = estimated_number_of_women_15_living_with_hiv,
                                pregant_women_hiv_status = percent_of_pregnant_women_with_known_hiv_status,
                                mother_child_transmission = final_mother_to_child_transmission_rate_0_14,
                                children_orphaned = estimated_number_of_children_aged_0_17_who_have_lost_one_or_both_parents_due_to_aids,
                                incidence_per_1000 = all_ages_incidence_per_1000_uninfected_population,
                                pregnant_women_need_ART = number_of_pregnant_women_living_with_hiv_needing_antiretroviral_treatment_to_prevent_vertical_transmission_of_hiv)
colnames(HIV_data)
HIV_data$year <- as.factor(HIV_data$year)
HIV_data$country_name <- as.factor(HIV_data$country_name)
class(HIV_data)
summary(HIV_data)

df <- HIV_data %>% 
    group_by(year) %>% 
    summarise(people_living_hiv = mean(people_living_hiv),
              death_due_to_hiv = mean(death_due_to_hiv),
              women_living_hiv = mean(women_living_hiv),
              pregant_women_hiv_status = mean(pregant_women_hiv_status),
              mother_child_transmission = mean(mother_child_transmission),
              children_orphaned = mean(children_orphaned),
              incidence_per_1000 = mean(incidence_per_1000),
              pregnant_women_need_ART = mean(pregnant_women_need_ART))

df2 <- HIV_data %>% group_by(country_name) %>% 
    summarise(people_living_hiv = sum(people_living_hiv),
              death_due_to_hiv = sum(death_due_to_hiv),
              women_living_hiv = sum(women_living_hiv),
              pregant_women_hiv_status = sum(pregant_women_hiv_status),
              mother_child_transmission = sum(mother_child_transmission),
              children_orphaned = sum(children_orphaned),
              incidence_per_1000 = sum(incidence_per_1000),
              pregnant_women_need_ART = sum(pregnant_women_need_ART))

df2 <- df2 %>% mutate(
    percent_living_hiv = (people_living_hiv / sum(people_living_hiv)) * 100,
    percent_deaths_hiv = (death_due_to_hiv / sum(death_due_to_hiv)) * 100,
    percent_women_living_hiv = (women_living_hiv / sum(women_living_hiv)) * 100,
    percent_pregant_hiv_status = (pregant_women_hiv_status / sum(pregant_women_hiv_status)) * 100,
    percent_mother_child_transmission = (mother_child_transmission / sum(mother_child_transmission)) *100,
    percent_children_orphaned = (children_orphaned / sum(children_orphaned)) * 100,
    percent_incidence_per_1000 = (incidence_per_1000 / sum(incidence_per_1000)) * 100,
    percent_pregnant_women_need_ART = (pregnant_women_need_ART / sum(pregnant_women_need_ART)) * 100)

# 1st indicator
p1 <- ggplot(df, aes(x = year, y = people_living_hiv, group = 1)) +
    geom_line(color = "salmon3", linewidth = 2) +
    geom_point(color = "salmon4", size = 3) +
    labs(title = "Estimated People Living with HIV (2007-2010)",
         x = "Year", y = "People Living with HIV") +
    theme_bw() +
    theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold", color = "gray15"),
        axis.title.y = element_text(size = 14, face = "bold", color = "gray15"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12)) +
    ylim(210500, 220500)
ggplotly(p1)

# 2end indicator
p2 <- ggplot(df, aes(x = year, y = death_due_to_hiv, group = 1)) +
    geom_area(fill = "wheat", alpha = 0.6) +
    geom_line(color = "wheat3", linewidth = 2) +
    geom_point(color = "wheat4", size = 3) +
    labs(title = "Trend of Deaths due to HIV (2007-2020)",
        x = "Year",
        y = "Estimated Deaths due to HIV") +
    theme_bw() +
    theme(plot.title = element_text(size = 16, face = "bold"),
          axis.title.x = element_text(size = 14, face = "bold", color = "gray15"),
          axis.title.y = element_text(size = 14, face = "bold", color = "gray15"),
          axis.text.x = element_text(size = 12), 
          axis.text.y = element_text(size = 12))
ggplotly(p2)

# 3rd indicator
p3 <- ggplot(df, aes(x = year, y = women_living_hiv, group = 1)) +
    geom_line(color = "royalblue1", linewidth = 2) +
    geom_point(size = 3, color = "royalblue4") +
    labs(title = "Trend of Women Living with HIV (2007-2020)",
         x = "Year",
         y = "Estimated Women Living with HIV") +
    theme_bw() +
    theme(plot.title = element_text(size = 16, face = "bold"),
          axis.title.x = element_text(size = 14, face = "bold", color = "gray15"),
          axis.title.y = element_text(size = 14, face = "bold", color = "gray15"),
          axis.text.x = element_text(size = 12), 
          axis.text.y = element_text(size = 12)) 
ggplotly(p3)

# 4th indicator
p4 <- ggplot(df, aes(x = year, y = pregant_women_hiv_status, group = 1)) +
    geom_area(fill = "pink", alpha = 0.4) +
    geom_line(color = "pink3", linewidth = 2) +
    geom_point(color = "pink4", size = 3) +
    labs(title = "Trend of Pregnant Women with Known HIV Status (2007-2020)",
        x = "Year",
        y = "Pregnant Women with Known HIV Status(%)") +
    theme_bw() +
    theme(plot.title = element_text(size = 16, face = "bold"),
          axis.title.x = element_text(size = 14, face = "bold", color = "gray15"),
          axis.title.y = element_text(size = 14, face = "bold", color = "gray15"),
          axis.text.x = element_text(size = 12), 
          axis.text.y = element_text(size = 12))
ggplotly(p4)

# 5th indicator
p5 <- ggplot(df, aes(x = year, y = mother_child_transmission, group = 1)) +
    geom_area(fill = "darkslategray2", alpha = 0.4) +
    geom_line(color = "darkslategray4", linewidth = 2) +
    geom_point(color = "darkslategray", size = 3) +
    labs(title = "Trend of Mother-to-Child Transmission Rate (2007-2020)",
        x = "Year",
        y = "Mother-to-Child Transmission Rate (%)") +
    theme_bw() +
    theme(plot.title = element_text(size = 16, face = "bold"),
          axis.title.x = element_text(size = 14, face = "bold", color = "gray15"),
          axis.title.y = element_text(size = 14, face = "bold", color = "gray15"),
          axis.text.x = element_text(size = 12), 
          axis.text.y = element_text(size = 12))
ggplotly(p5)

# 6th indicator
p6 <- ggplot(df, aes(x = year, y = children_orphaned, group = 1)) +
    geom_line(color = "mediumpurple1", linewidth = 2) +
    geom_point(size = 3, color = "mediumpurple4") +
    labs(title = "Trend of Orphaned Children Due to AIDS (2007-2020)",
         x = "Year",
         y = "Estimated Orphaned Children (0-17)") +
    theme_bw() +
    theme(plot.title = element_text(size = 16, face = "bold"),
          axis.title.x = element_text(size = 14, face = "bold", color = "gray15"),
          axis.title.y = element_text(size = 14, face = "bold", color = "gray15"),
          axis.text.x = element_text(size = 12), 
          axis.text.y = element_text(size = 12))
ggplotly(p6)

# 7th indicator
p7 <- ggplot(df, aes(x = year, y = incidence_per_1000, group = 1)) +
    geom_area(fill = "palegreen1", alpha = 0.4) +
    geom_line(color = "palegreen3", linewidth = 2) +
    geom_point(color = "palegreen4", size = 3) +
    labs(title = "Trend of HIV Incidence (2007-2020)",
        x = "Year",
        y = "All Ages Incidence (per 1000)") +
    theme_bw() +
    theme(plot.title = element_text(size = 16, face = "bold"),
          axis.title.x = element_text(size = 14, face = "bold", color = "gray15"),
          axis.title.y = element_text(size = 14, face = "bold", color = "gray15"),
          axis.text.x = element_text(size = 12), 
          axis.text.y = element_text(size = 12))
ggplotly(p7)

# 8th indicator
p8 <- ggplot(df, aes(x = year, y = pregnant_women_need_ART, group = 1)) +
    geom_line(color = "coral1", linewidth = 2) +
    geom_point(size = 3, color = "coral4") +
    labs(title = "Trend of Pregnant Women Needing ART (2007-2020)",
         x = "Year",
         y = "Pregnant Women Needing ART") +
    theme_bw() +
    theme(plot.title = element_text(size = 16, face = "bold"),
          axis.title.x = element_text(size = 14, face = "bold", color = "gray15"),
          axis.title.y = element_text(size = 14, face = "bold", color = "gray15"),
          axis.text.x = element_text(size = 12), 
          axis.text.y = element_text(size = 12))
ggplotly(p8)

ggsave("1_Trend_People_Living_with_HIV.png", p1)
ggsave("2_Trend_Deaths_due_to_HIV.png", p2)
ggsave("3_Estimated_Women_Living_with_HIV.png", p3)
ggsave("4_Pregnant_Women_with_Known_HIV_Status(%).png", p4)
ggsave("5_Mother_to_Child_Transmission_Rate(%).png", p5)
ggsave("6_Trend_Deaths_due_to_HIV.png", p6)
ggsave("7_Estimated_Orphaned_Children_(0-17).png", p7)
ggsave("8_Pregnant_Women_Needing_ART.png", p8)

interactive_plot1 <- ggplotly(p1)
interactive_plot2 <- ggplotly(p2)
interactive_plot3 <- ggplotly(p3)
interactive_plot4 <- ggplotly(p4)
interactive_plot5 <- ggplotly(p5)
interactive_plot6 <- ggplotly(p6)
interactive_plot7 <- ggplotly(p7)
interactive_plot8 <- ggplotly(p8)

# Save as HTML
htmlwidgets::saveWidget(interactive_plot1, "interactive_plot1.html")
htmlwidgets::saveWidget(interactive_plot2, "interactive_plot2.html")
htmlwidgets::saveWidget(interactive_plot3, "interactive_plot3.html")
htmlwidgets::saveWidget(interactive_plot4, "interactive_plot4.html")
htmlwidgets::saveWidget(interactive_plot5, "interactive_plot5.html")
htmlwidgets::saveWidget(interactive_plot6, "interactive_plot6.html")
htmlwidgets::saveWidget(interactive_plot7, "interactive_plot7.html")
htmlwidgets::saveWidget(interactive_plot8, "interactive_plot8.html")

# Country level analysis.
p11 <- ggplot(df2, aes(x = "", y = percent_living_hiv, fill = country_name)) +
    geom_bar(stat = "identity", position = "stack", color = "white", alpha = 0.8) +
    geom_text(aes(label = paste0(round(percent_living_hiv), "%")), position = position_stack(vjust = 0.5), size = 2.5, color = "white") +
    theme_grey() +
    labs(title = "Percentage of People Living with HIV by Country",
         x = "", y = "Percentage (%)", fill = "Country Name") +
    theme(plot.title = element_text(size = 11.5, face = "bold"),
          plot.title.position = "plot",
          axis.title.y = element_text(size = 14, face = "bold", color = "gray15"))
p11
ggsave("1_Percentage_People_Living_HIV_country.png", p11)

p12 <- ggplot(df2, aes(x = "", y = percent_deaths_hiv, fill = country_name)) +
    geom_bar(stat = "identity", position = "stack", color = "white", , alpha = 0.8) +
    geom_text(aes(label = paste0(round(percent_deaths_hiv), "%")), position = position_stack(vjust = 0.5), size = 2.5, color = "white") +
    theme_grey() +
    labs(title = "Percentage of Deaths Due to HIV by Country",
         x = "", y = "Percentage (%)", fill = "Country Name") +
    theme(plot.title = element_text(size = 11.5, face = "bold"),
          plot.title.position = "plot",
          axis.title.y = element_text(size = 14, face = "bold", color = "gray15"))
p12
ggsave("2_Percentage_Deaths_Due_HIV_country.png", p12)

p13 <- ggplot(df2, aes(x = "", y = percent_women_living_hiv, fill = country_name)) +
    geom_bar(stat = "identity", position = "stack", color = "white", , alpha = 0.8) +
    geom_text(aes(label = paste0(round(percent_women_living_hiv), "%")), position = position_stack(vjust = 0.5), size = 2.5, color = "white") +
    theme_grey() +
    labs(title = "Percentage of Women Living with HIV by Country",
         x = "", y = "Percentage (%)", fill = "Country Name") +
    theme(plot.title = element_text(size = 11.5, face = "bold"),
          plot.title.position = "plot",
          axis.title.y = element_text(size = 14, face = "bold", color = "gray15"))
p13
ggsave("3_Percentage_Women_Living_HIV_country.png", p13)

p14 <- ggplot(df2, aes(x = "", y = percent_pregant_hiv_status, fill = country_name)) +
    geom_bar(stat = "identity", width = 1, color = "white", , alpha = 0.8) +
    coord_polar("y", start = 0) +
    geom_text(aes(label = paste0(round(percent_pregant_hiv_status))), 
              position = position_stack(vjust = 0.5), size = 3, color = "white") +
    labs(title = "Percentage of Pregant Women Living with HIV Status",
         fill = "Country Name", x = "", y = "Percentage (%)") +
    theme_void() +
    theme_bw() +
    theme(plot.title = element_text(size = 14, face = "bold"), 
          axis.title.x.bottom = element_text(size = 12, face = "bold", color = "gray15"))
p14
ggsave("4_Percentage_Pregant_Women_Living_with_HIV_Status.png", p14)

p15 <- ggplot(df2, aes(x = country_name, y = percent_mother_child_transmission, fill = country_name)) +
    geom_bar(stat = "identity", width = 1, color = "white", alpha = 0.8) +
    geom_text(aes(label = paste0(round(percent_mother_child_transmission, 1), "%")), 
              vjust = -0.5, size = 2.5) +
    labs(title = "Percentage of Mother to Child Transmition by Country",
         fill = "Country Name", x = "", y = "Percentage (%)") +
    theme_bw() +
    theme(plot.title = element_text(size = 16, face = "bold"),
          axis.title.y = element_text(size = 14, face = "bold", color = "gray15"),
          axis.text.x = element_text(angle = 45, size = 10, hjust = 1))
p15
ggsave("5_Percentage_Pregant_Women_Living_with_HIV_Status.png", p15)

p16 <- ggplot(df2, aes(x = "", y = percent_children_orphaned, fill = country_name)) +
    geom_bar(stat = "identity", width = 1, color = "white", alpha = 0.8) +
    coord_polar("y", start = 0) +
    geom_text(aes(label = paste0(round(percent_children_orphaned))), 
              position = position_stack(vjust = 0.5), size = 3, color = "white") +
    labs(title = "Percentage of Orphaned Children by Countries",
         fill = "Country Name", x = "", y = "Percentage (%)") +
    theme_void() +
    theme_bw() +
    theme(plot.title = element_text(size = 14, face = "bold"), 
          axis.title.x.bottom = element_text(size = 12, face = "bold", color = "gray15"))
p16
ggsave("6_Percentage_Orphaned_Children_HIV.png", p16)

p17 <- ggplot(df2, aes(x = country_name, y = percent_incidence_per_1000, fill = country_name)) +
    geom_bar(stat = "identity", width = 1, color = "white", alpha = 0.8) +
    geom_text(aes(label = paste0(round(percent_incidence_per_1000, 1), "%")), 
              vjust = -0.5, size = 2.5) +
    labs(title = "Percentage of HIV Incidence(per 1000) by Country",
         fill = "Country Name", x = "", y = "Percentage (%)") +
    theme_bw() +
    theme(plot.title = element_text(size = 16, face = "bold"),
          axis.title.y = element_text(size = 14, face = "bold", color = "gray15"),
          axis.text.x = element_text(angle = 45, size = 10, hjust = 1))
p17
ggsave("7_Percentage_HIV_Incidence_per_1000.png", p17)

p18 <- ggplot(df2, aes(x = "", y = percent_pregnant_women_need_ART, fill = country_name)) +
    geom_bar(stat = "identity", width = 1, color = "white", alpha = 0.8) +
    coord_polar("y", start = 0) +
    geom_text(aes(label = paste0(round(percent_pregnant_women_need_ART))), 
              position = position_stack(vjust = 0.5), size = 3, color = "white") +
    labs(title = "Percentage of Women Needing ART by Countries",
         fill = "Country Name", x = "", y = "Percentage (%)") +
    theme_void() +
    theme_bw() +
    theme(plot.title = element_text(size = 14, face = "bold"), 
          axis.title.x.bottom = element_text(size = 12, face = "bold", color = "gray15"))
p18
ggsave("8_Percentage_Women_Needing_ART.png", p18)

HIV_data2 <- read.csv("Table2.csv")
HIV_data2 <- clean_names(HIV_data2)
HIV_data2 <- HIV_data2 %>% rename(people_living_hiv = estimated_number_of_people_living_with_hiv,
                                         death_due_to_hiv = estimated_number_of_deaths_due_to_hiv,
                                         women_living_hiv = estimated_number_of_women_15_living_with_hiv)
colnames(HIV_data2)
df3 <- HIV_data2 %>% group_by(year) %>% 
    summarise(population = sum(population),
              people_living_hiv = sum(people_living_hiv),
              gdp_per_capita = sum(gdp_per_capita),
              death_due_to_hiv = sum(death_due_to_hiv),
              women_living_hiv = sum(women_living_hiv),
              evg_gdp = mean(gdp_per_capita),
              evg_unemployment_rate = mean(unemployment_rate)) %>% 
    as.data.frame()
class(df3$people_living_hiv)

df4 <- df3 %>% mutate(percent_living_hiv = (people_living_hiv / population) * 100,
              percent_death_hiv = (death_due_to_hiv / population) * 100,
              percent_women_living_hiv = (women_living_hiv / population) * 100)
colnames(df4)

# Population
p21 <- ggplot(df4, aes(x = factor(year), y = population, group = 1)) +
    geom_bar(stat = "identity", fill = "cadetblue2", color = "white", alpha = 0.8) +
    geom_text(aes(label = paste0(round(population / 1000000), "M")), 
              position = position_stack(vjust = 1.03), size = 3, color = "black") +
    labs(title = "Population Trend Across Year", x = "Year", y = "Population") +
    theme_bw() +
    theme(plot.title = element_text(size = 16, face = "bold"),
          axis.title.x = element_text(size = 14, face = "bold", color = "gray15"),
          axis.title.y = element_text(size = 14, face = "bold", color = "gray15"),
          axis.text.x = element_text(size = 10), 
          axis.text.y = element_text(size = 10)) +
    ylim(0, 560000000)
p21
ggsave("Population_Trend_Across_Year.png", p21)

p22 <- ggplot(df4, aes(x = factor(year), y = percent_living_hiv, group = 1)) +
    geom_bar(stat = "identity", fill = "peachpuff", color = "white") +
    geom_text(aes(label = paste0(round(percent_living_hiv, 2), "%")), 
              vjust = -0.5, size = 2.5) +
    labs(title = "Proportion of People Living with HIV in Total Population", 
         x = "Year", y = "Population") +
    theme_bw() +
    theme(plot.title = element_text(size = 16, face = "bold"),
          axis.title.x = element_text(size = 14, face = "bold", color = "gray15"),
          axis.title.y = element_text(size = 14, face = "bold", color = "gray15"),
          axis.text.x = element_text(size = 10), 
          axis.text.y = element_text(size = 10))
p22
ggsave("Proportion_People_Living_with_HIV.png", p22)


p23 <- ggplot(df4, aes(x = factor(year), y = percent_death_hiv, group = 1)) +
    geom_bar(stat = "identity", fill = "lightsteelblue1", color = "white") +
    geom_text(aes(label = paste0(round(percent_death_hiv, 2), "%")), 
              vjust = -0.5, size = 2.5) +
    labs(title = "Proportion of Deaths Due to HIV in Total Population", x = "Year", y = "Population") +
    theme_bw() +
    theme(plot.title = element_text(size = 16, face = "bold"),
          axis.title.x = element_text(size = 14, face = "bold", color = "gray15"),
          axis.title.y = element_text(size = 14, face = "bold", color = "gray15"),
          axis.text.x = element_text(size = 10), 
          axis.text.y = element_text(size = 10))
p23
ggsave("Proportion_Deaths_Due_to_HIV.png", p23)

p24 <- ggplot(df4, aes(x = factor(year), y = percent_women_living_hiv, group = 1)) +
    geom_bar(stat = "identity", fill = "lightpink", color = "white", alpha = 0.8) +
    geom_text(aes(label = paste0(round(percent_women_living_hiv, 2), "%")), 
              vjust = -0.5, size = 2.5) +
    labs(title = "Proportion of Women Living with HIV in Total Population", x = "Year", y = "Population") +
    theme_bw() +
    theme(plot.title = element_text(size = 16, face = "bold"),
          axis.title.x = element_text(size = 14, face = "bold", color = "gray15"),
          axis.title.y = element_text(size = 14, face = "bold", color = "gray15"),
          axis.text.x = element_text(size = 10), 
          axis.text.y = element_text(size = 10)) 
p24
ggsave("Proportion_Women_Living_with_HIV.png", p24)

p25 <- ggplot(df3, aes(x = factor(year), y = evg_gdp, group = 1)) +
    geom_bar(stat = "identity", fill = "skyblue", color = "white", alpha = 0.8) +
    geom_text(aes(label = paste0(round(evg_gdp), "$")), 
              vjust = -0.5, size = 2.5) +
    labs(title = "Average GDP of Western Africa Region", x = "Year", y = "Average GDP per Capita") +
    theme_bw() +
    theme(plot.title = element_text(size = 16, face = "bold"),
          axis.title.x = element_text(size = 14, face = "bold", color = "gray15"),
          axis.title.y = element_text(size = 14, face = "bold", color = "gray15"),
          axis.text.x = element_text(size = 10), 
          axis.text.y = element_text(size = 10)) 
p25
ggsave("05_Avg_GDP.png",p25)

p26 <- ggplot(df3, aes(x = factor(year), y = evg_unemployment_rate, group = 1)) +
    geom_line(color = "royalblue", linewidth = 2) +
    geom_point(size = 3, color = "royalblue4") +
    labs(title = "Average Unemployment Rate of Western Africa Region",
         x = "Year",
         y = "Average Unemployment Rate") +
    theme_bw() +
    theme(plot.title = element_text(size = 16, face = "bold"),
          axis.title.x = element_text(size = 14, face = "bold", color = "gray15"),
          axis.title.y = element_text(size = 14, face = "bold", color = "gray15"),
          axis.text.x = element_text(size = 10), 
          axis.text.y = element_text(size = 10))
ggplotly(p26)
ggsave("Avg_unemployment_rate.png", p26)

# spline curve
ggplot(df, aes(x = year, y = people_living_hiv, group = 1)) +
    geom_point(size = 2, color = "darkred") +
    geom_smooth(method = "lm", formula = y ~ ns(x, 2), se = FALSE, color = "darkgreen", size = 1.2) +
    labs(
        title = "Smooth Trend of People Living with HIV (2007-2010)",
        x = "Year",
        y = "People Living with HIV"
    ) +
    theme_minimal()
# polar plot
ggplot(df, aes(x = factor(year), y = people_living_hiv, group = 1)) +
    geom_bar(stat = "identity", fill = "skyblue", alpha = 0.8) +
    coord_polar() +
    labs(title = "Trend of People Living with HIV (2007-2010)",
         x = "Year",
         y = "People Living with HIV") +
    theme_minimal()
# step plot
ggplot(df, aes(x = year, y = people_living_hiv, group = 1)) +
    geom_step(color = "blue", size = 1.2) +
    geom_point(size = 2, color = "red") +
    labs(
        title = "Step Trend of People Living with HIV (2007-2010)",
        x = "Year",
        y = "People Living with HIV"
    ) +
    theme_minimal()
# lollipop plot
ggplot(df, aes(x = year, y = people_living_hiv, group = 1)) +
    geom_segment(aes(xend = year, yend = 0), color = "lightblue", size = 1) +
    geom_point(color = "blue", size = 4) +
    labs(title = "Lollipop Trend of People Living with HIV (2007-2010)",
         x = "Year",
         y = "People Living with HIV") +
    theme_minimal()



ggsave("Trend_People_Living_with_HIV.png", plot1)
ggsave("Trend_Deaths_due_to_HIV.png", plot2)

print(plot1)

    
    
