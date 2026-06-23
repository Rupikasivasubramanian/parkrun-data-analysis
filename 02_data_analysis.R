# execute data cleaning script to fetch all required dataframes
source("01_data_cleaning.R")

# add required libraries
library(ggplot2)
library(ggrepel)
library(geosphere)
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)
library(kableExtra)
library(scales)
library(mapview)
library(htmlwidgets)

# Define a custom palette
# parkrun_palette <- c(
#   "#003f5c",
#   "#2f4b7c",
#   "#665191",
#   "#a05195",
#   "#d45087",
#   "#f95d6a",
#   "#ff7c43",
#   "#ffa600"
# )

parkrun_palette <- c("#264653", "#2A9D8F", "#E9C46A", "#F4A261",
  "#E76F51", "#8AB17D", "#577590", "#A8DADC")

# Custom theme function
theme_custom <- function(base_size = 10, base_family = "Helvetica") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 2, hjust = 0.5, color = "#333333"),
      plot.subtitle = element_text(size = base_size, hjust = 0.5, color = "#555555"),
      plot.caption = element_text(size = base_size - 2, color = "#666666", hjust = 1),
      axis.title = element_text(face = "bold", color = "#333333"),
      axis.text = element_text(color = "#444444"),
      panel.grid.major = element_line(color = "#949699", linewidth = 0.3 , linetype = "dotted"),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f2f5f7", color = NA),
      panel.background = element_rect(fill = "#f2f5f7", color = NA),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      legend.background = element_blank(),
      strip.background = element_rect(fill = "#E6E6E6", color = NA),
      strip.text = element_text(face = "bold", size = base_size)
    )
}

# scale functions for discrete and continuous use
scale_color_parkrun <- function() {
  scale_color_manual(values = parkrun_palette)
}

scale_fill_parkrun <- function() {
  scale_fill_manual(values = parkrun_palette)
}

###---------------------------- q1 ----------------------------###

# q1 Which parkrun provides the best marketing opportunity 
# with the largest number of people ?

# calculating unique runners per parkrun
unique_runners <- final_df %>%
  group_by(parkrun) %>%
  summarise(unique_runners = n_distinct(id))

# plot a bar chart of unique parkrunners per parkrun
# tiff(file="plots/q1_barplot_unique_runners.tiff",
#      width=7, height=5, units="in", res=300)
q1_total_unique_runners <- ggplot(unique_runners, 
       aes(x = reorder(parkrun, unique_runners), y = unique_runners)) +
  geom_bar(aes(fill = parkrun), 
           stat = "identity",
           show.legend = FALSE) +
  geom_text(aes(label = unique_runners),
            # place label slightly above the bar
            vjust = -0.3,          
            size = 3,
            # visible over dark fills
            color = "#444444",       
            fontface = "bold") +
  labs(
    title = "Unique runners in each parkrun",
    x = "Parkrun",
    y = "Unique Runners"
  ) +
  scale_fill_parkrun() +
  theme_custom()
q1_total_unique_runners
# dev.off()

#calculating weekly runners per parkrun per week
week_trend <- final_df %>%
  group_by(parkrun, week) %>%
  summarise(unique_runners_per_week = n_distinct(id),
            .groups = "drop")

# plot a line chart showing weekly trend of participants by parkrun
# tiff(file="plots/q1_lineplot_weekly_participants_trend.tiff",
#      width=7, height=5, units="in", res=300)
q1_weekly_runners_trend <- ggplot(week_trend, 
       aes(x = week, y = unique_runners_per_week,
           color = parkrun, group = parkrun)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Weekly participants trend by parkrun", 
       x = "Week", 
       y = "Unique Runners",
       color = "Parkrun") +
  scale_color_parkrun() +
  theme_custom()
q1_weekly_runners_trend
# dev.off()

# weekly average
avg_weekly <- week_trend %>%
  group_by(parkrun) %>%
  summarise(weekly_average = round(mean(unique_runners_per_week),0), 
            .groups = "drop")

# create a summary table which contains parkrun and weekly stats
q1_summary_table <- unique_runners %>%
  left_join(avg_weekly, by = "parkrun")

# format summary table with kable styling
q1_summary_table %>% 
kbl(caption = "Unique and Average Weekly Runners per Parkrun" ,
    col.names = c("Parkrun" , "Unique Runner" , "Weekly Average Runner")) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"), full_width = FALSE)

###---------------------------- q2 ----------------------------###
#2.Do new people come each week or is it always the same people ?
q2 <- final_df %>%
  arrange(parkrun, week, id) %>%
  group_by(parkrun, id) %>%
  mutate(
    first_week = min(week, na.rm = TRUE),
    runner_tag = if_else(week == first_week, 
                         "New",
                         "Returning")
    ) %>%
  ungroup()

# computing new and returning runners
summary_table_new_runners <- q2 %>%
  group_by(parkrun, week) %>%
  summarise(
    new_runners = n_distinct(id[runner_tag == "New"]),
    returning_runners = n_distinct(id[runner_tag == "Returning"]),
    total_runners = n_distinct(id),
    percentage_new_runners = round(new_runners / total_runners * 100,1),
    percentage_returning_runners = round(returning_runners / total_runners * 100 , 1 )
  ) %>%
  ungroup() %>%
  arrange(parkrun, week)

# computing new and returning runners for all parkruns 
q2_summary_table <- summary_table_new_runners %>% 
  group_by(parkrun) %>% 
  summarise(
    total_new = sum(new_runners),
    total_returning = sum(returning_runners),
    total_runners = sum(total_runners),
    new_pct = round(100*sum(new_runners)/sum(total_runners) , 2),
    returning_pct = round(100*sum(returning_runners)/sum(total_runners) , 2)
  )

# format summary table with kable styling
summary_table_new_runners %>% 
  kbl(caption = "Summary Table detailing New and Returning Runners",
      col.names = c("Parkrun" , "Week" , "New Runner" ,
                    "Returning Runner" , "Total Runner" ,
                    "New Runner Percentage" , "Returning Runner Percentage")) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"), full_width = FALSE)

# plot grouped barplot of new vs returning runners
# tiff(file="plots/q2_barplot_new_vs_returning_runners.tiff",
#      width=7, height=5, units="in", res=300)
q2_new_vs_returning <- ggplot(q2_summary_table %>%
         pivot_longer(
           cols = c(total_new , total_returning),
           names_to = "runner_type",
           values_to = "count"), 
       aes(x = reorder(parkrun , count),
           y = count,
           fill = runner_type)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(aes(label = count),
            position = position_dodge(width = 0.9),
            vjust = -0.3,
            size = 3.5,
            fontface = "bold",
            color = "#444444") +
  labs(
    title = "New vs Returning Runners by parkrun",
    x = "Parkrun",
    y = "Count",
    fill = "Type"
  ) +
  scale_fill_manual(values = c(parkrun_palette[1] , parkrun_palette[2]),
                    labels = c("New Runners", "Returning Runners"))+
  theme_custom()
q2_new_vs_returning
# dev.off()

# line plot of new runners over weeks by parkrun
# tiff(file="plots/q2_linechart_proportion_of_new_runners.tiff",
#      width=7, height=5, units="in", res=300)
q2_prop_new_runners <- ggplot(summary_table_new_runners, 
       aes(x = week, y = percentage_new_runners, 
           color = parkrun, group = parkrun)) +
  geom_line(size = 1) +
  geom_point() +  
  labs(
    title = "Varying proportion of New Runners over time",
    x = "Week",
    y = "Percentage",
    color = "Parkrun"
  ) +
  scale_color_parkrun() +
  theme_custom()
q2_prop_new_runners
# dev.off()

# extracting age group from final_df
# removed records where age is NOT AVAILABLE
q2 <- q2 %>%
  filter(age != "Not Available") %>%  
  mutate(
    age_lower = as.numeric(sub("-.*", "", age)),  
    age_group = case_when(
      age_lower <= 19 ~ "Junior",
      age_lower >= 20 & age_lower <= 44 ~ "Senior",
      age_lower > 44 ~ "Veteran",
      TRUE ~ NA_character_
    )
  )

# age distribution among new runners
new_runners <- q2 %>%
  filter(runner_tag == "New")

# compute number of new runners based on age group
age_group_new_runners <- new_runners %>%
  group_by(parkrun, age_group) %>%
  summarise(unique_runners_new = n_distinct(id), 
            .groups = "drop")

# plot a barplot of the new runners based on age group
# tiff(file="plots/q2_barplot_new_runners_by_age_group.tiff",
#      width=7, height=5, units="in", res=300)
q2_new_runners_by_age <- ggplot(age_group_new_runners, 
       aes(x = reorder(parkrun , unique_runners_new),
           y = unique_runners_new,
           fill = age_group)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = unique_runners_new), 
            position = position_dodge(width = 0.9),
            vjust = -0.3,
            size = 3,
            fontface = "bold",
            color = "#444444")  +
  labs(title = "New Runners by Age Group",
       x = "Parkrun", 
       y = "Count",
       fill = "Age Group") +
  scale_fill_parkrun() +
  theme_custom()
q2_new_runners_by_age
# dev.off()

# gender distribution
gender_new_runners <- new_runners %>%
  group_by(parkrun, sex) %>%
  summarise(unique_runners_new = n_distinct(id), 
            .groups = "drop")

# barplot of new runners based on gender
# tiff(file="plots/q2_barplot_new_runners_by_gender.tiff",
#      width=7, height=5, units="in", res=300)
q2_new_runners_by_sex <- ggplot(gender_new_runners,
       aes(x = reorder(parkrun , unique_runners_new)
           , y = unique_runners_new, fill = sex)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = unique_runners_new), 
            position = position_dodge(width = 0.9),
            vjust = -0.3,
            size = 3,
            fontface = "bold",
            color = "#444444")  +
  labs(title = "New Runners by Sex",
       x = "Parkrun", 
       y = "Count",
       fill = "Sex") +
  scale_fill_parkrun() +  
  theme_custom()
q2_new_runners_by_sex
# dev.off()

# stacked based on club association of new runners

# assign a binary value based whether new runner associated with club or not
q2_new_ruuners_by_club_asso <- q2 %>% 
  filter(runner_tag == "New") %>% 
  mutate(
    club_association = ifelse(
      club != "Not Available" , 1 , 0 
    )
  )

# converting club_association variable to factor
q2_new_ruuners_by_club_asso$club_association <- as.factor(q2_new_ruuners_by_club_asso$club_association)

# visualisation
# tiff(file="plots/q2_barplot_new_runners_by_club.tiff",
#      width=7, height=5, units="in", res=300)
q2_new_ruuners_by_club_asso_plot <- ggplot(
  q2_new_ruuners_by_club_asso %>%
    group_by(parkrun, club_association) %>%
    summarise(club_association_cnt = n(), .groups = "drop") %>%
    group_by(parkrun) %>%
    mutate(
      total = sum(club_association_cnt),
      pct   = club_association_cnt / total
    ),
  aes(x = reorder(parkrun, -total), y = pct, fill = club_association)
) +
  geom_col() +
  geom_text(
    aes(label = percent(pct, accuracy = 1)),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 3
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(
    name = "Club Association",
    values = c("0" = parkrun_palette[1], "1" = parkrun_palette[2]),
    labels = c("0" = "No", "1" = "Yes")
  ) +
  labs(
    title = "Proportion of New runners based on club association",
    x = "Parkrun",
    y = "Percentage of Runners"
  ) +
  theme_custom()
q2_new_ruuners_by_club_asso_plot
# dev.off()

###---------------------------- q3 ----------------------------###

#3. Do people move between Parkruns ? 
# Does this mean if you advertise at fewer places you eventually
#see people from other locations ?

# Let's find the number of parkrunners who ran in single or multiple runs
parkrunner_runs_count <- final_df %>%
  group_by(id) %>%
  summarise(parkrun_count =n_distinct(parkrun))
multiple_runners_count <- parkrunner_runs_count %>%
  filter(parkrun_count > 1) %>%
  summarise(count = n())
single_runners_count <- parkrunner_runs_count %>%
  filter(parkrun_count == 1) %>%
  summarise(count = n())
total_runners <- nrow(parkrunner_runs_count)
percentage_single <- round(single_runners_count$count / total_runners * 100, 1)
percentage_multi <- round(multiple_runners_count$count / total_runners * 100, 1)
print(paste("Total no of parkrunners who participate in one parkrun is",
            single_runners_count,"."))
print(paste("Total no of parkrunners who participate in multiple parkruns is",
            multiple_runners_count,"."))

#Showing distribution of how many parkruns people visit
q3_summary_parkrun_count <- parkrunner_runs_count %>%
  mutate(
    parkrun_count_new = ifelse(parkrun_count >= 4, "4+", parkrun_count)
  ) %>%
  group_by(parkrun_count_new) %>%
  summarise(
    count = n(),
    percentage = round(n() / total_runners * 100, 1)
  ) %>%
  arrange(parkrun_count_new)

# giving the summary table styling
q3_summary_parkrun_count %>% 
kbl(caption = "Distribution of Attended Parkruns",
    col.names = c("Attended Parkrun" , "Number of parkrunners" , "Percentage")) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"), full_width = FALSE)

# Visualisation
# tiff(file="plots/q3_barplot_attendance_runners_across_parkruns.tiff",
#      width=7, height=5, units="in", res=300)
q3_parkruns_attended <- ggplot(summary_parkrun_count,
       aes(x=factor(parkrun_count_new), 
           y = count,
           fill = factor(parkrun_count_new))) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label= count), 
            position = position_dodge(width = 0.9),
            vjust = -0.3,
            size = 3,
            fontface = "bold",
            color = "#444444",
            ) +
  labs(
    title = "Attendance of runners across different parkruns",
    x = "Parkruns Attended",
    y= "Count"
  ) +
  scale_fill_parkrun() +
  theme_custom()
q3_parkruns_attended
# dev.off()

#c reating overlap matrix
# 1. Create a pivot binary matrix of parkrunners vs parkrun
pivot_df <- final_df %>% 
  distinct(id , parkrun) %>% 
  mutate(value = 1) %>% 
  pivot_wider(names_from = parkrun , values_from = value , values_fill = 0)

# 2. Since we need parkrun matrix, it is good to remove id column
parkrun_matrix <- as.matrix(pivot_df[ , -1])

# 3. Overlap counts can be compute using cross product of parkrun matrix 
# and its transpose
overlap <- t(parkrun_matrix) %*% parkrun_matrix
overlap_df <- as.data.frame(as.table(overlap))
colnames(overlap_df) <- c("parkrun_A", "parkrun_B", "overlap_runners")

# 4. Add Unique runners at the parkruns and compute overlap percentage
# Since we need parkrun matrix, it is good to remove id column
parkrun_matrix <- as.matrix(pivot_df[ , -1])

# 3. Overlap counts can be compute using cross product of parkrun matrix 
# and its transpose
overlap <- t(parkrun_matrix) %*% parkrun_matrix
overlap_df <- as.data.frame(as.table(overlap))
colnames(overlap_df) <- c("parkrun_A", "parkrun_B", "overlap_runners")

# 4. Add Unique runners at the parkruns and compute overlap percentage
unique_counts <- final_df %>% 
  distinct(id , parkrun) %>% 
  count(parkrun , name = "unique_runners")

# 5. Join unique runner counts for A and B
overlap_df <- overlap_df %>%
  left_join(unique_counts, by = c("parkrun_A" = "parkrun")) %>%
  rename(unique_runners_A = unique_runners) %>%
  left_join(unique_counts, by = c("parkrun_B" = "parkrun")) %>%
  rename(unique_runners_B = unique_runners)

# 6. Calculate union and mixing %
overlap_df <- overlap_df %>%
  mutate(
    union = unique_runners_A + unique_runners_B - overlap_runners,
    mixing_pct = round((overlap_runners / union) * 100, 2)
  )

# 7. Remove rows with mixing_percentage is 100
overlap_df <- overlap_df %>% 
  filter(mixing_pct != 100)

# Creating heatmap data
heatmap_data <- overlap_df %>%
  bind_rows(
    overlap_df %>%
      rename(parkrun_A = parkrun_B, parkrun_B = parkrun_A)
  )

# plot heatmap
# tiff(file="plots/q3_heatmap_runners_overlap_rate.tiff",
#      width=7, height=7, units="in", res=300)
q3_parkrunners_movement <- ggplot(heatmap_data_upper <- heatmap_data %>%
         filter(parkrun_A != parkrun_B) %>%
         filter(as.numeric(factor(parkrun_B)) < as.numeric(factor(parkrun_A))), 
       aes(x = parkrun_A, y = parkrun_B, 
           fill = mixing_pct)) +
  geom_tile(color = parkrun_palette[7]) +
  geom_text(aes(label = paste0(round(mixing_pct, 2), "%")),
            color = "#444444",
            size = 3,
            fontface = "bold"
            ) +
  scale_fill_gradient(low = "white", high = parkrun_palette[7]) +
  labs(
    title = "Parkrunners overlap rate heatmap",
    x = "Parkrun A",
    y = "Parkrun B",
    fill = "Overlap Percent"
  ) +
  theme_custom() +
  theme(
    axis.text.x = element_text(angle = 90),
    axis.text = element_text(size = 9)
  )
q3_parkrunners_movement
# dev.off()

###---------------------------- q4 ----------------------------###

# q4 If Puma want to target elite runners, 
# where should they focus their efforts?

# Let's start this problem by defining who are elite runners
# Let's have a look on the finish time for each parkrun
# tiff(file="plots/q4_boxplot_parkrun_completion_time.tiff",
#      width=7, height=5, units="in", res=300)
q4_finish_time_boxplot <- ggplot(final_df,
       aes(x = parkrun , y = time , fill = parkrun)) +
  geom_boxplot(show.legend = FALSE) +
  stat_summary(fun="median", geom="text",
               aes(label=after_stat(y)),
               color = "white",
               vjust=-0.5) +
  labs(
    title = "Parkrun completion time",
    x = "Parkrun",
    y = "Time"
  ) +
  scale_fill_parkrun() +
  theme_custom()
q4_finish_time_boxplot
# dev.off()

# summarize the information to check
final_df %>% 
  group_by(parkrun) %>% 
  summarise(median= median(time))

# as we can we different parkruns have different median finish time
# let's compute 10% cutoff time for each parkrun
cutoff_time_df <- final_df %>% 
  group_by(parkrun) %>% 
  summarise(cutoff_time = quantile(time , probs = 0.1))

# Now label elite runners in the dataframe
q4_df <- final_df %>% 
  left_join(cutoff_time_df , by = "parkrun") %>% 
  mutate(elite = ifelse(time <= cutoff_time , 1 , 0))

# create summary table of elite runners
q4_summary <- q4_df %>% 
  group_by(parkrun) %>% 
  summarise(
    total_runners = n(),
    elite_runners = sum(elite)
  )

# weekly stats of elite runners
weekly_stat <- q4_df %>% 
  group_by(parkrun , week) %>% 
  summarise(
    weekly_runners = n_distinct(id),
    weekly_elites = sum(elite),
    .groups = "drop"
  )

# average weekly stats
weekly_avg_stats <- weekly_stat %>% 
  group_by(parkrun) %>% 
  summarise(
    weekly_avg_runners = round(mean(weekly_runners),0),
    weekly_elite_runners = round(mean(weekly_elites),0),
    .groups = "drop"
  )

# final summary table
q4_summary <- q4_summary %>% 
  left_join(weekly_avg_stats , by = "parkrun")

# giving the summary table styling
q4_summary %>% 
  kbl(caption = "Summary of elite parkrunners",
      col.names = c("Parkrun" , "Total runners" , "Elite runners" ,
                    "Average weekly runners" , "Average elite runners")) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"), full_width = FALSE)

# barplot based on total elite runners per parkrun
# tiff(file="plots/q4_barplot_elite_runners.tiff",
#      width=7, height=5, units="in", res=300)
q4_total_elite_runner <- ggplot(q4_summary) +
  geom_col(aes(x = reorder(parkrun, elite_runners), 
               y = elite_runners, fill = parkrun), show.legend = FALSE) +
  geom_text(aes(x = reorder(parkrun, elite_runners), 
                y = elite_runners, label = elite_runners), 
            position = position_dodge(width = 0.9),
            vjust = -0.3,
            fontface = "bold",
            color = "#444444",
  ) +
  labs(title = "Involvement of elite runners",
       x = "Parkrun",
       y = "Count") +
  scale_fill_parkrun() +
  theme_custom()
q4_total_elite_runner
# dev.off()

# grouped bar plot based on gender
# tiff(file="plots/q4_barplot_elite_runners_gender.tiff",
#      width=7, height=5, units="in", res=300)
q4_elite_runner_by_sex <- ggplot(q4_df %>% 
         filter(sex != "Not Available") %>% 
         group_by(parkrun , sex) %>% 
         summarise(elite = sum(elite , na.rm = TRUE) , .groups = "drop") ,
       aes(x = reorder(parkrun , elite), y = elite, fill = sex)
       ) +
  geom_col(position = "dodge") +
  geom_text(aes(label = elite), 
            position = position_dodge(width = 0.9), 
            vjust = -0.3) +
  labs(title = "Elite Runners by Gender",
       x = "Parkrun",
       y = "Count",
       fill = "Sex") +
  scale_fill_parkrun() +
  theme_custom()
q4_elite_runner_by_sex
# dev.off()

# grouped bar plot based on age group
# assign age group
q4_df <- q4_df %>%
  filter(age != "Not Available") %>%  
  mutate(
    age_lower = as.numeric(sub("-.*", "", age)),  
    age_group = case_when(
      age_lower <= 19 ~ "Junior",
      age_lower >= 20 & age_lower <= 44 ~ "Senior",
      age_lower > 44 ~ "Veteran",
      TRUE ~ NA_character_
    )
  )

# visualisation
# tiff(file="plots/q4_barplot_elite_runners_age_group.tiff",
#      width=7, height=5, units="in", res=300)
q4_elite_runner_by_age <- ggplot(q4_df %>% 
         filter(age_group != "Not Available") %>% 
         group_by(parkrun , age_group) %>% 
         summarise(elite = sum(elite , na.rm = TRUE) , .groups = "drop") ,
       aes(x = reorder(parkrun , elite) , y = elite, fill = age_group)
) +
  geom_col(position = "dodge") +
  geom_text(aes(label = elite), 
            position = position_dodge(width = 0.9), 
            vjust = -0.3) +
  labs(title = "Elite runners by Age group",
       x = "Parkrun",
       y = "Count",
       fill = "Age Group") +
  scale_fill_parkrun() +
  theme_custom()
q4_elite_runner_by_age
# dev.off()

# stacked based on club association of elite runners

# assign a binary value based whether elite runner associated with club or not
q4_elite_ruuners_by_club_asso <- q4_df %>% 
  filter(elite == 1) %>% 
  mutate(
    club_association = ifelse(
      club != "Not Available" , 1 , 0 
    )
  )

# converting club_association variable to factor
q4_elite_ruuners_by_club_asso$club_association <- as.factor(q4_elite_ruuners_by_club_asso$club_association)

# visualisation
# tiff(file="plots/q4_barplot_elite_runners_by_club.tiff",
#      width=7, height=5, units="in", res=300)
q4_elite_ruuners_by_club_asso_plot <- ggplot(
  q4_elite_ruuners_by_club_asso %>%
    group_by(parkrun, club_association) %>%
    summarise(club_association_cnt = n(), .groups = "drop") %>%
    group_by(parkrun) %>%
    mutate(
      total = sum(club_association_cnt),
      pct   = club_association_cnt / total
    ),
  aes(x = reorder(parkrun, -total), y = pct, fill = club_association)
) +
  geom_col() +
  geom_text(
    aes(label = percent(pct, accuracy = 1)),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 3
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(
    name = "Club Association",
    values = c("0" = parkrun_palette[1], "1" = parkrun_palette[2]),
    labels = c("0" = "No", "1" = "Yes")
  ) +
  labs(
    title = "Proportion of elite runners based on club association",
    x = "Parkrun",
    y = "Percentage of Runners"
  ) +
  theme_custom()
q4_elite_ruuners_by_club_asso_plot
# dev.off()

###---------------------------- q5 ----------------------------###

# q5 Puma believe more consistent runners have better potential as customers, 
# how many runners are consistent & is consistency different across locations?

# Lets consider parkrunner who attended for more than 5 weeks are consistent
q5_summary_df <- final_df %>% 
  group_by(parkrun , id , sex , age , club) %>% 
  summarise(cnt = n() , .groups = "drop") %>% 
  mutate(
    attended = case_when(
      cnt == 1 ~ "Only 1 week",
      cnt >= 2 & cnt <= 5 ~ "2 - 5 weeks",
      cnt >= 6 ~ "5+ weeks",
      TRUE ~ NA_character_
    ),
    consistent = ifelse(cnt >= 6 , 1 , 0)
  )

# Lets plot a summary table for consistent parkrunnners
q5_summary <- q5_summary_df %>% 
  group_by(parkrun) %>% 
  summarise(
    unique_runners = n_distinct(id),
    consistent_runners = sum(consistent),
    consistent_pct = round((consistent_runners/unique_runners)*100,2)
  )

# barplot based on total consistent runners per parkrun
# tiff(file="plots/q5_barplot_consistent_runners.tiff",
#      width=7, height=5, units="in", res=300)
q5_consistent_runner <- ggplot(q5_summary) +
  geom_col(aes(x = reorder(parkrun, consistent_runners), 
               y = consistent_runners, fill = parkrun), show.legend = FALSE) +
  geom_text(aes(x = reorder(parkrun, consistent_runners), 
                y = consistent_runners, label = consistent_runners),
            vjust = -0.3,
            fontface = "bold",
            color = "#444444") +
  labs(title = "Consistent Runners in parkrun",
       x = "Parkrun",
       y = "Count") +
  scale_fill_parkrun() +
  theme_custom()
q5_consistent_runner
# dev.off()

# grouped bar plot based on gender
# tiff(file="plots/q5_barplot_consistent_runners_gender.tiff",
#      width=7, height=5, units="in", res=300)
q5_consistent_runner_by_sex <- ggplot(q5_summary_df %>% 
         filter(sex != "Not Available") %>% 
         group_by(parkrun , sex) %>% 
         summarise(consistent = sum(consistent , na.rm = TRUE) , .groups = "drop") ,
       aes(x = reorder(parkrun , consistent), y = consistent , fill = sex)
) +
  geom_col(position = "dodge") +
  geom_text(aes(label = consistent), 
            position = position_dodge(width = 0.9), 
            vjust = -0.3) +
  labs(title = "Consistent Runners by Gender",
       x = "Parkrun",
       y = "Count",
       fill = "Sex") +
  scale_fill_parkrun() +
  theme_custom()
q5_consistent_runner_by_sex
# dev.off()

# grouped bar plot based on age group
# assign age group
q5_summary_df <- q5_summary_df %>%
  filter(age != "Not Available") %>%  
  mutate(
    age_lower = as.numeric(sub("-.*", "", age)),  
    age_group = case_when(
      age_lower <= 19 ~ "Junior",
      age_lower >= 20 & age_lower <= 44 ~ "Senior",
      age_lower > 44 ~ "Veteran",
      TRUE ~ NA_character_
    )
  )

# visualisation
# tiff(file="plots/q5_barplot_consistent_runners_age_group.tiff",
#      width=7, height=5, units="in", res=300)
q5_consistent_runner_by_age <- ggplot(q5_summary_df %>% 
         filter(age_group != "Not Available") %>% 
         group_by(parkrun , age_group) %>% 
         summarise(consistent = sum(consistent , na.rm = TRUE) , .groups = "drop") ,
       aes(x = reorder(parkrun , consistent) , y = consistent , fill = age_group)
) +
  geom_col(position = "dodge") +
  geom_text(aes(label = consistent), 
            position = position_dodge(width = 0.9), 
            vjust = -0.3) +
  labs(title = "Consistent Runner by Age group",
       x = "Parkrun",
       y = "Count",
       fill = "Age Group") +
  scale_fill_parkrun() +
  theme_custom()
q5_consistent_runner_by_age
# dev.off()

# convert consistent into factor
q5_summary_df$consistent <- as.factor(q5_summary_df$consistent)

# stacked based on consistent runners vs casual runners per parkrun
# tiff(file="plots/q5_barplot_consistent_vs_casual.tiff",
#      width=7, height=5, units="in", res=300)
q5_consistent_vs_casual_runner <- ggplot(
  q5_summary_df %>%
    group_by(parkrun, consistent) %>%
    summarise(consistent_count = n(), .groups = "drop") %>%
    group_by(parkrun) %>%
    mutate(
      total = sum(consistent_count),
      pct   = consistent_count / total
    ),
  aes(x = reorder(parkrun, -total), y = pct, fill = consistent)
) +
  geom_col() +
  geom_text(
    aes(label = percent(pct, accuracy = 1)),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 3
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(
    name = "Parkrunner Type",
    values = c("0" = parkrun_palette[1], "1" = parkrun_palette[2]),
    labels = c("0" = "Casual", "1" = "Consistent")
  ) +
  labs(
    title = "Proportion of Consistent vs Casual Runners",
    x = "Parkrun",
    y = "Percentage of Runners"
  ) +
  theme_custom()
q5_consistent_vs_casual_runner
# dev.off()

# stacked based on club association of consistent runners

# assign a binary value based whether consistent runner associated with club or not
q5_consistent_ruuners_by_club_asso <- q5_summary_df %>% 
  filter(consistent == 1) %>% 
  mutate(
    club_association = ifelse(
      club != "Not Available" , 1 , 0 
    )
  )

# converting club_association variable to factor
q5_consistent_ruuners_by_club_asso$club_association <- as.factor(
  q5_consistent_ruuners_by_club_asso$club_association
  )

# visualisation
# tiff(file="plots/q5_consistent_ruuners_by_club_asso_plot.tiff",
#      width=7, height=5, units="in", res=300)
q5_consistent_ruuners_by_club_asso_plot <- ggplot(
  q5_consistent_ruuners_by_club_asso %>%
    group_by(parkrun, club_association) %>%
    summarise(club_association_cnt = n(), .groups = "drop") %>%
    group_by(parkrun) %>%
    mutate(
      total = sum(club_association_cnt),
      pct   = club_association_cnt / total
    ),
  aes(x = reorder(parkrun, -total), y = pct, fill = club_association)
) +
  geom_col() +
  geom_text(
    aes(label = percent(pct, accuracy = 1)),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 3
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(
    name = "Club Association",
    values = c("0" = parkrun_palette[1], "1" = parkrun_palette[2]),
    labels = c("0" = "No", "1" = "Yes")
  ) +
  labs(
    title = "Proportion of consistent runners based on club association",
    x = "Parkrun",
    y = "Percentage of Runners"
  ) +
  theme_custom()
q5_consistent_ruuners_by_club_asso_plot
# dev.off()

# histogram of number of weeks attended per runner
# tiff(file="plots/q5_histogram_weeks_attended.tiff",
#      width=7, height=5, units="in", res=300)
q5_histogram_week_attended <- ggplot(q5_summary_df %>% 
         select(parkrun , id , cnt),
       aes(x = cnt)) +
  geom_histogram(binwidth = 1, fill = parkrun_palette[7], color = "white") +
  facet_wrap(~ parkrun, scales = "free") +
  labs(
    title = "Distribution of Weeks Attended by Runner",
    x = "Weeks Attended",
    y = "Count"
  ) +
  theme_custom()
q5_histogram_week_attended
# dev.off()

###---------------------------- q6 ----------------------------###

# q6 Does the distance between Parkrun locations predict the amount of mixing 
# of people between those  locations ?

# We can use overlap matrix created in q3

# Let's add latitude and longitude of each parkrun to compute distance between them
parkrun <- c("hagley" , "broadpark" , "foster" , "halswellquarry" , 
             "pegasus" , "riverlution" , "scarborough")
lat <- c(-43.52841179599572 , -43.4840932283815 , -43.606583960465365 , 
         -43.59901449970973 , -43.31002606196326 , -43.520900356331815 , 
         -43.57282348360055)
long <- c(172.62303004145045 , 172.722225524247 , 172.38726682610874 , 
          172.58014718378124 , 172.6949276629099 , 172.65951743959477 , 
          172.76982662610692)

# combine to create a dataframe for each parkrun with latitude and longitudes
parkrun_dist <- data.frame(parkrun = parkrun,
                           long = long,
                           lat = lat)

# create pairs of different parkruns
parkrun_pairs <- expand.grid(parkrun_A = parkrun_dist$parkrun , 
                             parkrun_B = parkrun_dist$parkrun , 
                             stringsAsFactors = FALSE)

# join parkrun pairs with latitude and longitude df
parkrun_pairs <- parkrun_pairs %>%
  left_join(parkrun_dist, by = c("parkrun_A" = "parkrun")) %>%
  rename(long1 = long, lat1 = lat) %>%
  left_join(parkrun_dist, by = c("parkrun_B" = "parkrun")) %>%
  rename(long2 = long, lat2 = lat)

# Calculate distance in km
parkrun_pairs <- parkrun_pairs %>%
  mutate(distance_km = round(
    distHaversine(
    matrix(c(long1, lat1), ncol = 2),
    matrix(c(long2, lat2), ncol = 2)
  ) / 1000 , 2))
  

# filter out duplicates
parkrun_pairs <- parkrun_pairs %>%
  filter(distance_km != 0)

# Let's join overlap dataframe and parkrun pairs with distance dataframe
# remove duplicate pairs from both dataframes
overlap_df <- overlap_df %>%
  mutate(
    # alphabetically smaller
    min_parkrun = pmin(parkrun_A, parkrun_B),
    # alphabetically larger
    max_parkrun = pmax(parkrun_A, parkrun_B))

overlap_df <- overlap_df %>% 
  distinct(min_parkrun , max_parkrun , .keep_all = TRUE)

parkrun_pairs <- parkrun_pairs %>%
  mutate(
    # alphabetically smaller
    min_parkrun = pmin(parkrun_A, parkrun_B),
    # alphabetically larger
    max_parkrun = pmax(parkrun_A, parkrun_B))

parkrun_pairs <- parkrun_pairs %>% 
  distinct(min_parkrun , max_parkrun , .keep_all = TRUE)

# apply join
q6_summary <- overlap_df %>%
  left_join(select(
    parkrun_pairs , long1 , lat1 , long2 , lat2 , distance_km , min_parkrun , max_parkrun
    ) , by = c("min_parkrun", "max_parkrun"))

# filter columns
q6_summary <- q6_summary %>% 
  rename(long_A = long1,
         long_B = long2,
         lat_A = lat1,
         lat_B = lat2) %>% 
  select(-min_parkrun , -max_parkrun)

# display summary table
q6_summary %>%
  select(parkrun_A , parkrun_B , distance_km , mixing_pct) %>% 
  kbl(caption = "Distance vs mixing rate across parkruns",
      col.names = c("Parkrun A" , "Parkrun B" , "Distance (kms)" , "Mixing Percentage")) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"), full_width = FALSE)

# scatterplot between distance vs mixing rate
# tiff(file="plots/q6_scatterplot_dist_vs_mixing.tiff",
#      width=7, height=5, units="in", res=300)
q6_dist_vs_mixing <- ggplot(q6_summary,
       aes(x = distance_km , y = mixing_pct)) +
  geom_point(size = 2 , color = parkrun_palette[5]) +
  labs(
    title = "Distance vs Mixing Rate across parkruns",
    x = "Distance (kms)",
    y = "Mixing Percentage"
  ) +
  theme_custom()
q6_dist_vs_mixing
# dev.off()

# find correlation between distance vs mixing rate
cor_dist_mix <- cor(q6_summary$mixing_pct , q6_summary$distance_km)

# apply a simple log regression model
model <- lm(mixing_pct ~ log(distance_km), data = q6_summary)
summary(model)

# Scatterplot with log model fit
# tiff(file="plots/q6_scatterplot_model_dist_vs_mixing.tiff",
#      width=7, height=5, units="in", res=300)
q6_dist_vs_mixing_model <- ggplot(q6_summary, aes(x = distance_km, y = mixing_pct)) +
  geom_point(size = 2, color = parkrun_palette[5]) +
  geom_smooth(method = "lm", formula = y ~ log(x),
              se = FALSE , color = parkrun_palette[2], linewidth = 1.2) +
  labs(
    title = "Relationship Between Distance and Mixing Percentage",
    x = "Distance (kms)",
    y = "Mixing Percentage"
  ) +
  theme_custom()
q6_dist_vs_mixing_model
# dev.off()

# Let's show the movement of parkrunner geographically
q6_geo_df <- bind_rows(
  q6_summary %>% 
    top_n(3,mixing_pct),
  q6_summary %>% 
    top_n(-3,mixing_pct)
)

# Create leaflet base map centered on Christchurch
base <- leaflet(q6_geo_df) %>%
  addTiles() %>%
  setView(lng = 172.60614082219382, lat = -43.45113989999124, zoom = 11) %>% 
  
  # Add parkrun markers with labels
  addMarkers(
    ~parkrun_dist$long, ~parkrun_dist$lat, 
    label = ~parkrun_dist$parkrun,
    labelOptions = labelOptions(
      noHide = TRUE, textOnly = TRUE, direction = "bottom",
      style = list(
        "font-weight" = "bold",
        "font-size" = "12px",
        "color" = "white",
        "text-shadow" = "1px 1px 2px black"
      )
    ),
    icon = icons(
      iconUrl = "https://maps.google.com/mapfiles/ms/icons/blue-dot.png",
      iconWidth = 30, iconHeight = 30
    )
  )

# Add connecting lines between parkruns
for (i in seq_len(nrow(q6_geo_df))) {
  base <- addPolylines(
    map = base,
    lng = c(q6_geo_df$long_A[i], q6_geo_df$long_B[i]),
    lat = c(q6_geo_df$lat_A[i], q6_geo_df$lat_B[i]),
    weight = q6_geo_df$mixing_pct[i] / 3,
    color = parkrun_palette[5],
    opacity = 0.6,
    label = paste0(
      q6_geo_df$parkrun_A[i], " ↔ ", q6_geo_df$parkrun_B[i], ": ",
      round(q6_geo_df$mixing_pct[i], 1), "%"
    )
  )
}

# Add mid-line labels showing mixing percentage
base <- base %>%
  addLabelOnlyMarkers(
    data = q6_geo_df %>% mutate(
      mid_long = (long_A + long_B) / 2,
      mid_lat  = (lat_A + lat_B) / 2
    ),
    lng = ~mid_long,
    lat = ~mid_lat,
    label = ~paste0(round(mixing_pct, 1), "%"),
    labelOptions = labelOptions(
      noHide = TRUE, textOnly = TRUE, direction = "auto",
      style = list(
        "color" = "white",
        "font-size" = "10px",
        "font-weight" = "bold",
        "background-color" = "rgba(0, 0, 0, 0.7)",
        "border" = "1px solid red",
        "border-radius" = "4px",
        "padding" = "2px 4px",
        "text-shadow" = "1px 1px 2px black"
      )
    )
  ) %>%
  
  # Add dark, label-free basemap
  addProviderTiles("CartoDB.DarkMatterNoLabels")

# Save the map as an HTML widget and save as png using third party app
saveWidget(base, file = "parkruns_mapview.html")