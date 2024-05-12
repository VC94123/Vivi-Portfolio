#A2 Team Assignment - Moneyball Capstone Project (Story of Oakland Athletics)


getwd()
setwd("/Users/vivi/Desktop/R Programing /A2")
library(dplyr)
library(tidyr)
library(ggplot2)

# Load the Batting ＆Salaries data from a CSV file into a dataframe
batting <- read.csv("Batting.csv")
salary <- read.csv("Salaries.csv")

# Filter the batting data for the years 1994 to 2001 and for players with more than 0 At-Bats (AB)
batting_1994_2001 <- batting %>% filter(yearID >= 1994 & yearID <= 2001, AB > 0)

# Replace NA values in HBP (Hit By Pitch) and SF (Sacrifice Fly) with 0
batting_1994_2001 <- batting_1994_2001 %>%
  mutate(
    HBP = replace_na(HBP, 0),
    SF = replace_na(SF, 0)
  )

# Calculate batting average (AVG), on-base percentage (OBP), and slugging percentage (SLG) for players
batting_1994_2001 <- batting_1994_2001 %>%
  mutate(
    AVG = H / AB,
    OBP = (H + BB + HBP) / (AB + BB + HBP + SF),
    SLG = (H + 2*`X2B` + 3*`X3B` + 4*HR) / AB
  )

# Filter the salary data for the years 1994 to 2001
salaries_1994_2001 <- salary %>% filter(yearID >= 1994 & yearID <= 2001)

# Merge the filtered batting data with the filtered salary data based on playerID and yearID
baseball_data <- merge(batting_1994_2001, salaries_1994_2001, by = c("playerID", "yearID","teamID","lgID"))

# Specify the playerIDs for the players to be replaced
gone_players <- c("giambja01", "damonjo01", "saenzol01")
gone_players.df <- subset(baseball_data, yearID == 2001 & playerID %in% gone_players) 
gone_players.df

# Filter the baseball_data by year, to show only the 2001's players
players_2001.df<- subset(baseball_data, yearID == 2001)
players_2001.df

# Replacement giambja01
replacement1 <- players_2001.df %>% filter(teamID != "OAK" & H >100 & G > 100 & AVG >0.25 & OBP >0.4 & SLG > 0.1 & salary < 4103333)
replacement1 # we will buy berkmla01

# Replacement damonjo01
replacement2 <- players_2001.df %>% filter(teamID != "OAK" & H >100 & G > 160 & AVG >0.25 & OBP >0.32 & SLG > 0.43 & salary < 7100000)
replacement2 # we will buy pujolal01

# Replacement saenzol01
replacement3 <- players_2001.df %>% filter(teamID != "OAK" & H >100 & G > 160 & AVG >0.21 & OBP >0.29 & SLG > 0.48 & salary < 290000)
replacement3 # we will buy drewjd01

# Create a data frame with the 3 new players and their stats
player_ids <- c("berkmla01", "pujolal01", "drewjd01") 
new_players_df <- subset(players_2001.df, playerID %in% player_ids)
print(new_players_df)



# Creating Visualizations
# Marking gone_players and their replacements
ggplot(players_2001.df, aes(x = OBP, y = SLG)) +
  geom_point(data = subset(players_2001.df, !playerID %in% gone_players), color = "grey") +
  geom_point(data = subset(players_2001.df, playerID == "berkmla01") , color = "green", size = 4) +
  geom_point(data = subset(players_2001.df, playerID == "pujolal01"), color = "blue", size = 4) +
  geom_point(data = subset(players_2001.df, playerID == "drewjd01") , color = "red", size = 4) +
  geom_point(data = gone_players.df, color = "black", shape = 17, size = 5) +
  theme_minimal() +
  labs(title = "Identifying Replacement Players for 2001",
       x = "On-Base Percentage (OBP)", y = "Slugging Percentage (SLG)",
       caption = "Black stars: Players to replace; Colored points: Potential replacements")

# This plot identifies the position of leaving players(dark triangles) and our selected replacements(colored circles) based on Slugging Percentage and On-Base Percentage. 
# We can see that most of our chosen replacements are good candidates because they show strong performance, just as much than the leaving players. 
# But also at the same time, those players are overlooked by the market and have a low market value.


# Create a bar chart where the height of each bar corresponds to the 'salary' value.
# Set up by Group and create the data frame
Group1 <- players_2001.df %>%
  filter(playerID %in% c("berkmla01", "giambja01")) %>%
  select(playerID, salary)

Group2 <- players_2001.df %>%
  filter(playerID %in% c("pujolal01", "damonjo01")) %>%
  select(playerID, salary)

Group3 <- players_2001.df %>%
  filter(playerID %in% c("drewjd01", "saenzol01")) %>%
  select(playerID, salary)

# Add a 'Group' column to each data frame
Group1$Group <- "Group 1"
Group2$Group <- "Group 2"
Group3$Group <- "Group 3"

# Combine the groups
all_groups <- rbind(Group1, Group2, Group3)

# Bars for different 'playerID's within the same 'Group' are placed next to each other for easy comparison.
ggplot(all_groups, aes(x = Group, y = salary, fill = playerID)) +
  geom_bar(stat = "identity", position = "dodge") +  # Use stat to use the values we want, and set up the position 
  labs(title = "Salary Comparison by Group",
       x = "Group",
       y = "Salary in million($)") +
  theme_minimal() +
  scale_y_continuous(labels = function(x) format(x / 10^6, scientific = FALSE)) + # Make the labels on the chart easier to read and understand by avoiding the representation of numbers in scientific notation 
  facet_wrap(~ Group, scales = "free_x")  # Used with scales = "free_x" to create separate panels for each group, with each panel only showing the x-axis labels (player IDs) that are relevant to that group

# The second plot shows the difference in the monetary value of the leaving versus our candidate replacements. 
# Indeed we made our candidate come at market value below their performance which is very attractive to our team. 
# Indeed the replacements of Jason Giambi and  Johnny Damon saved us millions which we could use to find a third replacement with more experience but still with high performance. 
# Overall, we were able to save well above $8.24 million and welcome great-performing players.



# Insights: 

# The storyline of the Oakland Athletics unfolds during the 2001-2002 season, with the assumption that the Group 2 is leading as General Managers during this specified timeline. 
# The filter was applied to the seasons spanning from 1994 to 2001 in order to focus on the last five years within this timeframe. Year 2001 was chosen as a crucial specific time frame for the analysis, 
# because the objective is to examine data to identify suitable replacements for the essential players the team lost during the 2001–02 offseason. 

# The decision to address missing values (NA) in the HBP (Hit By Pitch) and SF (Sacrifice Fly) columns was motivated by the distinctive nature of these statistics in baseball data and their crucial importance in calculations such as On-Base Percentage (OBP) and Slugging Percentage (SLG). 
# Making these adjustments is essential for maintaining the reliability and precision of performance metrics, which are key to evaluating players and informing strategic decisions. 
# Furthermore, players with no at-bats (AB) were removed to prevent division by zero errors when calculating AVG (Batting Average), a process that can be efficiently accomplished through a simple filtering operation.
# After analyzing the statistics and salaries of the three traded players, their data were compared with those of their counterparts from the 2001 season. 
# These chosen individuals were not only young and motivated, as reflected by their performance metrics, but also represented astute financial decisions for the Oakland Athletics. 
# This strategy illustrated that the team could secure skilled players without incurring excessive costs. 
# Additionally, this approach provided the players with a platform to boost their confidence and carve out their niches in the professional realm, considering their relatively nascent careers. 
# In sum, the Oakland Athletics managed to save $8,240,000 while still securing quality talent.
# Lance Berkman, who was 25 years old at the time, was selected for the 2001 National League All-Star team and chosen as a replacement for Jason Giambi. 
# Among all the players, he boasted superior batting statistics while earning a salary of only $305,000, enabling the Oakland Athletics to save $3,798,333.

# J.D. Drew, who was 24 years old at the time and had won the Golden Spikes Trophy in 1997, was selected to replace Olmedo Sáenz. 
# Despite being an underrated player, Drew commanded a salary of $2,750,000, which was $2,460,000 higher than Sáenz's. 
# However, given his potential and impressive statistics, the General Managers deemed him a worthy choice.
 
# Albert Pujols, who was 21 years old at the time and had won the Rookie of the Year award in 2001, was selected to replace Johnny Damon. 
# As the most cost-effective option with outstanding batting statistics, he enabled a savings of $6,900,000.

# The General Managers did not base their decisions on the age of the players; rather, the players selected emerged as the most suitable through the analysis. 
# Additionally, the search for players with lower salaries was driven by budgetary constraints, indicating a strategic approach to building the team within financial limitations.




# References:
# Baseball-Reference.com. (n.d.). Lance Berkman. Retrieved February 6, 2024, from https://www.baseball-reference.com/players/b/berkmla01.shtml
# Leith, W. (2020, April 18). Looking back at the career of J.D. Drew. MLB.com. Retrieved February 6, 2024, from https://www.mlb.com/news/looking-back-at-career-of-j-d-drew
# The world looks very different since Albert Pujols’ first home run in 2001. (2022, September 24). ESPN.com. https://www.espn.com/mlb/story/_/id/34621070/albert-pujols-700-home-runs-sports-tech-culture-rewind


