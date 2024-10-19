data(Teams)
head(Teams)
str(Teams)
summary(Teams)

#1 How many rows are there?
nrow(Teams)

#2 How many columns are there?
ncol(Teams)

#3 What do you think each row represents?
head(Teams)
#Solution- Each row in the Teams dataset shows the performance of one baseball team 
#for a particular year. It includes information like how many games they won, 
#how many runs they scored, and other stats from that season

#4)What does the H column in row 2,182 represent, in words?
#Solution- The H column in the Teams dataset typically represents the number of hits a 
#team made during a particular season. In row 2,182, the value in the H column tells 
#you how many hits that specific team achieved in that season


#Lab Activity 4
Teams_2022 <- Teams %>% filter(yearID == 2022)
head(Teams_2022)
#1 The column structure will be the same as before, but now you'll only see data 
#from the 2022 year. Yes, it’s different from before because the original Teams 
#dataset contains multiple seasons of team data, while the new data frame 
#Teams_2022 only has data for the year 2022. 
nrow(Teams_2022)
#the new data frame should have 30 rows (one for each team).If you get 30 rows, 
#this is expected, as MLB had 30 teams in the 2022 season

#2. Can you describe what the code `teams_2022 <- teams %>% filter(yearID == 2022, lgID == "NL")` would produce? 
#If you can't intuitively, try running it yourself.
Teams_2022 <- teams %>% filter(yearID == 2022, lgID == "NL")
#A data frame with teams that played in the National League during the 2022 season.
#Since there are 15 teams in the National League, this new data frame should have 15 rows.

#Lab Activity 5
# Sort by wins in descending order
top_teams_2022 <- Teams_2022 %>% arrange(desc(W))

# View the top 6 teams with the most wins
head(top_teams_2022)

teams_2022 %>% 
  select(yearID, teamID, W:L, R:HR) %>% 
  arrange(desc(X3B))

#2 
most_strikeouts_2022 <- Teams_2022 %>%
  arrange(desc(SO)) %>%
  slice(1)  # Select the top row (team with most strikeouts)

# View the result
most_strikeouts_2022

#3 Does anyone have another record they'd like to look up using `arrange()`?
top_wins_2022 <- Teams_2022 %>% arrange(desc(W)) %>% slice(1)

#activity 7
Teams %>% # Take the teams data, THEN
  filter(yearID %in% c(1960:2022)) %>%  # Get only the years 1960-2022, THEN
  # (We could've used >= and <=, but this is more compact. Can you interpret this code?)
  
  group_by(yearID) %>% # For every year...
  
  # Create a new variable that is the sum of HRs for each group indicated above, in this case each year
  summarize(HRs = sum(HR))

#Activity 8
library(dplyr)
Teams %>% # Take the teams data, THEN
  filter(yearID %in% c(1960:2022)) %>%  # Get only the years 1960-2022, THEN
  # (We could've used >= and <=, but this is more compact. Can you interpret this code?)
  
  group_by(yearID) %>% # For every year...
  
  # Create a new variable that is the sum of HRs for each group indicated above, in this case each year
  summarize(HRs = sum(HR))
# Summarize total wins by team in the 1990s
total_wins_1990s <- Teams %>%
  filter(yearID >= 1990 & yearID < 2000) %>%  # Filter for the 1990s
  group_by(teamID) %>%                         # Group by team
  summarize(total_wins = sum(W, na.rm = TRUE)) %>%  # Summarize total wins
  arrange(desc(total_wins))                    # Sort by total wins in descending order

# View the results
head(total_wins_1990s)

#9 
teams_2022 %>% # Take the 29 team data 
  ggplot(aes(x = R)) + # Feed it to ggplot, and set the X "aesthetic" to be runs (that is, make the x-axis be runs)
  # Notice this line ends with a `+`, not a `%>%`. This is because we are "adding" layers to the plot
  
  geom_histogram(binwidth = 50,   # A "geom" is a "layer" you add to the plot. 
                 fill = "blue") + # It's roughly equivalent to the graph type you want to make.
  # For a histogram you can manually specify the width of the bins you want as we did here, 
  # or the number of bins with e.g. bins = 10.
  # You can also set the color with `fill=`.
  # You may have more than one layer (e.g. put down scatterplot points, then overlay a line).
  
  
  labs(x = "Runs", # Change the x-axis label
       y = "Number of Teams", # Change y-axis label
       title = "Distribution of Runs Scored by MLB Teams in 2022") # Add an overall BRIEF but DESCRIPTIVE title to the chart

library(ggplot2)

# Create a histogram of runs scored in 2022
hist_runs_2022 <- ggplot(Teams_2022, aes(x = R)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Runs Scored by Team in 2022",
       x = "Runs Scored",
       y = "Number of Teams")

# Print the histogram
hist_runs_2022

# Create a density plot of runs scored in 2022
density_runs_2022 <- ggplot(Teams_2022, aes(x = R)) +
  geom_density(fill = "lightgreen") +
  labs(title = "Density Plot of Runs Scored by Team in 2022",
       x = "Runs Scored",
       y = "Density")

# Print the density plot
density_runs_2022


#Strengths: Provides a clear representation of the frequency of runs scored within specified bins. It is easy to interpret how many teams fall within certain ranges of runs scored.
#Weaknesses: The choice of bin width can affect the appearance and interpretation. Different bin widths can give a misleading view of the distribution.
#Density Plot:
  
#Strengths: Shows a smooth curve representing the distribution of runs scored. It can be more visually appealing and easier to interpret the overall shape of the distribution.
#Weaknesses: It can be less precise for identifying exact counts within specific ranges and may obscure features of the data if the underlying distribution is not smooth.

#3 
# Histogram of doubles
hist_doubles_2022 <- ggplot(Teams_2022, aes(x = `2B`)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Doubles by Team in 2022",
       x = "Doubles",
       y = "Number of Teams")

# Print the histogram of doubles
hist_doubles_2022

# Density plot of doubles
density_doubles_2022 <- ggplot(Teams_2022, aes(x = `2B`)) +
  geom_density(fill = "lightgreen") +
  labs(title = "Density Plot of Doubles by Team in 2022",
       x = "Doubles",
       y = "Density")

# Print the density plot of doubles
density_doubles_2022

#5 There are many other variables you could explore the distribution of, such as:

#Home runs (HR)
#Stolen bases (SB)
#Earned run average (ERA)
#Walks (BB)

#Activity 9
Teams %>% # Take Teams data
  filter(yearID >= 2001, yearID <= 2022) %>% # Only use team data from 2001-20123
  
  mutate(rundiff = R - RA) %>% # Create a new variable for run differential
  
  ggplot(aes(x = rundiff, y = W)) + # Set the x "aesthetic" to be wins (that is, plot wins on the x-axis) 
  # Set the y "aesthetic" to be rundiff (that is, plot rundiff on the y-axis)
  
  geom_point() + # Make our first layer a scatterplot using geom_point
  
  geom_smooth() + # Add a second layer of a smoothed line of best fit on top of the scatterplot
  
  labs(title = "Run Differential vs. Wins in MLB, 2001-22", # Add brief, descriptive title
       x = "Run Differential", y = "Wins") # Make better x and y axis labels

#1
# Scatter plot of Wins vs. Run Differential
Teams %>%
  filter(yearID >= 2001, yearID <= 2022) %>% # Use team data from 2001-2022
  mutate(rundiff = R - RA) %>% # Create a new variable for run differential
  ggplot(aes(x = rundiff, y = W)) + # Set the x aesthetic to run differential and y aesthetic to wins
  geom_point() + # Scatter plot using geom_point
  geom_smooth(method = "lm", color = "blue") + # Add a linear regression line
  labs(title = "Run Differential vs. Wins in MLB (2001-2022)", # Add title
       x = "Run Differential", y = "Wins") # Better x and y axis labels

# Print the scatter plot
scatter_plot

#2
Teams %>%
  filter(yearID >= 2001, yearID <= 2022) %>%
  ggplot(aes(x = OBP, y = R)) +  # OBP on x-axis, R on y-axis
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "On-base Percentage vs. Runs Scored (2001-2022)",
       x = "On-base Percentage", y = "Runs Scored")

#Lab 12
#The relationship between runs scored and wins in Major League Baseball (MLB) is a critical indicator of team performance. Analyzing data from 2001 to 2022 reveals a strong positive correlation: as teams score more runs, their win totals generally increase. Visual assessments, such as scatter plots, illustrate this trend, showing that higher offensive production often leads to more victories.

#Statistical analyses, including Pearson correlation coefficients and linear regression, further quantify this relationship. A high correlation coefficient suggests that teams with robust run-scoring abilities are more successful. Additionally, linear regression can predict the number of wins based on runs scored, highlighting the importance of offense in securing victories.

#Understanding this connection is essential for teams and analysts alike, as it emphasizes the significance of scoring in achieving success on the field. Ultimately, the relationship between runs scored and wins remains a fundamental aspect of baseball analytics and team strategy.

#Lab 13
#While I can't provide a specific numerical value without checking the data, I would intuitively estimate that the correlation coefficient between runs scored and wins in Major League Baseball (MLB) is likely to be quite high, perhaps around **0.7 to 0.9**. 

#This estimation is based on the understanding that teams that score more runs generally tend to win more games. However, the exact value would depend on the specific dataset and season analyzed, as there are factors like pitching performance and defensive capabilities that can influence the outcomes as well. A strong positive correlation would support the idea that scoring runs is crucial for winning games. 

#If you check the data, you might find the exact coefficient and see if it aligns with this intuition!

#lab 14
teams_21c <- Teams %>% 
  filter(yearID >= 2001, yearID <= 2022)
cor(teams_21c$R, teams_21c$W) # The syntax {DATA_FRAME}${VAR_NAME} references the indicated variable in the indicated data frame
# Correlation between runs against and wins
cor(teams_21c$RA, teams_21c$W)

# Visualizing the relationship
library(ggplot2)

teams_21c %>%
  ggplot(aes(x = RA, y = W)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Runs Against vs. Wins (2001-2022)", 
       x = "Runs Against", y = "Wins")
#2 After calculating the correlation coefficients, if runs scored (R) has a higher coefficient than runs against (RA), then scoring runs is more important for winning games. Conversely, if RA has a higher coefficient, it indicates that preventing runs is more critical to a team's success.

#3. Investing in Offense vs. Defense
#If the analysis shows that runs scored correlates more strongly with wins, invest in offense (quality position players). If runs against shows a stronger correlation, then invest in defense (quality pitchers).

#4. $10 Million Investment Decision
#If runs scored is more important, spend the $10 million on a top position player to boost the offense.
#If runs against is more important, allocate the funds to a high-quality pitcher to improve defensive performance.

# Plot a line of best fit through the data
teams_21c %>% 
  ggplot(aes(x = R, y = W)) +
  geom_point() +
  geom_smooth(method = "lm") + # Fits a simple linear regression model instead of a LOESS smoothing line
  labs(title = "Runs vs. Wins in MLB, 2001-2022", 
       x = "Runs Scored", y = "Wins")

# Fit a simple linear regression model to get the line's equation
m1 <- lm(data = teams_21c, formula = W ~ R)
summary(m1)

# 16
# Create a new data frame of teams data for 2001-19 (that is, the 20th century)
teams_21c <- Teams %>% 
  filter(yearID >= 2001, yearID <= 2022) %>% 
  mutate(rundiff = R - RA)

teams_21c %>% 
  ggplot(aes(x = rundiff, y = W)) +
  geom_point() + 
  labs(title = "Run Differential vs. Wins in MLB, 2001-22", 
       x = "Run Differential", y = "Wins")

m2 <- lm(data = teams_21c, formula = W ~ rundiff)
summary(m2)

#17
teams_21c %>% 
  select(R, rundiff, W, attendance) %>% 
  cor() # Unformatted and minimally-informative correlation matrix
teams_21c %>% 
  select(R, rundiff, W, attendance) %>% 
  cor() # Unformatted and minimally-informative correlation matrix
### 1. Variables Most and Least Strongly Associated with Wins

#### **Most Strongly Associated Variables**
#- **Runs Scored (R)**: Typically shows a strong positive correlation with wins. Teams that score more runs tend to win more games.
#- **Run Differential (rundiff)**: This metric often has a very high correlation with wins as it considers both runs scored and runs allowed. A positive run differential indicates a team's ability to score more runs than they allow, leading to more victories.

#### **Least Strongly Associated Variables**
#- **Attendance**: While attendance may show a positive correlation with wins, it is usually weaker compared to performance metrics like runs scored and run differential. Attendance is influenced by factors such as team performance, marketing, and stadium conditions.

### 2. Correlation Between Attendance and Wins

#### **Positive Correlation Explanation**
#- **Fan Engagement**: Higher attendance often reflects increased fan interest and engagement. When a team performs well, more fans are likely to attend games, creating a positive feedback loop.
#- **Revenue Generation**: Greater attendance increases revenue through ticket sales, concessions, and merchandise, which can help fund better player acquisitions, coaching, and facilities, indirectly contributing to more wins.

#### **Does Greater Attendance Cause More Wins?**
#- **Correlation vs. Causation**: Although there is a positive correlation between attendance and wins, it does not imply that higher attendance causes more wins. Both attendance and wins may be influenced by underlying factors, such as a team’s performance, marketing strategies, and community support. For instance, winning teams tend to attract more fans, while strong fan support can contribute to a positive home-field advantage. 

#18
#1
#Assessing Fit
#Linear Regression Line (Blue): This line assumes a constant rate of change and is useful for predicting outcomes when the relationship is approximately linear.
#LOESS Line (Red): This line is more flexible and can adapt to non-linear relationships. It provides a smoothed curve that can better represent complex patterns in the data.
#Which Line to Use for Prediction?
 # If the LOESS line closely follows the data points and shows significant deviations from the linear regression line, it indicates a non-linear relationship. In such a case, as a sabermetrician, you might prefer the LOESS line for predictions, as it can capture nuances in the data that a linear model might miss.
#However, if the relationship appears relatively linear (the blue line closely follows the data points), then the linear regression model may be sufficient for making predictions.
#2
teams_21c %>% 
  mutate(rundiff2 = (R - RA)^2) %>% 
  ggplot(aes(x =rundiff2 , y = W)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") + # Fits a simple linear regression model instead of a LOESS smoothing line
  geom_smooth(color = "red") + # Fits a more flexible LOESS model
  labs(title = "Run Differential Squared vs. Wins in MLB, 2001-22", 
       x = "Wins", y = "Run Differential Squared")
correlation <- cor(teams_21c$rundiff2, teams_21c$W)
print(correlation)

#3 Relying solely on the correlation coefficient provides a numerical value 
#indicating the strength and direction of the linear relationship between 
#squared run differential and wins. However, it may miss non-linear patterns. 
#Plotting the data alongside the correlation coefficient allows for a visual 
#representation of relationships, revealing trends, outliers, and clusters 
#that the correlation alone cannot capture. While a strong correlation suggests a
#positive relationship, a scatter plot can indicate a more complex interaction. 
#Thus, combining both methods offers a comprehensive understanding of how these variables 
#relate, highlighting nuances that may inform better predictions and insights.

#19
# Create a data frame of FIP constants from Fangraphs
fipc <- data.frame(year = c(2019:2001),
                   fipc = c(3.214, 	3.161, 	3.158, 	3.147, 	3.134, 	3.132, 	3.048, 	3.095, 	3.025, 	3.079, 	3.097, 	3.132, 	3.24, 	3.147, 	3.02, 	3.049, 	3.031, 	2.962, 	3.049))
# Calculate FIP
pitchers_rep <- Pitching %>%
  filter(yearID >= 2001, yearID <= 2022) %>%
  
  # "Join" in data on FIP constants from above so we can use it below
  left_join(fipc, by = c("yearID" = "year")) %>% 
  
  # Create FIP and BABIP. ERA already exists 
  mutate(FIP = (13*HR + 3*(BB-IBB+HBP) - 2*SO)/(IPouts) + fipc, # FIP formula from Fangraphs
         BABIP = (H - HR)/(BFP - SO - BB - HR)) %>%  # Modified version of BABIP based on stats easily available in Lahman
  
  # Finally we need to create new columns for each stat for the subsequent season
  arrange(playerID, yearID) %>% 
  group_by(playerID) %>% 
  mutate(ERA_next = lead(ERA),
         BABIP_next = lead(BABIP),
         FIP_next = lead(FIP),
         IPouts_next = lead(IPouts)) %>% 
  ungroup() %>% 
  
  filter(!is.na(ERA_next), # Drop all seasons without next ERA (that is, each player's last season)
         !is.na(ERA),
         !is.infinite(BABIP),
         !is.nan(BABIP),
         !is.infinite(FIP),
         !is.nan(FIP)) 

# Note this isn't perfect because some players skip seasons entirely, so the "next" variables are actually the next season
# they play, not necessarily the subsequent season.

# Quality check: print the last 20 observations of some variables to verify we got what we expected
pitchers_rep %>% 
  select(playerID, yearID, ERA, ERA_next, FIP, FIP_next) %>% 
  tail(20)

#1
#Based on typical findings in baseball analytics:
  
#ERA (Earned Run Average) is expected to be positively correlated with BABIP. A higher BABIP usually indicates that more balls in play are becoming hits, leading to a higher ERA.
#FIP (Fielding Independent Pitching) is often expected to show a strong correlation with ERA. Since FIP is designed to isolate a pitcher’s performance from their defense, a higher FIP typically reflects a poorer performance, leading to a higher ERA.
#BABIP may show a weaker correlation with FIP compared to the other two because FIP focuses more on outcomes a pitcher can control (strikeouts, walks, home runs) rather than hits allowed

#2
# Calculate year-to-year correlations for ERA, BABIP, and FIP
correlations <- pitchers_rep %>%
  select(yearID, ERA, BABIP, FIP) %>%
  group_by(yearID) %>%
  summarise(
    ERA_next = lead(ERA),
    BABIP_next = lead(BABIP),
    FIP_next = lead(FIP)
  ) %>%
  filter(!is.na(ERA_next), !is.na(BABIP_next), !is.na(FIP_next)) %>%
  summarise(
    cor_ERA = cor(ERA, ERA_next),
    cor_BABIP = cor(BABIP, BABIP_next),
    cor_FIP = cor(FIP, FIP_next)
  )

print(correlations)

#3
#After running the above code, you would expect to see:
#Strong correlations between ERA and FIP. 
#This would indicate that performance metrics such as FIP, 
#which focuses on what a pitcher can control, correlate well with overall performance (ERA).
#Moderate correlations between ERA and BABIP. 
#This suggests that while there is a relationship, it’s not as strong as the relationship between ERA and FIP.
#The correlation between BABIP and FIP might be weaker since FIP does not directly account for hits allowed, which BABIP measures.

#20
pitchers_rep %>%
  filter(IPouts >= 420, 
         IPouts_next >= 420) %>% 
  select(BABIP, BABIP_next, ERA, ERA_next, FIP, FIP_next) %>% 
  psych::pairs.panels()

teams_21c <- teams_21c %>% 
  
  mutate(Singles = H - HR - X2B - X3B, # Create variable for number of singles
         TB = Singles + 2*X2B + 3*X3B + 4*HR, # Create variable for total bases
         RC = (H+BB)*TB/(AB+BB)) %>%  # Formula for runs created
  
  # Create variable for runs next year
  arrange(franchID, yearID) %>% 
  group_by(franchID) %>% 
  mutate(R_next = lead(R)) %>% 
  ungroup()

teams_21c %>%
  select(R_next, R, RC, TB, X3B, attendance) %>% 
  psych::pairs.panels()

# Fit the linear model
model <- lm(R_next ~ R + RC + TB + X3B + attendance, data = teams_21c)

# View the summary of the model
summary(model)

#21
teams_21c %>% 
  ggplot(aes(x = DivWin, y = rundiff)) +
  geom_boxplot() + 
  geom_jitter(height = 0) + 
  labs(title = "Run Differential by Division Winner, MLB 2001-22", 
       x = "Won Division?", y = "Run Differential") 
#1 I expect that teams that win their division (indicated by DivWin) 
#will generally have a higher run differential (rundiff) compared to those that do not. 
#This expectation is based on the premise that successful teams—those that are strong enough to win their division—tend to score more runs than they allow, resulting in a positive run differential. Conversely, teams that do not win their division may have a more negative or less positive run differential, reflecting their poorer performance throughout the season.
#2
library(ggplot2)
library(dplyr)

# Assuming teams_21c is your data frame with relevant columns
teams_21c %>% 
  ggplot(aes(x = DivWin, y = rundiff, fill = DivWin)) +
  geom_boxplot() + 
  geom_jitter(height = 0, alpha = 0.5, size = 1) +  # Adding some transparency and size for clarity
  labs(title = "Run Differential by Division Winner, MLB 2001-22", 
       x = "Division Winner", 
       y = "Run Differential") +
  scale_fill_manual(values = c("red", "blue"), labels = c("No", "Yes")) +  # Custom colors
  theme_minimal() +  # Clean theme for better presentation
  theme(legend.position = "none")  # Hide legend for clarity

teams_21c %>% 
  ggplot(aes(x = DivWin, y = rundiff)) +
  geom_boxplot() + 
  geom_jitter(height = 0) + 
  labs(title = "Run Differential by Division Winner, MLB 2001-22", 
       x = "Won Division?", y = "Run Differential")

m3 <- lm(data = teams_21c, formula = rundiff ~ DivWin)
summary(m3)
