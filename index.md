# Here you will see the code I did over my spring of 2025 semester at Agnes Scott College. 
# This code explored the potential relationship with hurricane magnitudes and White Ibis occurances in and around the Georgia area.

<img width="1000" height="160" src="https://upload.wikimedia.org/wikipedia/commons/7/79/NOAA_logo.svg">

install.packages("readr")
#to read csv files
install.packages("dplyr")
#to manipulate data
install.packages("ggplot2")
#to visualize data
install.packages("lubridate")
#to handle date/time observations
install.packages("auk")
#read bird data from eBird
install.packages("plotly")
#for graphs
install.packages("maps")
#for maps to show up on graphs
install.packages("sp")
#both needed to separate hurricane data to repective counties


#to view the packages
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(auk)
library(plotly)
library(maps)
library(sp)

hurdat2_data <- read_csv("hurdat2.csv")
#read file as an object

# View the first few rows of the hurricane data from NOAA NHC
head(hurdat2_data)

Wanted_Hurricane_Data <- select(hurdat2_data, c('storm_name','num_of_obs','date','time',
                                'latitude','longitude','maximum_sustained_wind_knots'))
#made a seperate data object for the columns I wanted

lat_range <- c(30.5, 35.0)  
lon_range <- c(-85.5, -80.8)  
#this will be the latitude and longitude ranges of Georgia

Wanted_Hurricane_Data$longitude2 = Wanted_Hurricane_Data$longitude
#duplicated the columns


Wanted_Hurricane_Data$longitude <- ifelse(grepl("W", Wanted_Hurricane_Data$longitude),
                                          NA, Wanted_Hurricane_Data$longitude)
Wanted_Hurricane_Data$longitude2 <- ifelse(grepl("E", Wanted_Hurricane_Data$longitude2),
                                           NA, Wanted_Hurricane_Data$longitude2)
Wanted_Hurricane_Data$latitude <- ifelse(grepl("S", Wanted_Hurricane_Data$latitude),
                                         NA, Wanted_Hurricane_Data$latitude)
#These removed certain values, replaced them to NAs so that it is easier for me to 
#make the W values negative and then merge the columns

unique(Wanted_Hurricane_Data$longitude)
unique(Wanted_Hurricane_Data$longitude2)
unique(Wanted_Hurricane_Data$latitude)
#gives me a summary of what is in my column

grepl("S", Wanted_Hurricane_Data$latitude)
#there is no "S" characers in the data so I only 
#need to worry about "N" for the latitude column

Wanted_Hurricane_Data$latitude <- gsub("N", "", Wanted_Hurricane_Data$latitude)
Wanted_Hurricane_Data$longitude <- gsub("E", "", Wanted_Hurricane_Data$longitude)
Wanted_Hurricane_Data$longitude2 <- gsub("W","", Wanted_Hurricane_Data$longitude2)
#deletes the directionality of the coordinates

Wanted_Hurricane_Data$latitude <- as.numeric(Wanted_Hurricane_Data$latitude)
Wanted_Hurricane_Data$longitude <- as.numeric(Wanted_Hurricane_Data$longitude)
Wanted_Hurricane_Data$longitude2 <- as.numeric(Wanted_Hurricane_Data$longitude2)
#made sure these columns were numerical values

Wanted_Hurricane_Data$longitude2 <- ifelse(!is.na(Wanted_Hurricane_Data$longitude2),
                                           -abs(Wanted_Hurricane_Data$longitude2), NA)
#made the values negative without disturbing the NAs yet
#these values need to be negative as they are from the West directionality

Wanted_Hurricane_Data$longitude <- coalesce(Wanted_Hurricane_Data$longitude2, 
                                            Wanted_Hurricane_Data$longitude)
#this should've merged the data into the longitude column, didn't need to remove NAs

Wanted_Hurricane_Data <- Wanted_Hurricane_Data[order(-Wanted_Hurricane_Data$longitude), ]
#reordered the column in a descending order to best look at the negative and positive values

Wanted_Hurricane_Data$longitude2 <- NULL
#deleting the longitude2 column as it is no longer needed

sum(is.na(Wanted_Hurricane_Data))
#double-checking NAs, there are 0 NAs

# Filter data based on these ranges for Georgia
filtered_HData <- Wanted_Hurricane_Data[Wanted_Hurricane_Data$latitude >= lat_range[1] & 
                                        Wanted_Hurricane_Data$latitude <= lat_range[2] &
                                        Wanted_Hurricane_Data$longitude >= lon_range[1] & 
                                        Wanted_Hurricane_Data$longitude <= lon_range[2], ]
#all my data is now sorted within the range of Georgia, next is to remove the "UNNAMED"
#hurricanes as they were not named until after 1871, NHC started 
#collecting data  in 1851

#filtered_HData$storm_name[filtered_HData$storm_name == "UNNAMED"] <- NA
#the storms named "UNNAMED" were removed

#sum(is.na(filtered_HData))
#checking for NAs-> 336 NAs

#HURICANEData <- na.omit(filtered_HData)
#made a new object with the NAs deleted

#need to make a category for the hurricanes
#If I wanted to just use the named storms,
#Use previous commented lines and replace "filtered_HData" w/HURICANEData

HURICANEData <- filtered_HData %>%
  mutate(category_knots = case_when(
    maximum_sustained_wind_knots <= 33 ~ "Tropical Depression",
    maximum_sustained_wind_knots >= 34 & maximum_sustained_wind_knots <= 63 ~ "Tropical Storm",
    maximum_sustained_wind_knots >= 64 & maximum_sustained_wind_knots <= 82  ~ "Category 1",
    maximum_sustained_wind_knots >= 83 & maximum_sustained_wind_knots <= 95 ~ "Category 2",
    maximum_sustained_wind_knots >= 96 & maximum_sustained_wind_knots <= 112 ~ "Category 3",
    maximum_sustained_wind_knots >= 113 & maximum_sustained_wind_knots <= 136 ~ "Category 4",
    maximum_sustained_wind_knots >= 137 ~ "Category 5",
    TRUE ~ NA_character_  # For wind speeds below hurricane threshold or missing
  ))
#making a column category for the wind speeds in knots
#most of the data was either classified at a tropical storm or tropical depression
#only 6 were considered hurricanes within category 1
#https://www.nhc.noaa.gov/aboutsshws.php where I got the category info from

sum(is.na(HURICANEData))
#double checking for NAs again

HURICANEData <- HURICANEData %>%
  filter(maximum_sustained_wind_knots != -99)
#-99 within this column is representative of
#unknown/missing data, this needs to be deleted

class(HURICANEData$date)
#checking class of the date column
#want it to be treated as a date

HURICANEData$date <- as.character(HURICANEData$date)
#change to character first so that way I can make the column date properly
HURICANEData$date <- as.Date(HURICANEData$date,format = "%Y%m%d")
#column is now set as a date

HURICANEData$Year <- substr(as.character(HURICANEData$date), 1, 4)
#extract the year to make the plot easier to read

# First plot with the hurricane data

HMagnitude <- plot_ly(
  data = HURICANEData,
  x = ~Year,
  y = ~maximum_sustained_wind_knots,
  type = 'scatter',
  mode = 'markers',
  color = ~factor(category_knots),
  text = ~paste("Name:", storm_name, "<br>Category:", category_knots, 
                "<br>Wind Speed:", maximum_sustained_wind_knots, "knots",
                "<br>Date:", date),
  hoverinfo = 'text'
) %>%
  layout(
    title = "Hurricane Wind Speeds and Magnitude Over Time",
    xaxis = list(title = "Time"),
    yaxis = list(title = "Wind Speed (knots)"),
    legend = list(title = list(text = "Category"))
  )

HMagnitude
#this is graph number 1

# Georgia Map Using the Map Package

georgia_map <- map_data("state") %>% filter(region == "georgia")
#need Georgia map

Georgia_Hurricanes <- plot_ly() %>%
  add_trace(
    data = georgia_map,
    x = ~long, y = ~lat,
    type = 'scatter',
    mode = 'lines',
    line = list(color = 'black'),
    showlegend = FALSE
  ) %>%
  # Add hurricane points
  add_trace(
    data = HURICANEData,
    x = ~longitude, y = ~latitude,
    type = 'scatter',
    mode = 'markers',
    color = ~factor(category_knots),
    text = ~paste("Name:", storm_name, "<br>Category:", category_knots,
    "<br>Wind Speed:", maximum_sustained_wind_knots, "knots",
    "<br>Date:", date),
    hoverinfo = 'text',
    marker = list(size = 10)
  ) %>%
  layout(
    title = "Hurricane Magnitude In and Around Georgia",
    xaxis = list(title = "Longitude"),
    yaxis = list(title = "Latitude"),
    legend = list(title = list(text = "Category")),
    geo = list(scope = 'usa')
  )

Georgia_Hurricanes
#Shows a Georgia map and the hurricanes over the years

# Statistical Analyses

#------I need to do statistical analyses for the dataset-------

str(HURICANEData$Year)
HURICANEData$Year <- as.numeric(as.character(HURICANEData$Year))
#Change the year to a numerical variable to make the linear
#regresion model treat this column as a continuous variable

model <- lm(maximum_sustained_wind_knots ~ Year, data = HURICANEData)
summary(model)
#Residual standard error: 15.12 on 567 degrees of freedom

#Multiple R-squared:  0.1982,	Adjusted R-squared:  0.1968 

#F-statistic: 140.1 on 1 and 567 DF,  p-value: < 2.2e-16

#Residual standard error of 15.12 means that there is a lot of 
#variability in my standard error
#likely due to the fact that storms wind speeds vary a lot

#0.1982 indicates that there is around 
#20% in variation in wind speeds over time, makes sense 
#as other factors contribute to wind speed variation

#p value is significant, wind speeds do have significant change over time
#and it is unlikely to be random

#" There's a statistically significant trend in hurricane wind speeds (knots) 
#over time, the years alone only explains about 20% of the variation 
#in wind speed of hurricanes affecting Georgia.The rest likely comes from
#other meteorological or environmental factors. "

cor(HURICANEData$Year, HURICANEData$maximum_sustained_wind_knots, 
    use = "complete.obs")
#correlation between hurricane wind speeds and the years?
#-0.4451845


#there is a negative correlation, as the year increases, wind speeds decrease
#and essentially the category of the hurricanes decrease too.
#The strength is moderate (closer to -0.5 than 0), 
#meaning there’s a noticeable downward trend, but not an extremely 
#strong one.
#hurricanes around Georgia may be getting slightly weaker, 
#on average — or at least, the strongest ones are less frequent


#Overall we can say, that there is a statistically significant decrease 
#in hurricane wind speeds over time in my data (around Georgia), 
#with a moderate inverse relationship.

# Regression Model

ggplot(HURICANEData, aes(x = Year, y = maximum_sustained_wind_knots)) +
  geom_point(alpha = 0.5, color = "steelblue") +  # Scatter points
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +  # Trend line
  labs(
    title = "Trend in Hurricane Wind Speeds Over Time (Georgia Region)",
    x = "Time",
    y = "Wind Speeds (knots)"
  ) +
  theme_minimal()
#shows my regression line

# White Ibis Data pulled from eBird
#-------------------Moving on to work on bird data--------------------------

Bird_DATA <- read.delim("ebd_US-GA_whiibi_202408_185008_smp_relFeb-2025.txt")

Wanted_Bird_Data <- select(Bird_DATA, c('OBSERVATION.DATE','COUNTY','OBSERVATION.COUNT',
                                        'LATITUDE','LONGITUDE'))
#took out the colunns I wanted to look at 
#for the bird data into a seperate dataset

Wanted_Bird_Data$OBSERVATION.COUNT[Wanted_Bird_Data$OBSERVATION.COUNT == "X"] <- NA
#Noticed there were 'X' in the observation column
#changed those to NAs to be removed

sum(is.na(Wanted_Bird_Data))
#checking for NAs-> 46 NAs

BIRDData <- na.omit(Wanted_Bird_Data)
#made a new object with the NAs deleted

BIRDData <- BIRDData[order(BIRDData$OBSERVATION.DATE), ]
#sorted the data in descending date order

BIRDData$OBSERVATION.DATE <- as.Date(BIRDData$OBSERVATION.DATE)
#The date column will read as a date and not a character

class(BIRDData$OBSERVATION.COUNT)
#want this to be numeric, not a character

BIRDData$OBSERVATION.COUNT <- as.numeric(BIRDData$OBSERVATION.COUNT)
#I got an error that this column was a "character column," 
#changing it to a numerical column to be summed up in the next line of code

BIRDData2 <- BIRDData %>%
  group_by(OBSERVATION.DATE, COUNTY,) %>%
  summarize(OBSERVATION.COUNT = sum(OBSERVATION.COUNT))
#this merged repeating dates while adding up the observation values
#and by county also made them a separate data table so it's easier to read

# Plot White Ibis Data
#-----------------------------------------------------------------
BIRD <- BIRDData %>%
  group_by(COUNTY) %>%
  summarize(OBSERVATION.COUNT = sum(OBSERVATION.COUNT))
#wanted to see what it would look 
#like merging the counties and their observations

ggplot(BIRD, aes(x=OBSERVATION.COUNT, y=COUNTY)) + 
  geom_bar(stat = "identity")
#this shows me observation.counts and counties
#----------------------------------------------------------------------


plot(BIRDData2$OBSERVATION.DATE, BIRDData2$OBSERVATION.COUNT, type = "l", col = "blue",
     xlab = "Time", ylab = "White Ibis Observations",
     main = "Observations of White Ibis Over Time")
#this plots bird observations over time, needs to still be cleaned

# Get map data for Georgia counties
ga_map <- map_data("county") %>%
  filter(region == "georgia")

BIRD$COUNTY <- tolower(BIRD$COUNTY)
#need to lower case the county names in my data 


#Merge map data with your observations
ga_map2 <- ga_map %>%
  left_join(BIRD, by = c("subregion" = "COUNTY"))


ga_map_filtered <- ga_map2 %>%
  filter(!is.na(OBSERVATION.COUNT))
#remove the NAs in the data

#Plot
ggplot(ga_map_filtered, aes(x = long, y = lat, group = group, fill = OBSERVATION.COUNT)) +
  geom_polygon(color = "black") +
  coord_fixed(1.0) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "gray90") +
  labs(title = "Observations by County in Georgia",
       fill = "Observations") +
  theme_minimal()
#this plot is okay for now, I will see if I can change it:

#----------------------------------------------------------------

#Load Georgia county polygons
ga_map2 <- map_data("county", region = "georgia")

#Step 3: Create a function to assign each point to a county polygon
#We'll use point-in-polygon matching (no spatial objects needed)
point_in_county <- function(lat, lon, county_df) {
  for (county_name in unique(county_df$subregion)) {
    polygon <- county_df[county_df$subregion == county_name, c("long", "lat")]
    if (point.in.polygon(lon, lat, polygon$long, polygon$lat) > 0) {
      return(county_name)
    }
  }
  return(NA)
}

#Step 4: Assign counties to each storm observation
HURICANEData$COUNTY <- mapply(point_in_county, HURICANEData$latitude, HURICANEData$longitude, 
                              MoreArgs = list(county_df = ga_map))

#Step 5: Summarize storm counts by county
storm_counts <- HURICANEData %>%
  filter(!is.na(COUNTY)) %>%
  group_by(COUNTY) %>%
  summarise(STORM_COUNT = n_distinct(storm_name))

#Step 6: Join storm counts to the map
ga_map2 <- left_join(ga_map, storm_counts, by = c("subregion" = "COUNTY"))

#Step 7: Plot
ggplot(ga_map2, aes(x = long, y = lat, group = group, fill = STORM_COUNT)) +
  geom_polygon(color = "black") +
  coord_fixed(1.0) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "gray90") +
  labs(title = "Unique Storms by County in Georgia",
       fill = "Storms") +
  theme_minimal()

#Define your 10 counties (must be lowercase to match map_data)
target_counties <- tolower(c("Bibb", "Berrien", "Ben Hill", "Bartow", 
                             "Banks", "Baldwin", "Baker", "Bacon", 
                             "Appling", "Atkinson"))

# Filter map and storm data
ga_map_filtered <- ga_map %>%
  filter(subregion %in% target_counties)

storm_counts_filtered <- storm_counts %>%
  filter(COUNTY %in% target_counties)

#Join filtered data
ga_map_final <- left_join(ga_map_filtered, storm_counts_filtered, by = c("subregion" = "COUNTY"))

ggplot(ga_map_final, aes(x = long, y = lat, group = group, fill = STORM_COUNT)) +
  geom_polygon(color = "black") +
  coord_fixed(1.0) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "gray90") +
  labs(title = "Unique Storms in Selected Georgia Counties",
       fill = "Storms") +
  theme_minimal()
#plot for just the 10 counties


#Step 1: Filter only matched counties (same as before)
HURICANEData$COUNTY <- mapply(point_in_county, HURICANEData$latitude, HURICANEData$longitude, 
                              MoreArgs = list(county_df = ga_map))

#Step 2: Summarize wind speed per county (use mean, or change to sum/max if preferred)
wind_by_county <- HURICANEData %>%
  filter(!is.na(COUNTY)) %>%
  group_by(COUNTY) %>%
  summarise(AVG_WIND = mean(maximum_sustained_wind_knots, na.rm = TRUE))

#Step 3: Filter for your 10 counties
target_counties <- tolower(c("Bibb", "Berrien", "Ben Hill", "Bartow", 
                             "Banks", "Baldwin", "Baker", "Bacon", 
                             "Appling", "Atkinson"))

ga_map_filtered <- ga_map %>%
  filter(subregion %in% target_counties)

wind_filtered <- wind_by_county %>%
  filter(COUNTY %in% target_counties)

#Step 4: Join to map data
ga_map_final <- left_join(ga_map_filtered, wind_filtered, by = c("subregion" = "COUNTY"))

ggplot(ga_map_final, aes(x = long, y = lat, group = group, fill = AVG_WIND)) +
  geom_polygon(color = "black") +
  coord_fixed(1.0) +
  scale_fill_gradient(low = "lightyellow", high = "red", na.value = "gray90") +
  labs(title = "Average Wind Speed in Selected Georgia Counties",
       fill = "Wind Speed (knots)") +
  theme_minimal()
#---------------------------------------------------------------------------------


#Merge both datasets by COUNTY
merged_data <- left_join(BIRD, wind_filtered, by = c("COUNTY" = "COUNTY"))

#Check merged data
head(merged_data)

merged_data <- na.omit(merged_data)


# Calculate correlation between White Ibis occurrences and wind speed
correlation_result <- cor(merged_data$OBSERVATION.COUNT, merged_data$AVG_WIND, method = "pearson")

#Print the correlation
correlation_result
#0.1746848

# Plot the Correlation between White Ibis and Hurricane Magnitude

ggplot(merged_data, aes(x = AVG_WIND, y = OBSERVATION.COUNT)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +  # Adds a linear regression line
  labs(title = "White Ibis Occurrences vs Wind Speed",
       x = "Average Wind Speed (knots)",
       y = "White Ibis Occurrences") +
  theme_minimal()

cor_test <- cor.test(merged_data$OBSERVATION.COUNT, merged_data$AVG_WIND)
cor_test$p.value
