#Author = Adam Vajdak
#Document = Homework for week 3
######################
library(tidyverse)
# data taken from https://www.kaggle.com/datasets/joyshil0599/comprehensive-flight-data-from-priceline?resource=download 

library(readr)
setwd("C:/Users/Vajda/Desktop/HOmework 2SU/719/week3 hw")

air <- read_csv("flight-cleaned.csv")
str(air)
library(RColorBrewer)


colnames(air)
air$Airline <-air$`Airline name`
air$Dep <- air$`Depreture  Airport`
air$Dest <- air$`Destination Airport`
air$Price <-as.numeric(air$`Ticket prize(Doller)`)
air$Travel<- substr(air$Travel,1,nchar(air$Travel)-1)
air$Time<- substr(air$Time,1,nchar(air$Time)-1)
air$Travel <- as.numeric(air$Travel)
air$Time <- as.numeric(air$Time)
air$Stops <-as.numeric(air$Stops)

air$Travel_timeM <- ((air$Travel*60)+air$Time)
air$Travel_timeH <- (air$Travel_timeM/60)

unique(air$Airline)


hist(air$Price,col=c("orange")
     ,main="Histogram of Ticket Prices"
     , xlab = "Ticket Prices"
     ,breaks = 24
     ,labels = TRUE
)



air<-air[,-2:-3]


air <- air[complete.cases(air$Price), ]





hist(air$Travel_timeH,col=c("blue")
     ,main="Histogram of Total travel Times
     "
     , xlab = "Travel Time in Hours"
     ,breaks = 20
     ,labels = TRUE
)




library(ggplot2)

# Create a pie chart with labels
pie_chart <- ggplot(air, aes(x = "", fill = Airline)) +
  geom_bar(width = 1) +
  coord_polar("y", start = 0) +
  labs(fill = "Airline") +
  theme_void()

# Display the pie chart
print(pie_chart)

boxplot(x= air$Price
     ,col= "red"
     ,main="Price of a ticket"
     ,ylab= "Price of Ticket in USD$"
     #,ylab= "Travel Time in Minutes"
     ,bty = "n"
)

# Plot 1: Histogram of Prices
ggplot(air, aes(Price)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  labs(x = "Price", y = "Count") +
  ggtitle("Distribution of Prices")

# Plot 2: Bar chart of Airlines
ggplot(air, aes(Airline)) +
  geom_bar(fill = "steelblue") +
  labs(x = "Airline", y = "Count") +
  ggtitle("Number of Flights by Airline")


plot(x= air$Price,y=air$Travel_timeM
     ,col= "red"
     ,main="Price and Travel"
     ,xlab= "Price of Ticket in USD$"
     ,ylab= "Travel Time in Minutes"
     ,bty = "n"
)


#air <- air[order(air$Depreture_Time,decreasing=TRUE),]

#hist(air$`Depreture _Airport`,col=c("blue")
#    ,main="Histogram of Total travel Times"
#     ,xlab = "Travel Time in Hours"
#     ,breaks = 20
#     ,labels = TRUE
#)

fline<-(air %>%
  group_by(Airline) %>%
  summarize(distinct_points = n_distinct(ID)))


fline <- fline[order(fline$distinct_points,decreasing=TRUE),]
#fline$distinct_points <-as.numeric(fline$distinct_points)


barplot(fline$distinct_points
        , col=c("steelblue")
        ,names.arg= fline$Airline
        ,border = "NA"
        ,ylab = "Flights"
        ,main = "Numbers of Flights per Airline"
        ,las=3
        ,horiz = F
)


# price distribution by airline
box_plot <- ggplot(air, aes(x = Airline, y = Price)) +
  geom_boxplot() +
  labs(x = "Airline", y = "Price") +
  ggtitle("Price Distribution by Airline") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

box_plot

# Round Travel_timeH to the nearest whole number
air$Rounded_Travel_timeH <- round(air$Travel_timeH)

# Calculate the average price per rounded Travel_timeH
average_price <- air %>%
  group_by(Rounded_Travel_timeH) %>%
  summarize(Average_Price = mean(Price))

# Create the plot
average_price_plot <- ggplot(average_price, aes(x = Rounded_Travel_timeH, y = Average_Price)) +
  geom_point() +
  geom_line(color = "red") +
  labs(x = "Rounded Travel Time (hours)", y = "Average Price") +
  ggtitle("Average Price per Rounded Travel Time") +
  theme_minimal()

average_price_plot


#########

top_10_airlines <- air %>%
  group_by(Airline) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  top_n(10)

filtered_air <- air %>%
  filter(Airline %in% top_10_airlines$Airline)

# Create a pie chart with labels and borders
pie_chart <- ggplot(filtered_air, aes(x = "", fill = Airline)) +
  geom_bar(width = 1, color = "white") +
  coord_polar("y", start = 0) +
  labs(fill = "Airline") +
  theme_void()+
  ggtitle("Top 10 Airlines")

print(pie_chart)

# Calculate the count and average price per Travel_timeH for the top 10 airlines
top_10_airlines <- air %>%
  group_by(Airline) %>%
  summarize(Count = n(), Average_Price = round(mean(Price / Travel_timeH),2)) %>%
  top_n(10, Count)

# Create the bar plot
bar_plot <- ggplot(top_10_airlines, aes(x = reorder(Airline, -Count), y = Average_Price,fill = Airline)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Airline", y = "Average Price per Travel_timeH") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_text(aes(label = Average_Price), vjust = -0.2)+
  ggtitle("Price per hour traveled")


# Display the bar plot
print(bar_plot)


#######

top_15_airlines <- air %>%
  group_by(Airline) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  top_n(15)

filtered_air <- air %>%
  filter(Airline %in% top_15_airlines$Airline)

# Create a pie chart with labels and borders
pie_chart <- ggplot(filtered_air, aes(x = "", fill = Airline)) +
  geom_bar(width = 1, color = "white") +
  coord_polar("y", start = 0) +
  labs(fill = "Airline") +
  theme_void()+
  ggtitle("Top 15 Airlines")

print(pie_chart)

# Calculate the count and average price per Travel_timeH for the top 15 airlines
top_15_airlines <- air %>%
  group_by(Airline) %>%
  summarize(Count = n(), Average_Price = round(mean(Price / Travel_timeH), 2)) %>%
  top_n(15, Count) %>%
  arrange(desc(Count), Airline)

# Create the bar plot with airlines grouped alphabetically
bar_plot <- ggplot(top_15_airlines, aes(x = reorder(Airline, Count), y = Average_Price, fill = Airline)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Airline", y = "Average Price per Travel_timeH") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = Average_Price), vjust = -0.2) +
  ggtitle("Price per hour traveled")

# Display the bar plot
bar_plot





ggplot(air, aes(x = Airline, y = Price)) +
  geom_boxplot() +
  labs(x = "Airline", y = "Price") +
  ggtitle("Boxplot of Price by Airline")

ggplot(air, aes(x = Airline, y = Travel_timeH)) +
  geom_boxplot() +
  labs(x = "Airline", y = "Travel_timeH") +
  ggtitle("Boxplot of Travel_timeH by Airline")




theme_set(theme_bw())



filtered_air <- air %>%
  filter(Price <= 4000)

# Calculate the count of each airline
airline_counts <- filtered_air %>%
  count(Airline) %>%
  arrange(desc(n)) %>%
  top_n(15, n)

# Filter the data to include only the top 15 airlines by count
top_15_airlines <- filtered_air %>%
  filter(Airline %in% airline_counts$Airline)


# Create the boxplot with customizations
ggplot(top_15_airlines, aes(x = Airline, y = Price)) +
  geom_boxplot(fill = "#3366CC", color = "black", alpha = 0.8) +
  labs(x = "Airline", y = "Price") +
  ggtitle("Boxplot of Price by Airline") +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Calculate the count and average price per Travel_timeH for the top 15 airlines
top_15_airlines <- air %>%
  group_by(Airline) %>%
  summarize(Count = n(), Average_Price = round(mean(Price / Travel_timeH), 2)) %>%
  top_n(15, Count) %>%
  arrange(Count, Airline)

# Sort the Airline factor in alphabetical order
top_15_airlines$Airline <- factor(top_15_airlines$Airline, levels = sort(unique(top_15_airlines$Airline)))

# Create the bar plot with airlines grouped alphabetically
bar_plot <- ggplot(top_15_airlines, aes(x = Airline, y = Average_Price, fill = Airline)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Airline", y = "Average Price per Travel_timeH") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = Average_Price), vjust = -0.2) +
  ggtitle("Price per hour traveled")+
  theme(panel.grid = element_blank())

# Display the bar plot
bar_plot

library(igraph)

# Create an edge list from the air dataset
edges <- air %>%
  select(Dep, Dest) %>%
  filter(!is.na(Dep) & !is.na(Dest)) %>%
  distinct()

# Create a graph from the edge list
graph <- graph_from_data_frame(edges, directed = TRUE)

# Plot the network graph
plot(graph, layout = layout_with_fr, vertex.size = 10, edge.arrow.size = .75,
     vertex.label.cex = 2, edge.width = 2,
     main = "Flight Connections: Departure Airport to Destination Airport")
















