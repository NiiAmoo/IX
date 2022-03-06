Sys.setenv(TZ = 'GMT')
library(tidyverse)
library(readxl)
library(skimr)
library(plotly)


netflix <- read_csv("Data/netflix_titles.csv/netflix_titles.csv")
head(netflix)


# A pie chart of ratings 
pie_data <- netflix %>% group_by(rating) %>% 
    summarize(count = n())


pie <- plot_ly(pie_data, labels = ~rating, values = ~count, type = 'pie')
pie <- fig %>% layout(title = 'Netflix Movie ratings',
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

pie


# There's no simple function to create a bar chart in R using ggplot for a reason.
netflix %>% group_by(rating) %>% 
    summarize(count = n()) %>% 
    ggplot(aes(x="", y=count, fill=rating)) +
    geom_bar(stat="identity", width=1) +
    coord_polar("y", start=0) +
    theme_void()


# A bar chart of ratings
Tv_bar_data <- netflix %>% filter (type == 'Movie') %>% group_by(rating) %>% 
    summarize(count = n()) %>% arrange(desc(count)) %>% slice_head(n = 5)

bar <- Tv_bar_data %>% ggplot(aes(x = reorder(rating, -count, sum), y = count)) + 
    geom_col(fill = c('#34495E','#D6DBDF','#D6DBDF','#D6DBDF','#D6DBDF'),
    ) +
    xlab('Ratings') + ylab('Frequency') + ggtitle('Top 5 Movie Ratings on Netflix') +
    theme_minimal()

bar <- ggplotly(bar,tooltip = FALSE)


# Movie Ratings over time 
rating_data <- netflix %>% select(type,release_year,rating) %>% 
    group_by(release_year,rating,type) %>% summarise(frequency = n())


# Initial line plot
rating_data %>% 
    ggplot(aes(x = release_year,y=frequency,group = rating, color = rating)) +
    geom_line(size = 1.5)
                      

# Improve to draw out insights
rating_data <- rating_data %>% filter(type == "Movie" & release_year >=2000 & 
                                          release_year < 2020 & is.na(rating) == FALSE) %>% 
                mutate( highlight=ifelse(rating=="TV-MA", "TV-MA", "Others")) 

# Get overall growthrates over a 10 and 20 year period
min_year = min(rating_data$release_year)
max_year = max(rating_data$release_year)

rate_range <- rating_data %>% filter(release_year == min_year | release_year == max_year | release_year == '2010') %>% 
    filter(highlight =='TV-MA')

growth_20 <- (rate_range$frequency[3]/rate_range$frequency[1] -1) * 100

growth_10 <- round((rate_range$frequency[3]/rate_range$frequency[2] -1) * 100,0)


# Updated plot
rating_data %>%
    ggplot( aes(x=release_year, y=frequency, group=rating, color=highlight)) +
    geom_line(size = 1.5) +
    scale_color_manual(values = c("#D6DBDF","#34495E")) +
    xlab("Release year") + ylab("Number of Movies") +
    ggtitle('Increase In Mature Content Over The Last Decade') +
    theme_minimal() +
    theme(legend.position="none") +
    geom_label( x=2014, y=300,
                label=glue::glue("Shows for Mature Audiences \n increased {growth_10}% over the last decade"), 
                size=4, color="#34495E") +
    theme(
        legend.position="none",
        plot.title = element_text(size=18),
        axis.title = element_text(size = 13)
    ) 
    