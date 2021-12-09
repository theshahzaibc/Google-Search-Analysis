library(gtrendsR)
library(reshape2)
library(ggplot2)

getTopRelatedQueries <- function(keyword_, type_, time_){
  return (gtrends(keyword_, gprop = type_, time = time_)$related_queries)
}

related_queries <- getTopRelatedQueries(c("Data Science"), "web", "now 1-H")[0:10,]
topRQ <- data.frame(words=related_queries$value, freq=related_queries$subject)
ggplot(topRQ, aes(x=topRQ$words, y=topRQ$freq)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Top 10 related queries for: 'Data Science' in last hour") + xlab("Queries") + ylab("Hits")
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

getTopCountries <- function(keyword_, type_, time_){
  return (gtrends(keyword_, gprop = type_, time = time_)$interest_by_country)
}
topCounties <- getTopCountries(c("Data Science"), "web", "now 1-d")[0:10,]
print(topCounties)
ggplot(topCounties, aes(x=0, y=hits, fill=location)) +
  labs(title = "Top Countries searched: 'Data Science' in last 24 hours") +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) + 
  geom_text(aes(label = hits), position = position_stack(vjust = 0.5)) +
  theme_void()

getIOT <- function(keyword_, type_, time_, geo_){
  return (gtrends(keyword_, gprop = type_, time = time_, geo = geo_)$interest_over_time)
}
lastFiveYears <- getIOT(c("Data Science"), "web", "all", "US")
print(lastFiveYears)

theme_set(theme_classic())
ggplot(lastFiveYears, aes(x=date)) + 
  geom_line(aes(y=hits)) + 
  labs(title="Interest over time hits for 'Data Science'", y="Hits", x="Years")
