## Time work

# Set wd
setwd("/Users/David/Dropbox (MIT)/bluebikes/MIT-15.072-Bike-Rebalancing")

# Filtering longest trips (outliers)
# Creating columns if neccesary
df_time <- df %>% 
  filter(tripduration < quantile(df$tripduration, probs = .98)) 
# %>%
#   mutate(tripduration.mins = tripduration/60) %>%
#   mutate(tripduration.mins.int = ceiling(tripduration.mins))

# Plotting days of week and start hour travel times
df %>%
  group_by(start.hour,start.day.of.week) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_point(aes(x=start.hour,y=n,color = as.factor(start.day.of.week)))


df_time %>%
  ggplot() +
  geom_density(aes(tripduration.mins)) 

# Trip duration distribution 
df_time %>%
  group_by(tripduration.mins.int) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  ggplot() +
  geom_point(aes(tripduration.mins.int,n)) +
  geom_vline(aes(xintercept = tripduration.mins.int[which.max(n)], 
                 color = "green")) + 
  scale_x_continuous(breaks = seq(0,80,5))
  

# Trip duration dist: aggregated by user type
df_time %>%
  group_by(tripduration.mins.int, usertype) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  ggplot() +
  geom_point(aes(tripduration.mins.int,n, color = usertype)) +
  geom_vline(aes(xintercept = tripduration.mins.int[which.max(n)], 
                 color = "green")) + 
  scale_x_continuous(breaks = seq(0,80,5))

# Trip duration dist: aggregated by start hour
df_time %>%
  group_by(tripduration.mins.int, start.hour) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  ggplot() +
  geom_line(aes(tripduration.mins.int,n, color = as.factor(start.hour))) + 
  scale_x_continuous(breaks = seq(0,80,5))

# Trip duration dist: aggregated by start hour (5 - 13), usertype
df_time %>%
  filter(start.hour <= 13) %>%
  filter(start.hour >= 5) %>%
  filter(tripduration.mins.int < 40) %>%
  group_by(tripduration.mins.int, start.hour, usertype) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  ggplot() +
  geom_line(aes(tripduration.mins.int,n, color = as.factor(start.hour))) + 
  facet_wrap(vars(usertype)) +
  scale_x_continuous(breaks = seq(0,80,5)) +
  labs( title = "Trip Duration Count by Start Hour, User Status",
        x = "Trip  Duration in Mins",
        y = "Count",
        color = "Start Hour")

# Trip duration dist: aggregated by start hour (14 - 22), usertype
df_time %>%
  filter(start.hour <= 22) %>%
  filter(start.hour >= 14) %>%
  filter(tripduration.mins.int < 40) %>%
  group_by(tripduration.mins.int, start.hour, usertype) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  ggplot() +
  geom_line(aes(tripduration.mins.int,n, color = as.factor(start.hour))) + 
  facet_wrap(vars(usertype)) +
  scale_x_continuous(breaks = seq(0,80,5))

# Trips per day per start hour
# Trip duration dist: aggregated by start hour (14 - 22), usertype
df_time %>%
  ggplot() +
  geom_histogram(aes(as.factor(start.hour)), stat = "count") + 
  facet_grid(rows = vars(usertype), cols = vars(as.factor(start.day.of.week))) 
  
