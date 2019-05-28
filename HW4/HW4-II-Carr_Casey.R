'Casey Carr HW4-II Due April 23, 2018'


'1. Compute the average delay by destination, then join on the 
airports data frame so you can show the spatial distribution of
delays. Here is an easy way to draw a map of the United States: 

Use the size or color of the points to display the average
delay for each airport.  '
airports %>%
  semi_join(flights, c("faa" = "dest")) %>%
  ggplot(aes(lon, lat)) +
  borders("state") +
  geom_point() +
  coord_quickmap()

avg_dest_delays <-
  flights %>%
  group_by(dest) %>%
  # arrival delay NA's are cancelled flights
  summarise(delay = mean(arr_delay, na.rm = TRUE)) %>%
  inner_join(airports, by = c(dest = "faa"))

'must input: install.packages("maps")'
avg_dest_delays %>%
  ggplot(aes(lon, lat, color = delay)) +
  borders("state") +
  geom_point() +
  coord_quickmap() +
  xlab("Latitude") + ylab("Longitude") +
  labs(title = paste("Spatial Distribution of the Average Delay by Destination"))

'2. What does it mean for a flight to have a missing tailnum?
  What do the tail numbers that do not have a matching record in
planes have in common? (Hint: find one variable explains ~90%
                        of the problems.) '

'Flights have a missing tailnum are those that were cancellled, or without missing dep_time, etc.'
flights %>%
  filter(is.na(tailnum))

'For those tailnum that don't have a matching record in plane, it seems most of them come from 
the same two carriers. American Airlines (AA) and Envoy Airlines (MQ) (maybe do not report tail numbers).'

flights %>%
  anti_join(planes, by = "tailnum") %>%
  count(carrier, sort = TRUE)


'3. Find the 48 hours (over the course of the whole year) that
have the worst delays. Cross-reference it with the weather
data. Can you see any patterns? '

frequency_delay <-
  flights %>%
  group_by(month, day) %>%
  summarize(avg_delay = sum(arr_delay + dep_delay, na.rm = TRUE)) %>%
  mutate(twoday_delay = avg_delay + lag(avg_delay)) %>%
  arrange(-twoday_delay)

weather_patterns <-
  weather %>%
  group_by(month, day) %>%
  summarize_at(vars(humid, precip, temp), mean, na.rm = TRUE)

frequency_delay %>%
  left_join(weather_patterns) %>%
  arrange(desc(twoday_delay))

'It seems as though precipitation was higher in delays flights, and temprature was slightly higher.
Although the pattern is just by looking at top/bottom 10. Graphical inspection should yield more interesting
patterns.'
  
  
'4. Imagine you wanted to draw (approximately) the route each
plane flies from its origin to its destination. What variables
would you need? What tables would you need to combine? Draw
(approximately) the route each plane flies from its origin to
its TOP 10 destinations. '

'The main two tables we need to combine are airports and flights. 
We need to match bth origin and dest in flights with faa in airports. 
If we want additional information about each plane, then we will need to match tailnum in planes with tailnum 
in flights as well.'

'flights table: origin and dest
airports table: longitude and latitude variables
join flights with airports twice. The first join adds the location of the origin airport (origin). 
The second join adds the location of destination airport (dest) '

'You would need to combine airports with flights because the airports dataset the the coordinates of the airport. 
You could match them by the faa variable in airports and the origin and dest from flights'
flights %>%
  left_join(airports, by = c("origin" = "faa")) %>%
  left_join(airports, by = c("dest" = "faa")) %>% filter(!is.na(name.y)) %>% filter(!is.na(name.x)) %>%
  group_by(tailnum) %>%
  ggplot(mapping = aes(x = lat.x,lat.y, y = lon.x,lon.y))
  



'5. What happened on June 13 2013? Display the spatial pattern
of delays, and then use Google to cross-reference with the
weather. '

'There was a large series of storms in the southeastern US (see June 12-13, 2013)

The largest delays are in Tennessee (Nashville), the Southeast, and the Midwest'

flights %>% filter(year == 2013, month == 6, day == 13) %>%
  group_by(dest) %>%
  summarize(avg_arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  left_join(airports, by = c('dest' = 'faa')) %>%
  ggplot(aes(x = lon, y = lat, size = avg_arr_delay, color = avg_arr_delay)) +
  borders('state') +
  geom_point(alpha = .5) +
  scale_color_continuous(low = 'green', high = 'blue') + 
  coord_quickmap() +
  xlab("Longitude") + ylab("Latitude") +
  labs(title = paste("Spatial Pattern of Delays on June 13, 2013"))


'6. Filter flights to only show flights with planes and carriers
that have flown having most (top 10) and least (bottom 10)
flights. '

flights_flown <- flights %>%
  semi_join(count(flights, tailnum) %>% filter(!is.na(tailnum))) #%>% group_by(carrier) %>% summarise(count = n()))
ranked_flights <- flights_flown %>% group_by(carrier, tailnum) %>% summarise(n = n()) %>% arrange(desc(n))
