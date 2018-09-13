install.packages('RMySQL')
library(RMySQL)
library(dplyr)

#Connecting to database
host=Sys.getenv("MYSQL_HOST")
port=as.integer(Sys.getenv("MYSQL_PORT"))
dbname=Sys.getenv("MYSQL_DBNAME")
user=Sys.getenv("MYSQL_USER")
password=Sys.getenv("MYSQL_PASSWORD")

my_db = src_mysql(dbname=dbname, host=host, port=port, user=user, password=password)

#Display all tables in our database
src_tbls(my_db)

#We are going to work with the dataset containing 336776 flights that departed from NYC in 2013
install.packages('nycflights13')
library('nycflights13')
paste0("The flights data has ", nrow(flights), " observations a nd the variables in the dataset are:")
names(flights)

#We should avoid NAs in our data frame or we could not transfer data to a database
make_mysql_compatible = function(data_frame) {
  for(this_col in colnames(data_frame)) {
    isna=is.na(data_frame[[this_col]])
    if(any(isna)){
      if(is.numeric(data_frame[[this_col]])){
        data_frame[[this_col]][isna]=NaN
      }
      if(is.character(data_frame[[this_col]])){
        data_frame[[this_col]][isna]=''
      }
    }
  }
  return(data_frame)
}
flights=make_mysql_compatible(flights)

#Transfer data to the database
copy_to(my_db, flights, temporary = FALSE)

#Let's check if the table have been transfered
src_tbls(my_db)

#Connect to a table
flights_DB = tbl(my_db, "flights")
class(flights_DB)

#How mane distinct origins are in the flights table?
cat("Number of Distinct flights:")
flights_DB %>%
  distinct(origin) %>%
  count()

#How many flight were made on January first?
filter(flights_DB, month==1, day==1) %>%
  count()

#What's the most common carrier? 
flights_DB %>%
  group_by(carrier) %>%
  summarise(count=n()) %>%
  arrange(desc(count)) %>%
  head(1)

#Which months have the highest number of flights?
library(ggplot2)

flights_DB %>%
  group_by(month) %>%
  summarise(count=n()) %>%
  collect() %>%
  ggplot(aes(x=as.factor(month), y=count)) +
  geom_bar(stat='identity', color='purple', fill='skyblue') +
  coord_cartesian(ylim=c(25000,30000)) +
  xlab("Months")

#What is the longest distance for each carrier?
flights_DB %>%
  group_by(carrier) %>%
  summarise(Ldistance = max(distance, na.rm=TRUE)) %>%
  collect() %>%
  ggplot(aes(x=carrier, y=Ldistance)) +
  geom_point(color='red', aes(size=Ldistance))

#What is the five most common flight routes?
flights_DB %>%
  group_by(origin, dest) %>%
  summarise(total_flights = n()) %>%
  arrange(desc(total_flights)) %>%
  head(5)

#Is the carrier with the highest departure delay also with the highest arrival delay?
cat("The Carrier with the highest departure delay is ")
flights_DB %>%
  group_by(carrier) %>%
  summarise(H_dep_delay = max(dep_delay, na.rm=TRUE)) %>%
  arrange(desc(H_dep_delay)) %>%
  head(1)

cat("The Carrier with the highest arrival delay is ")
flights_DB %>%
  group_by(carrier) %>%
  summarise(H_arr_delay = max(arr_delay, na.rm=TRUE)) %>%
  arrange(desc(H_arr_delay)) %>%
  head(1)

# Are the unique destinations the same in all months?
flights_DB %>%
  distinct(dest, month) %>%
  group_by(month) %>%
  summarise(distinct_destinations = n()) %>%
  collect() %>%
  mutate(month = factor(month, levels = month[order(distinct_destinations, decreasing=TRUE)])) %>%
  ggplot(aes(x=month, y=distinct_destinations)) +
  geom_bar(stat='identity', color='skyblue', fill='#b35900') +
  xlab("") + ggtitle("Number of unique destinations for each month") +
  ylab("Count") + coord_cartesian(ylim=c(80,100))