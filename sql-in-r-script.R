# load in libraries
library(DBI)
library(RPostgres)
library(dplyr)

# establish connection with postgres data base
con <- dbConnect(RPostgres::Postgres(),dbname = 'fueleconomy', 
                 host = 'address', # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'
                 port = 5432, # or any other port specified by your DBA
                 user = 'username',
                 password = 'password')

# make sql query
res <- dbSendQuery(con, "

                   SELECT *
                   FROM common
                   WHERE make LIKE 'B%';
                   
                   ")
# execute query
df <- dbFetch(res)

# clear query
dbClearResult(res)

# disconnect from data base
dbDisconnect(con)
