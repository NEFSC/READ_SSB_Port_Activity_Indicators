# This is code that uses Roracle to connect to oracle databases. 
# ROracle can be tricky to set up. See this document for instructions https://docs.google.com/document/d/1Qsv_Jfc8CsoG49-qK-2RHdSJzR7v48W7ehZbG9k3RUQ/edit

# your oracle id is stored in the R object "id"
# your oracle password is stored in the the object "oracle_pw"


if(!require(ROracle)) {  
  install.packages("ROracle")
  require(ROracle)}


# DBI and ROracle Connection
# ITD now manages oracle connections. The most surefire way to connect to oracle is:

drv<-dbDriver("Oracle")
tns_alias<-"Looks this up in TNSNAMES.ORA file, which is in $ORACLE_HOME"

users_conn<-ROracle::dbConnect(drv, id, password=oracle_pw, dbname=tns_alias)
# your query goes here
dbDisconnect(users_conn)




############################################################################################
#################These are old instructions, They are here for ease of looking up, but you should use the method above###########
############################################################################################
# This code assumes that
# your oracle id is stored in the R object "id"
# your oracle password is stored in the the object "novapw"


 ############################################################################################
 #First, set up Oracle Connection
 ############################################################################################

# The following are details needed to connect using ROracle. 
#drv<-dbDriver("Oracle")
#shost <- "<nefsc_users.full.path.to.server.gov>"
#port <- port_number_here
#ssid <- "<ssid_here>"

#nefscusers.connect.string<-paste(
#  "(DESCRIPTION=",
#  "(ADDRESS=(PROTOCOL=tcp)(HOST=", shost, ")(PORT=", port, "))",
#  "(CONNECT_DATA=(SERVICE_NAME=", ssid, ")))", sep="")
# users_conn<-ROracle::dbConnect(drv, id, password=oracle_pw, dbname=nefscusers.connect.string)
# # your query goes here
# dbDisconnect(users_conn)

############################################################################################
#################This is the end of the old instructions###########
############################################################################################


############################################################################################
#################This is sample code to pull data###########
############################################################################################


START.YEAR= 2015
END.YEAR=2018

#First, pull in permits and tripids into a list.
permit_tripids<-list()
i<-1

users_conn<-ROracle::dbConnect(drv, id, password=oracle_pw, dbname=tns_alias)

for (years in START.YEAR:END.YEAR){
  querystring<-paste0("select permit, tripid from vtr.veslog",years,"t")
  permit_tripids[[i]]<-dbGetQuery(users_conn, querystring)
  i<-i+1
}
  dbDisconnect(users_conn)

#flatten the list into a dataframe

permit_tripids<-do.call(rbind.data.frame, permit_tripids)
colnames(permit_tripids)[which(names(permit_tripids) == "PERMIT")] <- "permit"



# Pull in gearcode data frame from sole
users_conn<-ROracle::dbConnect(drv, id, password=novapw, dbname=tns_alias)

querystring2<-paste0("select gearcode, negear, negear2, gearnm from vtr.vlgear")
VTRgear<-dbGetQuery(users_conn, querystring2)

dbDisconnect(users_conn)











  
