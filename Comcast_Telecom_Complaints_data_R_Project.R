#################### 1.)Import data into R environment.####################

### Setting the directory 
setwd("C:/Users/Abhishek/Desktop/Project/")
getwd()   #checking the path of directory

#reading csv file
Comcast = read.csv("Comcast Telecom Complaints ?ata.csv")
View(Comcast)




#################### 2.(ii)Provide the trend chart for the number of complaints daily granularity levels.####################
#Loading The Date Into Single Format
#Use Lubridate Library to Format the Date Column
library(lubridat?)
li=parse_date_time(x = Comcast$Date,
                   orders = c("d m y", "d B Y", "m/d/y"),
                   locale = "eng")
Comcast_new = Comcast
Comcast_new$Date = li
#Dates Loaded In the Same Format in the new Dataframe
str(Comcast_new$Date)
View?Comcast_new)

#Extracting Month Column and Converting to The labels. 
Comcast_new$Month = format(as.Date(Comcast_new$Date), "%m")
Comcast_new$Month= month.abb[as.integer(Comcast_new$Month)]
View(Comcast_new)

#importing dplyr
library(dplyr)

#grouping data?with date and calculating Frequency by summarise
Comcast_date = Comcast_new %>% group_by(Date) %>% summarise(frequency = n())
View(Comcast_date)

#ploting graph date against Number of Complaints
plot(Comcast_date,type='l', main = "number of complaints dail? granularity levels", xlab = "Date", ylab = "Number of complaints",col="#ff0000")

#Loading The Date Into Single Format
#Use Lubridate Library to Format the Date Column
library(lubridate)
li=parse_date_time(x = Comcast$Date,
                   orders = c("? m y", "d B Y", "m/d/y"),
                   locale = "eng")
Comcast_new = Comcast
Comcast_new$Date = li
#Dates Loaded In the Same Format in the new Dataframe
str(Comcast_new$Date)
View(Comcast_new)

#Extracting Month Column and Converting to The labels. 
?omcast_new$Month = format(as.Date(Comcast_new$Date), "%m")
Comcast_new$Month= month.abb[as.integer(Comcast_new$Month)]
View(Comcast_new)

#importing dplyr
library(dplyr)

#grouping data with date and calculating Frequency by summarise
Comcast_date = Comcas?_new %>% group_by(Date) %>% summarise(frequency = n())
View(Comcast_date)

#ploting graph date against Number of Complaints
plot(Comcast_date,type='l', main = "number of complaints daily granularity levels", xlab = "Date", ylab = "Number of complaints",col?"#ff0000")


#################### 3.)Provide a table with the frequency of complaint types ####################

library(dplyr)

#Converting All String Values to Lower, so as to Eliminate Duplication of Any Complaint
Comcast_Complaint_type = Comcast_new %%?mutate(Customer.Complaint = tolower(Customer.Complaint))
View(Comcast_Complaint_type)

#forming table to calculate frequency of complain
CustTable = table(Comcast_Complaint_type$Customer.Complaint)
View(CustTable)
#storing table in dataframe 
CustTable = d?ta.frame(CustTable)

#renaming columns using filter
filtered  = CustTable %% rename(Custom_Complaint = Var1, Frequence = Freq )

#arranging data in Desc order of frequency
Final = filtered %% arrange(desc(Frequence))

#head shows the top 10 results
Most_fr?quent_complains = head(Final,10)
View(Most_frequent_complains)

attach(Most_frequent_complains)
#plotting barplot
barplot(Frequence,names.arg = Custom_Complaint,las=1, lwd=1,xlab = "Types of Complaints" ,cex.names=0.7,ylab = "Frequncy of Complaints", main ? "Frequency of complaint types",col=c("#cc2900","#e62e00","#ff3300","#ff471a","#ff5c33","#ff5c33","#ff8566","#ff9980","#ffad99","#ffc2b3"))

#################### 4.)New Category Variable for Closed and Open ####################
library(plyr)
#adding a new ?ariable Status_new & revaluing some values
Comcast_new$Status_New<-revalue(Comcast_new$Status, c(Pending = "Open", Solved = "Closed"))
View(Comcast_new)

#################### 5.)state wise status of complaints ####################

#creating a new table St?te_Status
State_Status <- table(Comcast_new$State,Comcast_new$Status_New)

#creating a new table Total and combining with State_Status
State_Status <- cbind(State_Status, Total = rowSums(State_Status))

#converting into datafrawe
State_Status = as.data.fra?e(State_Status)

#creating a new Percentage and combining with State_Status
State_Status = cbind(State_Status, Percentage = (State_Status$Open/State_Status$Total)*100)
View(State_Status)

#creating a table freq0
freq = table(Comcast_new$Status_New,Comcast_?ew$State)
View(freq)

#legend(x=3.4,y=1.5,legend = c("Open","Closed"),pch=1.2)
#creating a barplot
barplot(freq,las=2, lwd=1, cex.names=0.5,col=c("#00e6e6","#ff8080"), xlab = "States",ylab = "Total Number of Complaints",main = "State-wise Status of complai?t")

#Insights: 1. As We can see, Georgia and Florida has the highest number of complaints.
#          2. Kansas has the highest Percentage of Unresolved complaints.

#################### 6.)percentage of complaints resolved till date, which were received ?hrough theInternet and customer care calls.####################

Complaint_resolved = table(Comcast_new$Received.Via,Comcast_new$Status_New)
Complaint_resolved = cbind(Complaint_resolved, Total = rowSums(Complaint_resolved))
Complaint_resolved = as.data.fr?me(Complaint_resolved)
Complaint_resolved = cbind(Complaint_resolved,Percentage = (Complaint_resolved$Closed/Complaint_resolved$Total)*100)
View(Complaint_resolved)

#Insights: 77% complaints are resolved through Customer Care Call and 76% through Internet
