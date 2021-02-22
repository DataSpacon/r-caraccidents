################################## Decisiont tree#######################

#set working directory (cache)
setwd('C:/Users/SlawekPC/Desktop/Main Uni Assessment')

#install.packages("plyr")
#install.packages("party")
#install.packages("neuralnet")

library(party)
library(plyr)
library(neuralnet)

frm_AccidentData_Raw <- read.csv("dftRoadSafety_Accidents_2016.csv")

# look at the data structure
names(frm_AccidentData_Raw)
head(frm_AccidentData_Raw)
tail(frm_AccidentData_Raw)
summary(frm_AccidentData_Raw)
str(frm_AccidentData_Raw)

plot(frm_AccidentData_Raw$Speed_limit, col=rainbow(8), main="Number of accidents per speed limit", 
                                        xlab = "Speed limit",
                                        ylab = "Number of accidents")

#Take only required columns
frm_AccidentData <- frm_AccidentData_Raw[,c(7,8,9,15,17,18,19,20,21,23:31)]

names(frm_AccidentData)

#  remove unknown values
frm_AccidentData <- frm_AccidentData[frm_AccidentData$Road_Type!=-1 &
                                       frm_AccidentData$Light_Conditions!=-1 &
                                       frm_AccidentData$Weather_Conditions!=-1 &
                                       frm_AccidentData$Road_Surface_Conditions!=-1 &
                                       frm_AccidentData$Special_Conditions_at_Site!=-1
                               ,]

#create factors and replace values
frm_AccidentData$Accident_Severity <- as.factor(frm_AccidentData$Accident_Severity)
frm_AccidentData$Accident_Severity <- revalue(frm_AccidentData$Accident_Severity,
                                              c("1"="fatal",
                                                "2"="serious",
                                                "3"="slight"
                                                ))

frm_AccidentData$Speed_limit <- as.factor(Accident_Data$Speed_limit)

frm_AccidentData$X1st_Road_Class <- as.factor(frm_AccidentData$X1st_Road_Class)
frm_AccidentData$X1st_Road_Class <- revalue(frm_AccidentData$X1st_Road_Class,
                                        c("1"="Motorway",
                                          "2"="A(M)",
                                          "3"="A",
                                          "4"="B",
                                          "5"="C",
                                          "6"="Unclass"
                                          ))

frm_AccidentData$X2nd_Road_Class <- as.factor(frm_AccidentData$X2nd_Road_Class)
frm_AccidentData$X2nd_Road_Class <- revalue(frm_AccidentData$X2nd_Road_Class,
                                            c("1"="Motorway",
                                              "2"="A(M)",
                                              "3"="A",
                                              "4"="B",
                                              "5"="C",
                                              "6"="Unclass"
                                            ))

frm_AccidentData$Road_Type <- as.factor(frm_AccidentData$Road_Type)
frm_AccidentData$Road_Type <- revalue(frm_AccidentData$Road_Type,
                                  c("1"="Roundabout",
                                    "2"="One way",
                                    "3"="Dual carr",
                                    "6"="Single carr",
                                    "7"="Slip road",
                                    "9"="Unknown"
                                  ))

frm_AccidentData$Pedestrian_Crossing.Human_Control <- as.factor(frm_AccidentData$Pedestrian_Crossing.Human_Control)
frm_AccidentData$Pedestrian_Crossing.Human_Control <- revalue(frm_AccidentData$Pedestrian_Crossing.Human_Control,
                                                          c(
                                                            "0"="None ",
                                                            "1"="School patrol",
                                                            "2"="Other auth person",
                                                            "-1"="Data missing"
                                                          ))

frm_AccidentData$Pedestrian_Crossing.Physical_Facilities <- as.factor(frm_AccidentData$Pedestrian_Crossing.Physical_Facilities)
frm_AccidentData$Pedestrian_Crossing.Physical_Facilities <- revalue(frm_AccidentData$Pedestrian_Crossing.Physical_Facilities,
                                                                 c(
                                                                   "0"="None",
                                                                   "1"="Zebra",
                                                                   "4"="Pelican, puffin, toucan or similar non-junction pedestrian light crossing",
                                                                   "5"="Pedestrian phase at traffic signal junction",
                                                                   "7"="Footbridge or subway",
                                                                   "8"="Central refuge",
                                                                   "-1"="Data missing"
                                                                 ))

frm_AccidentData$Light_Conditions <- as.factor(frm_AccidentData$Light_Conditions)
frm_AccidentData$Light_Conditions <- revalue(frm_AccidentData$Light_Conditions,
                                         c(
                                           "1"="Daylight",
                                           "4"="Darkness - lights lit",
                                           "5"="Darkness - lights unlit",
                                           "6"="Darkness - no lighting",
                                           "7"="Darkness - lighting unknown"
                                         ))

frm_AccidentData$Junction_Detail <- as.factor(frm_AccidentData$Junction_Detail)
frm_AccidentData$Junction_Detail <- revalue(frm_AccidentData$Junction_Detail,
                                        c(
                                          "0"="Not at junc",
                                          "1"="Roundabout",
                                          "2"="Mini-roundabout",
                                          "3"="T or staggered",
                                          "5"="Slip road",
                                          "6"="Crossroads",
                                          "7"="More than 4 arms",
                                          "8"="Private drive",
                                          "9"="Other",
                                          "-1"="Data missing"
                                        ))

frm_AccidentData$Junction_Control <- as.factor(frm_AccidentData$Junction_Control)
frm_AccidentData$Junction_Control <- revalue(frm_AccidentData$Junction_Control,
                                         c(
                                           "0"="Not at junc",
                                           "1"="Authorised person",
                                           "2"="Auto traffic signal",
                                           "3"="Stop sign",
                                           "4"="Give way or uncontr",
                                           "-1"="Data missing"
                                         ))


frm_AccidentData$Weather_Conditions <- as.factor(frm_AccidentData$Weather_Conditions)
frm_AccidentData$Weather_Conditions <- revalue(frm_AccidentData$Weather_Conditions,
                                           c("1"="Fine no high winds",
                                                      "2"="Raining no high winds",
                                                      "3"="Snowing no high winds",
                                                      "4"="Fine + high winds",
                                                      "5"="Raining + high winds",
                                                      "6"="Snowing + high winds",
                                                      "7"="Fog or mist",
                                                      "8"="Other",
                                                      "9"="Unknown",
                                                      "-1"="Data missing"
                                                      ))

frm_AccidentData$Road_Surface_Conditions <- as.factor(frm_AccidentData$Road_Surface_Conditions)
frm_AccidentData$Road_Surface_Conditions <- revalue(frm_AccidentData$Road_Surface_Conditions,
                                                c(
                                                  "1"="Dry",
                                                  "2"="Wet or damp",
                                                  "3"="Snow",
                                                  "4"="Frost or ice",
                                                  "5"="Flood over 3cm",
                                                  "6"="Oil or diesel",
                                                  "7"="Mud",
                                                  "-1"="Data missing"
                                                ))

frm_AccidentData$Special_Conditions_at_Site <- as.factor(frm_AccidentData$Special_Conditions_at_Site)
frm_AccidentData$Special_Conditions_at_Site <- revalue(frm_AccidentData$Special_Conditions_at_Site,
                                                   c(
                                                     "0"="None",
                                                     "1"="Auto traff signal-out",
                                                     "2"="Auto signal part def",
                                                     "3"="Road sign or marking defective or obscured",
                                                     "4"="Roadworks",
                                                     "5"="Road surface defective",
                                                     "6"="Oil or diesel",
                                                     "7"="Mud",
                                                     "-1"="Data missing"
                                                   ))

frm_AccidentData$Carriageway_Hazards <- as.factor(frm_AccidentData$Carriageway_Hazards)
frm_AccidentData$Carriageway_Hazards <- revalue(frm_AccidentData$Carriageway_Hazards,
                                            c(
                                              "0"="None",
                                              "1"="Vehicle load on road",
                                              "2"="Other object on road",
                                              "3"="Previous accident",
                                              "4"="Dog on road",
                                              "5"="Other animal on road",
                                              "6"="Pedestrian in carriageway - not injured",
                                              "7"="Any animal in carriageway (except ridden horse)",
                                              "-1"="Data missing"
                                            ))

frm_AccidentData$Urban_or_Rural_Area <- as.factor(frm_AccidentData$Urban_or_Rural_Area)
frm_AccidentData$Urban_or_Rural_Area <- revalue( frm_AccidentData$Urban_or_Rural_Area,
                                              c(
                                                "1"="Urban",
                                                "2"="Rural",
                                                "3"="Unallocated"
                                              ))

frm_AccidentData$Did_Police_Officer_Attend_Scene_of_Accident <- as.factor(frm_AccidentData$Did_Police_Officer_Attend_Scene_of_Accident)
frm_AccidentData$Did_Police_Officer_Attend_Scene_of_Accident <- revalue(frm_AccidentData$Did_Police_Officer_Attend_Scene_of_Accident,
                                                                    c("1"="Yes",
                                                                      "2"="No",
                                                                      "3"="Self Completion"
                                                                    ))
head(frm_AccidentData)


# Create only 2 factor classification
frm_AccidentData$Fatal <- ifelse(frm_AccidentData$Accident_Severity == "slight", "slight","serious")
frm_AccidentData$Fatal <- as.factor(frm_AccidentData$Fatal)

str(frm_AccidentData)

# partitiond data into training and test
set.seed(123)
pd <- sample(2,nrow(frm_AccidentData),replace = TRUE, prob =c(0.6,0.4))

Accident_Train <- frm_AccidentData[pd==1,] 
nrow(Accident_Train)
Accident_Test <- frm_AccidentData[pd==2,] 
nrow(Accident_Test)

dim(Accident_Train)
dim(Accident_Test)

Accident_Tree_Sev <- ctree(Fatal~
                             Road_Type+
                             Number_of_Vehicles+
                             Number_of_Casualties+
                             Special_Conditions_at_Site +
                             X1st_Road_Class +
                             Junction_Detail +
                             Light_Conditions+
                             Speed_limit+
                             Urban_or_Rural_Area+
                             Did_Police_Officer_Attend_Scene_of_Accident+
                             Road_Surface_Conditions
                           ,Accident_Train)
# draw the tree
plot(Accident_Tree_Sev)
plot(Accident_Tree_Sev,type="simple")
help("ctree")


# create confusion mattrix 
ConfMattrix_DT_tree <- table(predict(Accident_Tree_Sev,newdata = Accident_Test),Accident_Test$Fatal)
ConfMattrix_DT_tree <- table(predict(Accident_Tree_Sev),Accident_Train$Fatal)
print(ConfMattrix_DT_tree)

# calculate classification accuracy and error on test data set
sum(diag(ConfMattrix_DT_tree))/sum(ConfMattrix_DT_tree)
1-	sum(diag(ConfMattrix_DT_tree))/sum(ConfMattrix_DT_tree)


############################### Neural networks###########

#set working directory (cache)
setwd('C:/Users/SlawekPC/Desktop/Uni/Assesment project/Road Accidents')

Accident_Data <- read.csv("dftRoadSafety_Accidents_2016.csv")

names(Accident_Data)
head(Accident_Data)
tail(Accident_Data)
summary(Accident_Data)
str(Accident_Data)


#Take only required data
Accident_Data <- Accident_Data[,c(7,8,9,15,17,18,19,20,21,23:31)]

#  remove unknown values
Accident_Data <- Accident_Data[Accident_Data$Road_Type!=-1 &
                                 Accident_Data$Light_Conditions!=-1 &
                                 Accident_Data$Weather_Conditions!=-1 &
                                 Accident_Data$Road_Surface_Conditions!=-1 &
                                 Accident_Data$Special_Conditions_at_Site!=-1
                               ,]

names(Accident_Data)

Accident_Data$Accident_Severity <- as.factor(Accident_Data$Accident_Severity)
str(Accident_Data)
#create factors:
Accident_Data$Accident_Severity <- revalue(Accident_Data$Accident_Severity,c("1"="fatal",
                                                                             "2"="serious",
                                                                             "3"="slight"))

# check data dimension
str(Accident_Data)

#  Add factor column, we are interested in only serious accidents
Accident_Data$Fatal <- ifelse(Accident_Data$Accident_Severity == "slight", "slight","serious")
#Accident_Data$Fatal <- ifelse(Accident_Data$Accident_Severity == "Fatal", "Fatal","No")
Accident_Data$Fatal <- as.factor(Accident_Data$Fatal)


#reproduce results by using sed function
set.seed(123)

#pd <- sample(2,nrow(Accident_Data),replace = TRUE, prob =c(0.9,0.1))
pd <- sample(2,nrow(Accident_Data),replace = TRUE, prob =c(0.6,0.4))
pd

Accident_Train <- Accident_Data[pd==1,] 
nrow(Accident_Train)
Accident_Test <- Accident_Data[pd==2,] 
nrow(Accident_Test)

dim(Accident_Train)
dim(Accident_Test)

install.packages("neuralnet")
library(neuralnet)
# model matrix
# code training set
Accident_NN_Train <- model.matrix(~0 +
                                    Speed_limit+
                                    Urban_or_Rural_Area+
                                    Number_of_Vehicles+
                                    Light_Conditions+
                                    Weather_Conditions+
                                    Road_Surface_Conditions+
                                    Number_of_Vehicles+
                                    Fatal
                                  ,data = Accident_Train)
head(Accident_NN_Train,1)

#code test data
Accident_NN_Test <- model.matrix(~0 +
                                   Speed_limit+
                                   Urban_or_Rural_Area+
                                   Number_of_Vehicles+
                                   Light_Conditions+
                                   Weather_Conditions+
                                   Road_Surface_Conditions+
                                   Number_of_Vehicles+
                                   Fatal
                                 ,data = Accident_Test)
head(Accident_NN_Test)
names(Accident_NN_Test)
# build the network

Accident_Net <- neuralnet(Fatalslight~
                            Speed_limit20+
                            Speed_limit30+
                            Speed_limit40+
                            Speed_limit50+
                            Speed_limit60+
                            Speed_limit70+
                            Speed_limitNULL+
                            Urban_or_Rural_Area+
                            Number_of_Vehicles+
                            Light_Conditions+
                            Weather_Conditions+
                            Road_Surface_Conditions+
                            Number_of_Vehicles,

                          Accident_NN_Train,
                          hidden = 5, lifesign = "full", linear.output=FALSE, threshold = 0.1)

plot(Accident_Net, rep ="best")

Accident_Net2 <- neuralnet(Fatalslight ~
                             Speed_limit20+
                             Speed_limit30+
                             Speed_limit40+
                             Speed_limit50+
                             Speed_limit60+
                             Speed_limit70+
                             Speed_limitNULL+
                             Urban_or_Rural_Area+
                             Number_of_Vehicles+
                             Light_Conditions+
                             Weather_Conditions+
                             Road_Surface_Conditions+
                             Number_of_Vehicles,
                           
                           Accident_NN_Train,
                          hidden = 20, lifesign = "minimal", linear.output=FALSE, threshold = 0.1)
plot(Accident_Net2, rep ="best")


Accident_NN_Test2_temp <- subset(Accident_NN_Test, select =  c("Speed_limit20",           "Speed_limit30"     ,      "Speed_limit40"       ,    "Speed_limit50"          ,
 "Speed_limit60" ,          "Speed_limit70"      ,     "Speed_limitNULL"      ,   "Urban_or_Rural_Area"    ,
 "Number_of_Vehicles",      "Light_Conditions"    ,    "Weather_Conditions"    ,  "Road_Surface_Conditions"))

# get results for hidden 5
Accident_Net.results <- compute(Accident_Net,Accident_NN_Test2_temp)
results <- data.frame(actual = Accident_NN_Test[,13], prediction =Accident_Net.results$net.result)
results


# roundup results
roundedresults<-sapply(results,round,digits=0)

roundedresultsdf=data.frame(roundedresults)

attach(roundedresultsdf)
ConfMattrix_NN <- table(actual,prediction)
ConfMattrix_NN

# calculate classification accuracy and error on test data set
sum(diag(ConfMattrix_NN))/sum(ConfMattrix_NN)
1- sum(diag(ConfMattrix_NN))/sum(ConfMattrix_NN)


# get results for hidden 20
Accident_Net2.results <- compute(Accident_Net2,Accident_NN_Test2_temp)
results <- data.frame(actual = Accident_NN_Test[,13], prediction =Accident_Net2.results$net.result)
results

# roundup results
roundedresults<-sapply(results,round,digits=0)

roundedresultsdf=data.frame(roundedresults)

attach(roundedresultsdf)
ConfMattrix_NN <- table(actual,prediction)
ConfMattrix_NN

# calculate classification accuracy and error on test data set
sum(diag(ConfMattrix_NN))/sum(ConfMattrix_NN)
1- sum(diag(ConfMattrix_NN))/sum(ConfMattrix_NN)

############################### Clustering ###########

setwd('C:/Users/SlawekPC/Desktop/Main Uni Assessment/Clustering R scripts')

#install.packages("reshape2")
#install.packages("factoextra")
library(reshape2) 
library(factoextra) 
library(plyr)

normalise <- function(dv)
{
  return (((dv- min(dv))/(max(dv)-min(dv))*(1-0))+0)
}

Make_Model <- read.csv("MakeModel2016.csv")
names(Make_Model)
head(Make_Model)
tail(Make_Model)
summary(Make_Model)
str(Make_Model)

# make per vehicle /  manoeuvre
Make_Model_Clust <- Make_Model[,c(23,6,13)]
names(Make_Model_Clust)
head(Make_Model_Clust)
summary(Make_Model_Clust)
str(Make_Model_Clust)

#  remove unknown values
Make_Model_Clust <- Make_Model_Clust[Make_Model_Clust$make!="NULL" &
                                       Make_Model_Clust$Vehicle_Manoeuvre!=-1  &
                                       Make_Model_Clust$X1st_Point_of_Impact!=-1,]

# make as factor
Make_Model_Clust$Vehicle_Manoeuvre <- as.factor(Make_Model_Clust$Vehicle_Manoeuvre)
Make_Model_Clust$Vehicle_Manoeuvre <- revalue(Make_Model_Clust$Vehicle_Manoeuvre,
                                              c("1"="Reversing",
                                                "2"="Parked",
                                                "3"="Waiting to go - held up",
                                                "4"="Slowing or stopping",
                                                "5"="Moving off",
                                                "6"="U-turn",
                                                "7"="Turning left",
                                                "8"="Waiting to turn left",
                                                "9"="Turning right",
                                                "10"="Waiting to turn right",
                                                "11"="Changing lane to left",
                                                "12"="Changing lane to right",
                                                "13"="Overtaking moving vehicle - offside",
                                                "14"="Overtaking static vehicle - offside",
                                                "15"="Overtaking - nearside",
                                                "16"="Going ahead left-hand bend",
                                                "17"="Going ahead right-hand bend",
                                                "18"="Going ahead other"
                                              ))
Make_Model_Clust$X1st_Point_of_Impact <- as.factor(Make_Model_Clust$X1st_Point_of_Impact)
Make_Model_Clust$X1st_Point_of_Impact <- revalue(Make_Model_Clust$X1st_Point_of_Impact,
                                                 c("0"="Did not impact",
                                                   "1"="Front",
                                                   "2"="Back",
                                                   "3"="Offside",
                                                   "4"="Nearside"
                                                 ))
str(Make_Model_Clust)

# Pivot data set
Make_Model_Pivot <- dcast(Make_Model_Clust, make ~ Vehicle_Manoeuvre + X1st_Point_of_Impact,length)
str(Make_Model_Pivot)
head(Make_Model_Pivot)

# set value makes as rownames
rownames(Make_Model_Pivot) <- Make_Model_Pivot[,1]
Make_Model_Pivot[,1] <- NULL
str(Make_Model_Pivot)
summary(Make_Model_Pivot)
head(Make_Model_Pivot)

write.table(Make_Model_Pivot,file = "MakeModel2016_Pivot.csv",sep = ",", row.names = TRUE, col.names = TRUE)

#normalize data
Car.Make <- rownames(Make_Model_Pivot)
Make_Model_Pivot_n <- as.data.frame(lapply(Make_Model_Pivot,normalise))
rownames(Make_Model_Pivot_n) <- Car.Make
head(Make_Model_Pivot_n)

############ hierarchical clustering  by distance similarity
distance <- dist(Make_Model_Pivot_n,method = "euclidean")
fviz_dist(distance,show_labels = FALSE)
Make_Model_Pivot_n.hclust <- hclust(distance)
Make_Model_Pivot_n.hclust

plot(Make_Model_Pivot_n.hclust, hang = 1)

distance <- dist(Make_Model_Pivot_n,method = "maximum")
fviz_dist(distance,show_labels = FALSE)
Make_Model_Pivot_n.hclust <- hclust(distance)
Make_Model_Pivot_n.hclust

plot(Make_Model_Pivot_n.hclust)

distance <- dist(Make_Model_Pivot_n,method = "manhattan")
fviz_dist(distance,show_labels = FALSE)
Make_Model_Pivot_n.hclust <- hclust(distance)
Make_Model_Pivot_n.hclust

plot(Make_Model_Pivot_n.hclust)

distance <- dist(Make_Model_Pivot_n,method = "canberra")
fviz_dist(distance,show_labels = FALSE)
Make_Model_Pivot_n.hclust <- hclust(distance)
Make_Model_Pivot_n.hclust
plot(Make_Model_Pivot_n.hclust)
############ hierarchical clustering - compare by link

hclust.average <- hclust(distance, method = "average")
plot(hclust.average)
rect.hclust(hclust.average,4)


hclust.single <- hclust(distance, method = "single")
plot(hclust.single)
rect.hclust(hclust.single,4)

hclust.centroid <- hclust(distance, method = "centroid")
plot(hclust.centroid)
rect.hclust(hclust.centroid,3)

hclust.complete <- hclust(distance, method = "complete")
plot(hclust.complete)
rect.hclust(hclust.complete,3)
rect.hclust(hclust.complete,4)
rect.hclust(hclust.complete,6)


############ K - Means
# check clustering tendency
tendency <- get_clust_tendency(Make_Model_Pivot_n,n = nrow(Make_Model_Pivot_n)-1,graph = TRUE)
tendency$hopkins_stat
fviz_nbclust(Make_Model_Pivot_n,kmeans,method = "wss")

# perform k-means clustering with k = 3
set.seed(123)
km.fit <- kmeans(Make_Model_Pivot_n,3,nstart = 1)
km.fit$cluster
km.fit$size

fviz_cluster(km.fit,Make_Model_Pivot_n, revel = TRUE, main = "K-means")

# remove outliers
Make_Model_Pivot_n2 <- subset(Make_Model_Pivot_n, !(
  rownames(Make_Model_Pivot_n)) %in% 
    c("FORD", "VAUXHALL", "VOLKSWAGEN"))

set.seed(123)
km.fit <- kmeans(Make_Model_Pivot_n2,3,nstart = 1)
km.fit$cluster
km.fit$size

fviz_cluster(km.fit,Make_Model_Pivot_n2, revel = TRUE, main = "K-means")
