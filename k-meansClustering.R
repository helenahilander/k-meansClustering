
#idea of this practise:
#clustering users into groups for making marketing more precise
#data is a from the New York Citi Bike sharing program. 
#Citi Bike is the largest bike sharing program in the United States and serves various parts of the New York city

#we have file citibike.csv that contains trip information for the bike rides for the month of July 2013:
# . tripduration: Time duration of the trip (in seconds)
# . startstation: Name of the start station
# . endstation: Name of the end station
# . gender: Gender of user (1 = male, 2 = female)
# . age: Age of the user
# . day: Day on which the trip was started (Mon, Tue, Wed, Thu, Fri, Sat, Sun)
# . starttime: Start time of the trip in unit of hours (measured from 0 (12am) to 23 (11pm))

#------------------------------------------------------

#structure of analysis

#1. read file and get to know data
#2. convert categorical day variable into 7 binary variables
#3. normalize data
#4. apply k-means clustering
#5. discussion

#---------------------------------------------------------
#1.read file and get to know data

#read file
setwd("..")
citi <- read.csv("citibike.csv", stringsAsFactors = F )
#stringsAsFactors = F prevents converting character vectors to factors


#how many stations we have?
starts <- unique(citi$startstation) #329 start stations
#unique removes dublicate values
ends <- unique(citi$endstation) #329 end stations 
stations <- c(starts, ends)
stations <- unique(stations) #329 stations overall


#average durations of trips taken by customes every day:
tapply(X = citi$tripduration, INDEX  = citi$day, FUN= mean)
#tapply applies FUN to X in groups specified in INDEX

#Fri      Mon      Sat      Sun      Thu      Tue      Wed 
#832.3580 853.6248 894.2661 887.5528 865.7822 857.4895 843.5679 




#how many bikes is rented every hour?
hour <-tapply(X=citi$starttime, INDEX = citi$starttime, FUN= length)
#checking that tapply really counted the amounts of bikes right: 
nrow(subset(citi, citi$starttime == 23)) == hour["23"] #true

#0     1     2     3     4     5     6     7     8     9 
#7675  4184  2424  1478  1151  3117 13410 29131 52420 41442 

#10    11    12    13    14    15    16    17    18    19 
#25888 26569 33166 34584 32547 34654 43676 65277 65684 51495 

#20    21    22    23 
#37399 26727 20176 13464 

which.max(hour) #6pm people rent most
which.min(hour) #4am people rent least


#what is the proportion of female renters
length(which(citi$gender==2))/ length(citi$gender) #23.5%


#--------------------------------------------------------------
#2. convert categorical day variable into 7 binary variables
#why? easier to predict
#for example if the day is sunday, in col Sun we have 1, and in other columns 0.

#create new dataframe for weekdays:
#first create right size matrix
m <- matrix(0, nrow = nrow(citi), ncol = 7)
#convert to dataframe
df <- as.data.frame(m)
#give columnnames
days <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
colnames(df) <- days

for(i in 1:7){df[,i] <- ifelse(citi$day == days[i], 1,0 )}
#what for loop does:
#if row x has value citi$day == "Mon" give value one to df[x,"Mon"] else keep 0. Repeat this to all days and all rows

#add binary columns to citi
citi <- cbind(citi, df)


#------------------------------------------------------------------
#3. normalize data
#why? otherwise large values, like tripduration will dominate when we calculate distances in clustering

if(!require(caret)){
  install.packages("caret")
  library(caret)
}

?preProcess
#Pre-processing transformation (centering, scaling etc.) can be estimated from the training data 
#and applied to any data set with the same variables with predict()

#calculate mean and sd of columns of citi except startstation, endstation, day
preproc <- preProcess(citi)
preproc$mean
preproc$std
#extract mean and sd from data
citi.Norm <- as.data.frame(predict(preproc, citi))

#check that normalizing succeeded:

#first remove columns with no numerical values (no mean can be calculated from then)
non.Num.Cols <- which(sapply(X=citi.Norm, class) != "numeric")
#sapply returns a list of the same length as X, each element of which is the result of applying FUN to the corresponding element of X.

#calculate columnwise mean and sd:

apply(X= citi.Norm[,-non.Num.Cols], MARGIN = 2, mean) #means close to 0

#tripduration        gender           age     starttime 
#-8.565793e-17 -9.798368e-18  1.452381e-17 -2.986434e-15 
#Mon           Tue           Wed           Thu 
#2.098075e-17  1.067086e-18  1.459464e-16 -2.507719e-17 
#Fri           Sat           Sun 
#-6.410337e-17  1.266674e-16 -2.993903e-17 

apply(X= citi.Norm[,-non.Num.Cols], MARGIN = 2, sd) #standard deviations 1 --> all good

#tripduration       gender          age    starttime          Mon 
#1            1            1            1            1 
#Tue          Wed          Thu          Fri          Sat 
#1            1            1            1            1 
#Sun 
#1 



#---------------------------------------------------------------------
#4. apply k-means clustering

#why not do hierarchial clustering?: 
#TOO BIG DATA. Hierarchial clustering is computationally heavy (we have to calculate 667738*667737 distance  values only at the first round.

#we must reomve all non numerical parameters from the data since we calculate distances
non.Num.Cols <- which(sapply(X=citi.Norm, class) != "numeric")
citi.Norm.num <- citi.Norm[, -non.Num.Cols]

#explanations of function do_kmeans:
#does k-means clustering with initial centroids from min_centers to max_centers,
# with max 100 iterations of algorithm and 20 different sets tested for intial cluster centroids.
#returns table of results


do_kmeans <- function( min_centers, max_centers){



  #create table for results:
  #first create right size matrix
  m <- matrix(0, nrow= max_centers-min_centers +1, ncol = 4)
  #convert to dataframe
  df <- as.data.frame(m)
  #give columnnames and rownames
  cn <- c("tot.withinss", "iter", "ifault", "size")
  colnames(df) <- cn
  rownames(df) <- seq(min_centers, max_centers, 1)
  
  for(i in min_centers:max_centers){
    #kmean with i centers, max 100 iterations of algorithm, 20 different sets tested for intial cluster centroids
    k <- kmeans(citi.Norm.num, centers = i, iter.max = 100, nstart = 20)
    #save results to df
    df[as.character(i),1]  <- k$tot.withinss
    df[as.character(i),2]  <- k$iter
    df[as.character(i),3]  <- k$ifault
    df[as.character(i),4]  <- paste0(k$size, collapse = ",")
  }
  
  return(df)

}

min_centers <- 2
max_centers <- 11

#set seed for making results replicable, what we would notice without the seed,
#cluster change quite a lot with every run: there is no only one right answer
set.seed(100)
results <- do_kmeans(min_centers, max_centers)
results
#      tot.withinss iter ifault
#2       6526965    1      0
#3       5715445    3      0
#4       4967324    3      0
#5       4168225    3      0
#6       3429730    3      0
#7       2666550    3      0
#8       2390187    2      0
#9       2459517    5      0
#10      2219055    3      0
#11      2147899    4      0


#which amount of clusters have smallest tot.withinss
which.min(results[,1]) #10 clusters


#look results of 10 clusters more carefully:
#kmean with 10 centers, max 10 iterations of algorithm, 20 different sets tested for intial cluster centroids
set.seed(100)
k <- kmeans(citi.Norm.num, centers = 10, iter.max = 10, nstart = 20)

centers <- k$centers
centers
#tripduration      gender           age     starttime        Mon         Tue        Wed
#1  26.149334543  0.16835693 -3.541962e-05  0.2314449666  0.2036872 -0.01147791 -0.1339444
#2   0.003831515  0.18315589 -6.393024e-01  0.0534489975 -0.4372836 -0.48555563 -0.4882786
#3  -0.012917251 -0.01317644  2.625888e-02 -0.0105123293 -0.4372836 -0.48555563 -0.4882786
#4  -0.022110203 -0.02173882  3.514668e-02  0.0147589066 -0.4372836  2.05949316 -0.4882786
#5   0.042235987  1.80429727 -1.158291e-01  0.0568232802  2.2868421 -0.48555563 -0.4882786
#6   0.009198465 -0.13223038  1.126398e+00 -0.0525112358 -0.4372836 -0.48555563 -0.4882786
#7  -0.027508922 -0.03076554  6.208221e-03 -0.0905383431 -0.4372836 -0.48555563 -0.4882786
#8   0.001185646  0.08493623 -1.269010e-01  0.0261103874 -0.4372836 -0.48555563 -0.4882786
#9  -0.019077658 -0.02018798  3.927514e-02  0.0132153193 -0.4372836 -0.48555563  2.0480080
#10 -0.042490924 -0.55423157  6.884530e-02  0.0002743592  2.2868421 -0.48555563 -0.4882786
#Thu         Fri         Sat          Sun
#1   0.05245555 -0.07417693 -0.04092613  0.006003032
#2  -0.38164199 -0.37336420  2.94892855 -0.339893041
#3   2.62025285 -0.37336420 -0.33910571 -0.339893041
#4  -0.38164199 -0.37336420 -0.33910571 -0.339893041
#5  -0.38164199 -0.37336420 -0.33910571 -0.339893041
#6  -0.38164199 -0.37336420  2.94892855 -0.339893041
#7  -0.38164199  2.67834598 -0.33910571 -0.339893041
#8  -0.38164199 -0.37336420 -0.33910571  2.942097608
#9  -0.38164199 -0.37336420 -0.33910571 -0.339893041
#10 -0.38164199 -0.37336420 -0.33910571 -0.339893041

#we have found subgroups (=clusters) that differ in some parameters from average population:
#for example:
#group 1 stands out as group of people doing longer travels
#group 5 has more females than average sample and does more trips on monday compared to average people
#group 6 has higher average age than whole sample and does more trips on Saturday than people on average




#--------------------------------------------------
#optional: 
#scale back to normal to more interpretable results:

#get original data mean values
mean <- apply(citi[,-non.Num.Cols], MARGIN = 2,mean )
#create matrix
mean.m <- matrix(mean, nrow = 10, ncol = length(mean), byrow = T)
#get original data sd
sd <-  apply(citi[,-non.Num.Cols], MARGIN = 2,sd )
#create matrix
sd.m <- matrix(sd, nrow = 10, ncol = length(sd), byrow = T)

#scale back
centers.orig <- centers*sd.m + mean.m
centers.orig




#-----------------------------------------------
#5. discussion

#What do we actually get with the results from k-means clustering? What the algorithm does?
#Algorithm sorts the data into groups.
#data is sorted such that every point in one group has 
#shortest euclidean distance to the mean value of the group in comparison to the mean values of other groups. 

#euclidean distance between point x and centroid 1 is calculated:
#sqrt[(x_tripduration - mean1_tripduration)^2 + (x_gender - mean1_gender)^2 + ..+ (x_Fri - mean1_Fri)^2]
#we can see from the formula that all variables has equal weight in the mean value. (This could be changed with weight parameters)



#we can see from the results, that most parameters get full range of values inside clusters, and do not represent any subset, instead whole population. 
#(We can see from the normalized mean of the cluster. If it is close to the original data mean, 0,  cluster represents the whole data from the part of this specific parameter) 

#However in some clusters there are some parameters whose mean values are higher than 0. 
#This means that custer represents subgroup which differs from the average population. 

#how one makes use of this?
#One can learn from subgroups that differ from population in certain parameters 
#(Like in here, we found group of women who does six times more trips on monday than average population. 
#Other days they do trips like an average population, their age is average and time is average as well.)


#are these results reliable?
#mean value is not very robust for outliers. One should also check median value, when analyzing subgroups more carefully.
#one should also check the cluster size: Is the size of the subgroup significant enough?

#as discussed before, without set.seed() clusters would change in every run and results would differ.
#there are many different kinds of subset combos to be found. It is up to the analyser to decide which clusters seem most reasonable considering their field and data.

