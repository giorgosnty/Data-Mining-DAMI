
# *********************************************
# DAMI Preprocessing Exercise R file
# Complete the codes to complete the assignment
# *********************************************

# 1. Import data for analysis to R environment
# Downloaded "Adult" dataset from UCI Machine Learning Repository
# URL http://archive.ics.uci.edu/ml/datasets/Adult
# Import dataset in adult_db
# Missing values are represented as "?" in data file, make sure that R read them as missing values (NAs)
# HINT: use read.table() function, use ?read.table for more help
# ------------------------------------------------------------------------------------------------------ #
adult_db <- read.table(file = "http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data",
                               header = FALSE, sep = ",", na.strings = "?", strip.white = TRUE, 
                               stringsAsFactors = FALSE)
  
  
  
# Assign attribute names (column names) to the data we just imported
# Attribute names are in separate file "adult.names", scroll down to the bottom of this file
# Attribute names such as ("age", "workclass", "fnlwgt",...)
# Last column of the dataset adult.db with values ">50K" and "<=50K" does not have name, 
# this is the class attribute, so we just name it as "class"
# ----------------------------------------------------------------------------------------- #
names(adult_db) = c("age",
                    "workclass",
                    "fnlwgt",
                    "education",
                    "education_num",
                    "marital_status",
                    "occupation",
                    "relationship",
                    "race",
                    "sex",
                    "capital_gain",
                    "capital_loss",
                    "hours_per_week",
                    "native_country",
                    "class")

  
  
# Inspect data set in tabular form
# -----------------------------
fix(adult_db)

# Change class labels to 1 (adults who earn more than 50K) and 0 (adults who earn less than or equal to 50K)
# ----------------------------------------------------------------------------------------------
adult_db$class[adult_db$class==">50K"] <- 1
adult_db$class[adult_db$class=="<=50K"] <- 0


# 2. Check for missing values
# Write code to check how many missing values each attribute has
# Hint: use "apply()" function along columns of "adult.db", for each column (attribute) find how many NAs are there
# is.na(x) function can be used to see if x has NA, ?is.na for help
# --------------------------------------------------------------------------------------------------------------- #
# ****** YOUR CODE HERE ******* #


apply(adult_db, 2, function(x) sum(is.na(x)))
  
# Delete records (rows) with any missing value
# --------------------------------------- #
adult_db_nomiss <-na.omit(adult_db)

  
  
  
# 3. We will take only small chunk of the data for our experimental purpose.
# So, randomly select 1000 records from among 30 thousand records in the dataset.
# ------------------------------------------------------------------------------- #
set.seed(1013)
idx = sample(1:nrow(adult_db_nomiss),1000)
adult_db_lim = adult_db_nomiss[idx,]
row.names(adult_db_lim) <- NULL


  
  
  
# Examine attributes of the dataset
# 3a. Plot histogram for numeric attribute "age", with 50 breaks, show main title and attribute name on the plot.
# HINT: use hist() function for plotting histogram, ?hist to see how to use it.
# --------------------------------------------------------------------------------------------------------

# ******* YOUR CODE FOR HISTOGRAM PLOT GOES HERE ******** #

class_over_50 <- (adult_db_lim$class==1)

hist(adult_db_lim$age[!class_over_50],breaks = 50,main ="Age Distribution",ylim=c(0,30),xlab="Age",ylab="frequency" ,col = rgb(1,0,0,1))
hist(adult_db_lim$age[class_over_50],breaks = 50,ylim=c(0,30),xlab="Age",ylab="frequency", col = rgb(0,0,1,1), add=T)
legend("topright",legend=c(">50K","<=50K"),col=c("blue","red"),pch=20,cex=1.45)

# 3b. Plot barchart for categorical attribute "race", show legend, attribute name and main title for the plot.
# HINT: use barplot() function for plotting barchars, ?barplot for more help.
# --------------------------------------------------------------------------------------

# ******* YOUR CODE FOR BAR CHART GOES HERE ******* #


colors<- c("black","red", "green", "blue", "cyan")
races<- c("Amer-Indian-Eskimo", "Asian-Pac-Islander", "Black","Other","White")

height_of_bar <- table(adult_db_lim$race)
barplot(height_of_bar, col=colors,
        main = "Race of Adults", 
        names.arg = races,
        cex.names = 0.8)

legend("topleft",legend=races ,col=colors,pch=20,cex=0.85)


  
# 3c. Plot a boxplot for attribute "Age" and show possible outlier for this attribute
# HINT: ?boxplot for more help
# ---------------------------------------------------------------------------------------------
# ****** YOUR CODE GOES HERE ***** #

age_boxplot <- boxplot(adult_db_lim$age, pch=20, col="red", main = "Age Of Adults")


# show possible outlier values
boxplot.stats(adult_db_lim$age)$out



#4 Create new data set from our latest dataset with only numeric attributes
# ------------------------------------------------------------------------
adult_db_numeric <- adult_db_lim[,c("age", "fnlwgt", "education_num", "capital_gain", "capital_loss", "hours_per_week")]
class_val <- as.numeric(adult_db_lim[,c("class")])





# Standardize numeric attributes in "adult_db_numeric" dataset.
# mean = 0 and sd = 1 for all numeric attributes
# -----------------------------------------------------------------------------------------------

age_standarized <- scale(adult_db_numeric$age)
fnlwgt_standarized <- scale(adult_db_numeric$fnlwgt)
education_num_standarized <- scale(adult_db_numeric$education_num)
capital_gain_standarized <- scale(adult_db_numeric$capital_gain)
capital_loss_standarized <- scale(adult_db_numeric$capital_loss)
hours_per_week_standarized <- scale(adult_db_numeric$hours_per_week)

adult_db_numeric$age<-age_standarized
adult_db_numeric$fnlwgt<-fnlwgt_standarized
adult_db_numeric$education_num<-education_num_standarized
adult_db_numeric$capital_gain<-capital_gain_standarized
adult_db_numeric$capital_loss<-capital_loss_standarized
adult_db_numeric$hours_per_week<-hours_per_week_standarized

adult_db_num_std <- adult_db_numeric


  
# we can check the mean and standard deviation of the standardized data
# ------------------------------------------------------------------
apply(adult_db_num_std, 2, mean)
apply(adult_db_num_std, 2, sd)



  
  
# 5a. Run Principal Component Analysis (PCA) on the numeric dataset from above "adult_db_num_std"
# plot the first 2 principal components
# HINT: for class specific colours, in plot(...) command use parameter col = (class_val + 2)
# HINT: ?prcomp to know about the parameters
# ------------------------------------------------------------------------------------------

# ******** YOUR CODE FOR GETTING PRINCIPAL COMPONENTS GOES HERE ******** #

pr.out <- prcomp(adult_db_num_std, scale = TRUE, center = TRUE)
names(pr.out)
head(pr.out$x)

principal_components <- pr.out$x
  
  
#******** PLOT FOR FIRST TWO PRINCIPAL COMPONENTS GOES HERE ****** #  
plot(principal_components[,1:2], col = ( class_val+2), pch = 20, main = "First two Prinsipal Components")
legend("topleft",legend=c(">50K","<=50K"),col=c("green","red"),pch=20,cex=0.75)

        
        
        
        
# 5b. Plot percentage of the variance explained by principal components
# ----------------------------------------------------------------------------
# write a code to show proportion of variance explained by each Principal Component
# Standard deviation are stored as "sdev"

# *** YOUR CODE TO FIND PROPORTION OF VARIANCE EXPLAINED *** #
pr.var <- (pr.out$sdev)^2
pve <- pr.var/sum(pr.var)

# *** PLOT VARIANCE EXPLAINED BY PRINCIPAL COMPONENTS AND CUMULATIVE PROPORTION OF VARIANCE *** #
# par(...) for a plot with 2 figures.

par(mfrow=c(1,2), oma=c(0,0,2,0))
plot(pve, xlab = "PC", ylab = "Variance", type = "b",col ="red", ylim = c(0,1))
# use cumsum(pve) for cumulative sum of variance
#plot(#*** YOUR CODE, fill up parameters to plot cumulative proportion of variance explained ***#)
#mtext("Proportion of variance explained by Principal Components", outer=TRUE, cex=1.2)
  
plot(cumsum(pve), xlab = "PC", ylab = "Cumulative variance", type = "b",col ="red", ylim = c(0,1))
mtext("Proportion of Variance explained by PC", outer = TRUE)
par(mfrow = c(1,1))


# 5c. Write as comments how many principal components would you use to capture at least 50% variance
# and 90% varience respectively.
   
#Answer:         
#From the plot we can see that we need 3 principal components to capture at least 50%
#variance and 6 to capture 90%. 
#To be more convincing,I printed pve and I took 
# "0.2289799 0.1748101 0.1617089 0.1588597 0.1488012 0.1268402".
#So with 3 principal components we get the sum of the first 3 that is approximately 0.56.
#With 5 we get approximately 0,88 so we need 6 to capture 90%.
