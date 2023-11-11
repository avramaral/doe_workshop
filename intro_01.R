### First Session for DoE Workshop
### Introduction to R

# Set working directory to source file location (only works in "RStudio")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Reading data from files

## Read data from a .txt file
data1 <- read.table(url("https://raw.githubusercontent.com/avramaral/doe_workshop/main/Data/Human_data.txt"))
str(data1)   # presents the structure of the data object
head(data1)  # Show the first five values in the object

## The data file has a header and was not correctly read.
## Fix this:
data1 <- read.table(url("https://raw.githubusercontent.com/avramaral/doe_workshop/main/Data/Human_data.txt"), header = T)
str(data1)
head(data1)

## Read data from csv file
data1 <- read.csv(url("https://raw.githubusercontent.com/avramaral/doe_workshop/main/Data/Human_data.csv"))
str(data1)
head(data1)

## View the data 
View(data1) # opens a spreasheet-like window with the data

## Subsetting/Slicing
data1[1:5, 3:6]                               # Extract values in rows 1 to 5 and columns 3 to 6
data1[1:5, c('Gender','Salary','blood_type')] # Rows 1 to 5 and named columns
data1[1:5, c(2,8,9)]                          # Same as above

## Access variables with $ notation
## object$variable extracts variable from data object
data1$Ocupation[1:5] 

## Body Mass Index
## Define a new variable in the data frame with bmi
## bmi = weight / height^2

data1$bmi = data1$Weight_kg/((data1$Height_cm/100)^2)
head(data1)

## Another way to do this using within
data1 <- within(data1, {bmi = Weight_kg/(Height_cm/100)^2})
head(data1)

## Use round to reduce the number of decimals
data1$bmi = round(data1$Weight_kg/((data1$Height_cm/100)^2),2)
head(data1)


## Use the function cut to create a new variable 
## bmi_cat in data1 by dividing the subjects 
## into four categories according to the 
## value of bmi: below 20 corresponds to underweight, 
## greater than 20 and up to 25 is normal, greater 
## than 25 and up to 30 is overweight and above 30 is obese.

data1$bmi_fac <- cut(data1$bmi, breaks = c(0, 20, 25, 30, 40),
          labels = c('underweight', 'normal', 'overweight', 'obese'))
head(data1)
str(data1)

## Create a contingency table for Gender and bmi_fac
(tab1 <- table(data1$Gender, data1$bmi_fac))

## Second contingency table with another variable (occupation)
(tab2 <- table(data1$Ocupation, data1$bmi_fac))

## Mosaic plot for the table
## Define a new color palette
cols <- rainbow(50)
pie(rep(1, 50), col = rainbow(50), main = "Rainbow Palette")
## Mosaic plot with different colors
mosaicplot(tab1, col = c(cols[30],cols[28]), main = 'Body Mass Index')
mosaicplot(tab1, col = c('wheat','wheat3'), main = 'Body Mass Index')

## Mosaic plot for second table
mosaicplot(tab2, col = c('wheat','wheat3'), main = 'Body Mass Index')


## Barplots for first table
barplot(tab1, legend.text = T, col = cols[c(25,28)])
## Another arrangement
barplot(tab1, legend.text = T, col = cols[c(25,28)], beside = T)

## Add margins to the table
addmargins(tab1)

## Proportions over the total
(pt1 <- prop.table(tab1))

## Proportions with margins
addmargins(pt1) 

## Proportions over rows
prop.table(tab1, margin = 1)

## Proportions over columns
prop.table(tab1, margin = 2)

addmargins(prop.table(tab1, margin = 2), margin = 1)


## Chi-square test for the table
(csq.out <- chisq.test(tab1))

## Expected and observed values
csq.out$expected
csq.out$observed

## Fisher's exact test
fisher.test(tab1)

## Test for table 2
(csq.out2 <- chisq.test(tab2))
csq.out2$expected

## Tables with more than two dimensions
(with(data1, table(Ocupation, bmi_fac, Gender)))

(with(data1, ftable(Ocupation, bmi_fac, Gender)))


## Loading data manually
dog_data <- data.frame(id = c("Duke", "Lucy", "Buddy", "Daisy", "Bear", "Stella"),
                       weight = c(25, 12, 58, 67, 33, 9),
                       sex=c("M", "F", "M", "F", "M", "F"),
                       location=c("north", "west", "north", "south", "west", "west"))

## Selecting data from object
library(dplyr)
## Filter dogs that weight more than 40 
filter(dog_data, weight > 40)
# Another way
dog_data[dog_data$weight > 40,]

# female dogs in the north or south locations
filter(dog_data, (location == "north" | location == "south") & sex == "F")

## ANother way
with(dog_data, dog_data[ (location == "north" | location == "south") & sex == "F",])

# select
select(dog_data, id, sex)
select(dog_data, -c(id, sex))

## Appending by rows
# make a data.frame of new dogs
more_dogs <- data.frame(id = c("Jack", "Luna"),
                        weight=c(38, -99),
                        sex=c("M", "F"),
                        location=c("east", "east"))

# Make sure that data frames have the same columns
names(dog_data)
names(more_dogs)

# Appended dataset combines rows
all_dogs <- rbind(dog_data, more_dogs)
all_dogs

# Add new dog variable
# Matching variables do not have to be sorted
dog_vax <- data.frame(id = c("Luna", "Duke", "Buddy", "Stella", 
                             "Daisy", "Lucy", "Jack", "Bear"),
                      vaccinated = c(TRUE, TRUE, TRUE, TRUE, 
                                     TRUE, FALSE, FALSE, FALSE))

# id appears in both datasets, so will be used to match observations
dogs <- inner_join(all_dogs, dog_vax)
dogs

## NAs
# subset to weight values equal to -99, and then change
#  them all to NA
dogs$weight[dogs$weight == -99] <- NA
dogs$weight


## Descriptive statistics
head(data1)
# Subset some numerical variables  
data2 <- select(data1, Head_size,Height_cm,Weight_kg,bmi)
head(data2)

## Calculate correlations
cor(data2)
## Plot Correlations
corrplot::corrplot(cor(data2))
corrplot::corrplot.mixed(cor(data2))


### Some Useful Functions
### subset
data2 <- subset(data1, select = c('Gender','Head_size','Height_cm','Weight_kg'))
head(data2)

## Apply functions
## Use Iris data set. Two formats
## iris dataset is a data frame
str(iris)
head(iris[,-5])

## iris3 data set is an array
head(iris3)
str(iris3)

View(iris)
View(iris3)

## Apply functions
# iris_num = iris[,-5]
dimnames(iris3)
# Calculate mean for each variable
apply(iris3, 2, mean) 
# Mean for each variable according to species
apply(iris3, c(2,3), mean)

(iris.means <- apply(iris3, c(2,3), mean)) # Store means
## Center data
iris.ctd <- sweep(iris3, c(2,3), iris.means,'-') 
str(iris.ctd)

(iris.sd <- apply(iris3, c(2,3), sd)) # Store std dev
iris.std <- sweep(iris.ctd, c(2,3), iris.sd,'/') 

(apply(iris.std, c(2,3), mean)) # Calculate means
(apply(iris.std, c(2,3), sd)) # Calculate std dev


## tapply
## Use mtcars data
# Calculate mean of mpg according to am
tapply(mtcars$mpg, mtcars$am, mean)

# Calculate mean of mpg according to am and cyl
tapply(mtcars$mpg, list(mtcars$am,mtcars$cyl), mean)

### Aggregate
library(MASS)
str(crabs)
head(crabs)
# Calculate median for numerical variables according
# to sex and species
aggregate(crabs[, 4:8], list(sp=crabs$sp, sex=crabs$sex), median)

##################################
##################################
### Basic Graphs in R
## The plot function

attach(iris) # Places object iris in a preferent place in teh search path
plot(Sepal.Length, Sepal.Width, pch = 16)
plot(Sepal.Width ~ Sepal.Length, pch = 16)
## Color by species
plot(Sepal.Width ~ Sepal.Length, pch = 16, col =  Species)
# Add legend
legend('topright', c('setosa','versicolor','virginica'), col = 1:3, pch = 16)

## Scatterplot matrix
plot(iris[1:4], col = Species, pch = 16)

## Boxplots
boxplot(Petal.Length ~ Species, data = iris, col = 'white') 
# Add points
points(Petal.Length ~ Species, data = iris, col = 'blue', pch = 16) 
# Add a little noise
boxplot(Petal.Length ~ Species, data = iris, col = 'white') 
points(jitter(as.numeric(Species), factor = 0.2), 
       pch = 16, Petal.Length,
              col = cols[c(32,28,30)][as.numeric(Species)])


## Histograms
hist(Sepal.Length, col = 'azure3')

## Histograms for the numerical variables in iris
library(MASS)
par(mfrow=c(2,2))
truehist(iris$Petal.Length, nbins=12, xlab = 'Petal Length')
truehist(iris$Petal.Width, nbins=12, xlab = 'Petal Width')
truehist(iris$Sepal.Length, nbins=12, xlab = 'Sepal Length')
truehist(iris$Sepal.Width, nbins =12, xlab = 'Sepal Width')
par(mfrow=c(1,1))

# Histograms for petal length for each species, same scale
PL.set <- subset(iris,Species == 'setosa',Petal.Length)
PL.ver <- subset(iris,Species == 'versicolor',Petal.Length)
PL.vir <- subset(iris,Species == 'virginica',Petal.Length)
par(mfrow = c(3,1))
truehist(PL.set$Petal.Length, nbins = 10, xlim = c(0,8), xlab='Petal Length',
         main = 'Setosa')
truehist(PL.ver$Petal.Length, nbins = 10, xlim = c(0,8), xlab='Petal Length',
         main = 'Versicolor')
truehist(PL.vir$Petal.Length, nbins = 10, xlim = c(0,8), xlab='Petal Length',
         main = 'Virginica')
par(mfrow = c(1,1))

## Contours Plots
## Bivariate normal distribution.
library(mvtnorm)
x.points <- y.points <- seq(-3,3,length.out=100) 
z <- matrix(0,nrow=100,ncol=100)
mu <- c(1,1)
sigma <- matrix(c(2,1,1,1),nrow=2)
for (i in 1:100) {
  for (j in 1:100) {
    z[i,j] <- dmvnorm(c(x.points[i],y.points[j]),
                      mean=mu,sigma=sigma) 
  }
}
filled.contour(x.points,y.points,z)

## 3D plots
y <- x <- seq(-10, 10, length= 30)
f <- function(x, y) { r <- sqrt(x^2+y^2); 10 * sin(r)/r }
z <- outer(x, y, f); z[is.na(z)] <- 1
persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue")

## Star plots
stars(iris[,1:4], key.loc=c(25,20), nrow = 6, len = 2,
      main = 'Star plots for individual plants',
      col.stars=as.numeric(iris$Species))

## Interactive 3d graphs
library(rgl)
x <- iris$Sepal.Length
y <- iris$Petal.Length
z <- iris$Sepal.Width

rgl.open() # Open a new RGL device
rgl.points(x, y, z, color ="lightgray") # Scatter plot

rgl.bg(color = "white") # Setup the background color
rgl.points(x, y, z, color = "blue", size = 5) # Scatter plot

## Statistical Analysis

t.test(Salary ~ Gender, data = data1, alternative = 'less')
t.test(Sugar_in_blood ~ Gender, data = data1)

detach(iris)





