Prep Work

#getwd() SDMs/Physalia Course/PhysaliaCourseWork
spc <- read.csv("species_coords.csv", sep = ",")
head(spc)


x <- c(1, 2, 3, 4, 5)
x <- 1:5
x
x *3

c(1, 2, 3)
c(1, 2, 2.5, 3) # run these and see the differences

z <- c(2, 5, 3, NA, 10)

z

z * 2

class(x)

sizes <- c("tiny", "small", "medium", "large", "gigantic")
sizes
class(sizes)
sizes<-as.factor(sizes) #factor: categorical variable with associated values
class(sizes)
sizes
soil_classes <- 1:5
soil_classes <- as.factor(soil_classes)

sum(c(1, 2, 3, 4, 5))
prod(1:5)
mean(1:5)
sqrt(1:5)

x <- seq(from = 1, to = 10, by = 1)
x <- seq(1, 10, by = 1)
y <- c(1, 3, 5, 7, 9)
y <- seq(1, 10, by = 2)

y
seq(1, 10, by = 0.1)

mean(y)
sum(y)
length(y)

plot(y)

plot(z) #missing a NA value
mean(z) #won't compute because NA
sum(z) #won't compute because NA

#subsets
x[1] #indexing/subsetting by position, first value that appears
x[c(1,3,4)] # vales in 1st, 3rd, 4th position
x[-3] #all values but position 3
x[-c(1,3,4)]
sizes[2]
z
z[1:3] #first to third
z[-c(1:3)]

#subsets
x[x<5]
z[z>=3]
x[x==3]
z[z==3]
x[x>5 & x<9] #and
z[z<5 | z>=10] #or

z[!is.na(z)] #is NOT NA

class(spc) # class of the dataset we imported before
head(spc) # show the first rows
str(spc) # structure of the object
nrow(spc) # number of rows
ncol(spc) # number of columns
length(spc) # elements of a data frame are columns
names(spc)
# if ‘spc’ were a matrix rather than a data frame, you’d need:
colnames(spc)

dim(spc) # check object dimensions (number of rows and columns)
spc[1:3, c(2, 3)] #rows 1 to 3, columns 2 and 3
spc[3, 2]
spc[c(2, 7:10), ] #if not specified all elements are returned
spc[ , 3] # all rows, column 3
spc[3, ] # row 3, all columns
spc[1:10, -c(1, 3)] # rows 1 to 10, all columns except 1 and 3

# you can also index by name:
spc[ , "species"]
# or, with data frames (which are lists), you can also use:
spc$species

# but with $ you must use the actual name of the column,
# whereas with [ ] you can use an object containing the name:
my_column <- "species"
spc[ , my_column]

# you can mix positions and names on ≠ sides of the comma:
spc[2:4, c("species", "latitude")]

# you can also index/subset by value:
unique(spc$species)
spc[spc$species == "Gorilla beringei", ]


#totally disconnected from spc
mylist <- list(1:10, animal = "wolf", c(TRUE, TRUE, FALSE), values = c(1.2, 3.5, 4.1))
mylist #a list is a (non-atomic) vector of heterogeneous elements!
names(mylist)

mylist[1] #access list elements (containers)
mylist[[1]] #access content of list elements

length(mylist[1])
length(mylist[[1]])

class(mylist[1])
class(mylist[[1]])

mylist[[4]]
mylist[["values"]]
# subset list elements:
mylist[[4]] [c(1, 3)]

help(seq)
?seq
?`%%`

install.packages("raster")
library(raster)
help("raster")


#automation of analyses
head(longley)
plot(longley[ , 1])
plot(longley[ , 2])

for (i in 1:ncol(longley)) {
  plot(longley[ , i])
}

par(mfrow = c(3, 2)) #3 x 2 plots
plot(longley[ , 1])
plot(longley[ , 2])

for (i in 1:ncol(longley)) {
  plot(longley[ , i])
}


par(mar = c(2, 2, 2, 2))
plot(longley[ , 1])
plot(longley[ , 2])

for (i in 1:ncol(longley)) {
  plot(longley[ , i])
}



dev.off( )
plot(longley[ , 1])
plot(longley[ , 2])

for (i in 1:ncol(longley)) {
  plot(longley[ , i])
}


# ‘for’ loop producing nothing:
for (i in 1:ncol(longley)) {
  range(longley[ , i])
}


# ‘for’ loop producing results on screen only:
for (i in 1:ncol(longley)) {
  print(names(longley)[i])
  print(range(longley[ , i]))
}


#for loop placing results in var_ranges
var_ranges <- NULL # object must be created first (even if empty)

for (i in 1:ncol(longley)) {
  var_ranges[[i]] <- range(longley[ , i])
  }

var_ranges
names(var_ranges) <- names(longley)
var_ranges


# using ‘apply’ functions:
apply(longley, MARGIN = 2, FUN = sum) #apply() is for matrices, xapply() for data frames and lists
#margin = 2 gives you columns 1 is rows, Fun is function
#SO THIS SUMMED ALL COLUMNS
apply(longley, MARGIN = 2, FUN = range)
#lapply() for list output, sapply() for simple output


