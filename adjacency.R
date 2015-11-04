library(igraph)

#just load the plotting function

build_plot <- function(p=1){
  CourseMatrix <- data.matrix(df, rownames.force = TRUE)
  
  g <- graph.adjacency(CourseMatrix, weighted=T, mode = "undirected")
  g <- simplify(g) # remove loops
  # set labels and degrees of vertices
  V(g)$label <- V(g)$name
  V(g)$degree <- degree(g)
  
  set.seed(456) # set seed to make the layout reproducible
  layout1 <- layout.fruchterman.reingold(g)
  
  if(p==1){
    plot(g, layout=layout1)
  }else{
      plot(g, layout=layout.kamada.kawai) #just another way of doing the plot
    }
}



# Say we created a dataset with 2 variables: studentID and course
# This could be for a single year, by faculty, or by academic career
# The code below takes such data and creates an adjacency matrix


# I haven't pulled the real student/course data, so we need to create some dummy data
# This code below generates a random binomial matrix

n <- 30 #courses for the x axis
k <- n #courses for the y axis

# The 2 functions below do the same thing.  I am playing around with the speed of R
jared <- function(m, n)
  matrix(sample(0:1, m * n, replace = TRUE), m, n)

f <- function(m, n) {
  tmp <- sample.int(2L, size = m * n, replace = TRUE) - 1L
  dim(tmp) <- c(m, n)
  tmp
}



set.seed(123) #set the seed so can replicate the data
data1 <- jared(n,k)
set.seed(123) #
data2 <- f(n,k) #generating the data again to prove replication
all.equal(data1, data2) #proving replication is possible
identical(data1, data2) #can also use the identical function

#convert the data to a dataframe and rename the rows
df <- as.data.frame(data1,row.names = paste0("course", 1:n)) 
#rename the columns which are named V1.....Vn
names(df) <- gsub("V", "course", names(df))


# just making the above into a function to call it easily
rebuild_data <- function(){
  set.seed(123) #set the seed so can replicate the data
  data1 <- jared(n,k)
  df <- as.data.frame(data1,row.names = paste0("course", 1:n)) 
  names(df) <- gsub("V", "course", names(df))
}

### Issues ###
# Because the data is generated randomly, the resulting plot will not show any patterns.  
# Actually, it will show a pattern...one that shows equal connections between ALL courses
# Let's build plot type 1
build_plot(p=1)
# Try the other plot type 2
build_plot(p=2)

#reset the df
rebuild_data()

# We can try to set some columns and rows to 0
# That is, we can make it so that some courses are extremely commonly connected and others are not
df[,1:10] <- 1 #colums 
df[11:20] <- 0 #rows
# Now let's plot it again
# Let's build plot type 1
build_plot(p=1)
# Try the other plot type 2
build_plot(p=2)



#reset the df
rebuild_data()


# But that won't do either!  We need to make sure the same courses we set to 0 or 1 on the X axis is 
# also done on the Y axis!
df[1:5,] <- 1
df[,1:5] <- 1
df[11:20,] <- 0
df[,11:20] <- 0
# Let's build plot type 1
build_plot(p=1)
# Try the other plot type 2
build_plot(p=2)


# The last plot is extreme.  In reality, my assumption is that this is a sparse matrix.  We should see many lines connecting
# commonly linked courses but most won't really be linked.  I'm hoping something interesting will jump out at me.

# If my assumption is true, we should see a mass cluster in the plot that is all jumbled and connected, but with an
# extremely large number of satellite and sparsely connected courses.




# the below will detach the igraph library and reset the R environment base packages 
# (becuase igraph 'overwrites' some base functions)
detach("package:igraph", unload=TRUE)
