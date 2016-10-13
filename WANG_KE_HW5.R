#HW 5

# Acquire dataset "diamonds" and give it structure. 
data("diamonds")
str(diamonds)

# Question 1
# Define a function funcQ1 which gives all methods and attributes associates with a dataframe and
# the number of columns in a dataframe.
# The parameter is a dataframe.
funcQ1 <- function(data){
print(attributes(data)) # Print the attributes of the input dataframe
print(methods(class="data.frame")) # Print all methods of the class data.frame
print(ncol(data)) # Print number of columns in a datadrame
}

# Question 2
# Define a function funcQ2 which gives the number of rows in a dataframe.
# The parameter is a dataframe.
funcQ2 <- function(data){
print(nrow(data)) # Print number of rows in a datadrame
}

# Question 3
# Define a function funcQ3 which gives the column names of a dataframe.
# The parameter is a dataframe.
funcQ3 <- function(data){
  for(names in colnames(data)){ # Loop for each element in the vector of column names of the dataframe
  print(names) # Print each name in the column name (here I used loop so that each name is printed in a different line since colnames function returns a vector.)
  }
}

# Question 4
# Define a function funcQ4 which prints the type of each column in a dataframe.
# The parameter is a dataframe.
funcQ4 <- function(data){
  sapply(data, class) # Apply the class function (which returns the class of the input) to each element of the dataframe
}

# Question 5
# Define a function funcQ5 which loops through a dataframe and calculates the mean of every numeric column.
# The parameter is a dataframe.
funcQ5 <- function(data){
  namelst <- colnames(data) # Assign the vector of column names of the dataframe to a new vector namelst
  for(names in namelst){ # Loop for each element in the vector of namelst
    if(sapply(data[names], is.numeric)) # Check each column if it is numeric
      cat(names, sapply(data[names], mean), '\n') # If a column is numeric, outputs that column name and its mean for each numeric column and put each of them in a different line
  }
}

# Question 6
# Define a function funcQ6 which loops through a dataframe and creates a frequency table for every factor column.
# The parameter is a dataframe.
funcQ6 <- function(data) {
  namelst <- colnames(data) # Assign the vector of column names of the dataframe to a new vector namelst
  for (names in namelst){ # Loop for each element in the vector of namelst
    if (sapply(data[names], is.factor)) { # Check each column if it is factor
      cat(names) # Output the column name and the frequncy table of each factor column
      print(table(data[names]))
    }
  }
}

# Question 7
# Define a function funcQ7 which loops through a dataframe and determines the number of rows containing NA (missing value) 
# in each column and the percentage of rows containing an NA in any of the columns.
# The parameter is a dataframe.
funcQ7 <- function(data){
print(nrow(data) - sum(complete.cases(data))) # The complete.cases function returns true if there is no missing values. The total number of rows minus the number of rows without missing values gives the desired value.
print(sapply(data, function(x) sum(is.na(x))/length(x))) # Apply a function to each element of the dataframe. The function takes an input and calculates the total number of missing values divided by the total number of the input.
}

# Question 8
# Define a function funcQ8 which accepts a dataframe as a parameter and 
# returns a dataframe that contains each pair of column names in the first column in a single string separated by a -,
# and their corresponding Pearson correlation coefficient in the second column.
# The parameter is a dataframe.
funcQ8 <- function(data){
  num <- sapply(data, is.numeric) # Check the numeric columns of the dataframe and assign them to a logical vector num.
  new_data <- data[,num] # Create a new dataframe with only numeric vectors.
  names <- colnames(new_data) # Assign the vector of column names of the new dataframe to a new vector names
  combonames <- combn(names, 2) # Assign the combination of vector names (here we used n choose 2, which give the combination of choosing 2 elements from the vector names) to combonames
  combo <- combn(length(colnames(new_data)), 2) # Assign the similar combination as the above line to the length of the vector colnames(new_data), namely the vector names
  variable <- paste(combonames[1,], combonames[2,], sep = '-') # Assign the pairs of column names linked with "-" to a new vector variable
  Pcorcoeff <- c() # Assign an empty vector to the column of Pearson correlation coefficient
  
  for(i in 1:length(variable)){ # Loop for each element in variable
    p <- cor(x= new_data[combo[1,i]], y = new_data[combo[2,i]]) # Assign the correlation of pairs of column to p.
    Pcorcoeff[i] <- p[1] # Assign p with the desired Pearson correlation coefficient to the vector of Pcorcoeff.
  }
  return(data.frame(variable, Pcorcoeff)) # Outputs the dataframe containing pairs of column names and their Pearson correlation coefficient
}