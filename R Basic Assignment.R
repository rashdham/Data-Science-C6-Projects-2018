 
#	Import the Bollywood data set in Rstudio in a variable named bollywood


bollywood <- read.csv("Bollywood.csv")
View(bollywood)


#	When you import a data set, R stores character vectors as factors (by default)
# You can check the structure of the data frame by using str()
str(bollywood)

# You can change the attribute 'Movie' from factor to character type using the given command
bollywood$Movie <- as.character(bollywood$Movie)
	 

#Q1.
#	Access the last 10 movies (from the bottom of the Bollywood data frame) using column bollywood$Movie
# Store the names of those movies in last_10 vector (in the same order)

last_10 <- c(bollywood$Movie [52:61])
last_10
View(last_10)
	  
#Q2.
#	Find out the total number of  missing values (NA) in the bollywood data frame.
# Store the result in na_bollywood vector

na_bollywood <- sum(is.na(bollywood))
View(na_bollywood)	  
	
#Q3
#	Write the command to find out which movie tops the list in terms of Total Collections
# Store the movie name in variable named top_movie
 
top_movie <- bollywood[which.max(bollywood$Tcollection), 1]
top_movie
View(top_movie)

  
#Q4
#	Write the command to find out which movie comes second on the list in terms of Total Collections
# Store the movie name in variable named top_2_movie

x <- bollywood[order(- bollywood$Tcollection), 1]
top_2_movie <- x[2][1]
View(top_2_movie) 
	  
	
# Now let's find out the movies shot by Shahrukh, Akshay and Amitabh separately.
# subset() function is used for that. The code has already been written for you. 
	
shahrukh <- subset(bollywood, Lead == "Shahrukh")
akshay <- subset(bollywood, Lead == "Akshay")
amitabh <- subset(bollywood, Lead  == "Amitabh")

# You can view what the above data frames look like

View(shahrukh)
View(akshay)
View(amitabh)

#Q5
#	What is the total collection of Shahrukh, Akshay and Amitabh movies individually?
# You can use	a column named 'Tcollection' for this 
 
shahrukh_collection <- sum(shahrukh$Tcollection)
View(shahrukh_collection)
akshay_collection <- sum(akshay$Tcollection)
View(akshay_collection)
amitabh_collection <- sum(amitabh$Tcollection)
View(amitabh_collection)  
	
#Q6  
# Write command/s to find out how many movies are in Flop, Average, Hit and Superhit categories in the entire Bollywood data set.

x <- summary(bollywood$Verdict)
View(x)
   
#You can use SAPPLY function if you want to apply a function specific columns in a data frame 
#You can write a command to find the maximum value of Ocollection, Wcollection, Fwcollecion and Tcollection using sapply
  
oc <- c(bollywood$Ocollection)
wc <- c(bollywood$Wcollection)
fc <- c(bollywood$Fwcollection)
tc <- c(bollywood$Tcollection)
combined_df <- data.frame(oc,wc,fc,tc)
New_df<-sapply(combined_df, max , na.rm = TRUE)
View(New_df)

#Q7 
# Write a command to find the names of the movies which have the maximum Ocollection, Wcollection, Fwcollecion & Tcollection
# Store the names of 4 movies in same sequence in movie_result vector

x<- bollywood[which.max(bollywood$Ocollection), 1]
y<- bollywood[which.max(bollywood$Wcollection), 1]
z<- bollywood[which.max(bollywood$Fwcollection), 1]
a<- bollywood[which.max(bollywood$Tcollection), 1]
View(x)
View(y)
View(z)
View(a)
movie_result <- c(as.character(x),as.character(y),as.character(z),as.character(a))
View(movie_result)  


	

   
    


    
    
    