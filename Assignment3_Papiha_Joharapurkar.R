#Assignment: Assignment 3 
#Class: Special Topics 
#Date: June 24 2022
#Name: Papiha Joharapurkar

# Prepare a dictionary of words to choose from and save it in a txt file (one column)
# Read the word list from your program.

#SetWD 
setwd("/Users/papihajoharapurkar/Downloads/")

#Have provided my own list of words, can replace with your list of words as well 
dict_words <- read.table("assign_3_dict_random_words.txt")

# Choose a random element from the list. Hint: You may want to check sample() and sample.int()  functions. 
# There are many different ways of doing this. You are welcome to experiment.
secret_word <- dict_words[sample(nrow(dict_words), 1, replace=T),]

# Inform the user on the length of the secret word. Hint: You may test nchar()
print(paste("User, the legnth of the word is",nchar(secret_word)))

# Informing the user about the number of wrong guesses/tries allowed.
# The length of the word * 2 is equivalent to the number of guesses user is allowed
# A vector called 'Guesses log' is being created, will catalogue user's previously-attempted characters 
print (paste("User, you are allowed", nchar(secret_word)*2, "incorrect guesses"))
guesses = nchar(secret_word)*2
guesses_log <- c()

#A visual progress indicator of the secret word is being made as a vector with seq_along function
user_word = ""
for (i in seq_along(1:nchar(secret_word))){
  user_word <- paste0(user_word, "_")
}

#the while loop will run as long as the inputted word by the user is not equal to secret word 
while (user_word != secret_word)  {
  
  #if user has more than >0 guesses, this portion of loop will run
  if (guesses > 0) {
    #User is being informed on the progress of the word, guesses, and which letters they have already tried
    print (paste("Hello User. This is the current word:", user_word, "and you have", guesses, "guesses left" ))
    cat("You have already used these letters: ", guesses_log)
    
    #User has the option to guess the entire word
    user_guess_word <- readline(prompt = "Would you like to guess the entire word? (-2 points if incorrect). (y/n): ")
    #input is being converted to character and lower-case type
    user_guess_word < as.character(user_guess_word)
    user_guess_word <- tolower(user_guess_word)
    
    #If the user does not want to guess the entire word (by entering "n"), this loop will run
    #User is asked to input in a single-letter as a guess
    #User input will be converted to character and lowercase-type 
    if (user_guess_word == "n") {
      user_guess <- readline(prompt = "Then, please guess 1 character: ")
      user_guess <- as.character(user_guess)
      user_guess <- tolower(user_guess)
      
      #if the user input is valid i.e. input is not NA, is 1 character in length, and consists of characters from A-Z
      if ((!is.na(user_guess) == T) & nchar(user_guess) == 1 & grepl("[a-z]",user_guess) == T) {
        
        #If user guess is not in guess log, will be added to guesses-vector
        if (user_guess %in% guesses_log == F) {
          guesses_log <- c(guesses_log, user_guess)
          
          #For loop will be iterating over every position in the secret word 
          #To determine whether any characters match with the user_guess from the substr function
          for (i in seq_along(1:nchar(secret_word))) {
            #if the user guess matches a character in the secret word
            #The user_guess will be updated with substr function and 1 guess will be incremented
            if (user_guess == (substr(secret_word, i, i))) {
              substr(user_word, i, i) <- user_guess
              guesses = guesses + 1
            }
            
          }
          #Every time a user' guess is tried, 1 guess will be subtracted
          #If user guess is correct, then 1 guess will have been added previously
          #This ensures no change in total # of guesses if user correctly attempts a character
          guesses = guesses - 1
          
          #If user guess is already in guess log, will inform user, and return user to initial user prompt
        } else {
          print ("You've already chosen this character. Select another.") 
        }
        
        #Informing if user' input is invalid i.e. if input has NA values, multiple characters or non-alphabetic characters 
        #Will return user to initial user prompt 
      } else {
        print ("Incorrect Input. Enter a single alphabetic character.")
      }
      
      
      #Inner loop will run if user attempts to enter full word (entering "y")
      #User is being prompted to answer secret_word
      #User's prompt is being converted to character-string and lowercase
    } else if (user_guess_word == "y") {
      user_guess_secret_word <- readline(prompt = "Please enter your secret word: ")
      user_guess_secret_word <- as.character(user_guess_secret_word)
      user_guess_secret_word <- tolower(user_guess_secret_word)
      
      #Is testing if user input is invalid - whether it has NA values or non-alphabetic characters 
      #If invalid, am informing user and returning to initial user prompt
      if (is.na(user_guess_secret_word) == T | grepl("[a-z]",user_guess_secret_word) == F) {
        print( "Incorrect Input Type. Try again in next turn.")
      }
      
      #If user has successfully entered secret word, will be equating user_word to secret word 
      else if (user_guess_secret_word == secret_word){
        user_word <- secret_word
      }
      
      #If user has entered incorrect word, -2 guesses being taken away as warned
      else {
        print ("Incorrect word. Try again.")
        guesses <- guesses - 2 
      }
      
    }
    
    #Printing error-message if user chooses invalid input for answering whether 
    #they would like to attempt a full-word option (y/n)
    else {
      print ("Invalid Input. Try again.")
    } 
    
    #If user has no more guesses available, informing user and breaking while-loop
  } else {
    print ("You're out of guesses")
    break 
  }
}

#Final summary statement of the game is being provided
#If user was successful, offering congratulates and reminder of secret word 
#If user was unsuccessful, informing user of secret word 
if (user_word == secret_word) {
  print (paste0("Congratulations! You solved the secret word: ",secret_word))
} else {
  print (paste("The correct word was ",secret_word,"! Better luck next time!", sep=""))
}
