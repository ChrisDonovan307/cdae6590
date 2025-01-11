#' Lab 2 Demo
#' 2024-01-24

#' Welcome to the crashiest of courses in basic r operations!

#' The [#] symbol is for commenting. Anything on a line to the right of the #
#' will be a comment, change color, and will not be run as code.

# The window you're reading from right now is called the script or the source.
# It is the reproducible set of instructions for our analysis. Directly below
# the script is the console. That is where outputs go when we run operations. In
# the upper right is the environment, which tells us what objects we have access
# to. Note that objects are NOT saved when you save the script. They must be
# saved separately. The lower right has the file structure for our project.

# See the vertical line to the right? ---------------------------------------->
# That's the width of 80 characters. It is recommended not to go past this line
# for ease of reading.

#' To run a line of code, click anywhere on the line to put the cursor on it and
#' hit [CTRL / CMD + ENTER]. You can also select a range of lines to run that
#' section. To run the entire script, use [CTRL / CMD + SHIFT + ENTER]. Note
#' that this won't work very well here because I've put some broken commands
#' into this script, so you will get error messages.



# Objects -----------------------------------------------------------------


#' Lots of things are objects. Values, vectors or lists of values, data frames,
#' even functions are objects in R. To work with an object, we have to assign it
#' in our environment using the assignment operator: [<-]. Note that the very
#' helpful shortcut for this operator is [ALT + -].

#' Let's assign the value 3 to the object x. To run this line of code, put your
#' cursor anywhere on the line and hit [CTRL / CMD + ENTER]. You may highlight
#' the line if you wish, but it is not necessary.
x <- 3
#' Now take a look at the environment tab in the upper right. This is where we
#' can see all the objects we have loaded into our environment.

#' Let's try printing out our object now
print(x)
#' This simply prints the contents of our object to the console so we can see
#' it. Note that we can also just the object itself without using print to the
#' same effect
x

#' If we reassign another value to the same object, we will overwrite it.
x <- 7
print(x)

#' There are several types or [classes] of objects. We have [numeric] objects
x <- 3
class(x)

#' Or we can store text in [character] objects, also known as strings
y <- 'This is a string.'
class(y)
y

#' Character strings are specified by quotations. Quotes tell R to treat that
#' input differently than usual. Both single and double quotes work fine in most cases.
#' However, if you ever need to put quotes within quotes, it should be done by
#' putting single quotes within double quotes. There are some other edge cases
#' where single vs double quotes matter, but they are unusual.
z <- "You're doing great!"
z

#' We can also have [integer] classes, which are a like numeric but do not allow
#' for decimals, and [factor] classes, which are for categorical data.

#' We can convert between classes using functions. Let's assign 3 to x, then
#' convert it to a character.
x <- 3
class(x)
x <- as.character(x)
class(x)
x
#' Note that it still says 3, but now it is a character and has quote around it.
#' There are plenty of variations of this, like as.numeric or as.integer, etc.
#' It is important to know what class your variables are in so as to avoid
#' ridiculous headaches trying to figure out why your function isn't working.
#' For example, let's try to multiply x by 3
x * 3
#' This throws an error because we can't multiply a character by a number.

#' We can run into similar shenanigans with factors.
x <- as.factor(3)
class(x)
x
x * 3

#' When you're starting out with R, mixing up classes is a really common
#' mistake. It's one of the first things to troubleshoot if something isn't
#' working right.



# Vectors -----------------------------------------------------------------


#' A [vector] is a series of values of the same class of data. We use the
#' concatenate [c()] function to put these into a vector. Concatenate means to
#' link or chain something together.
z <- c(7, 2, 4)
class(z)
z
#' Now we have a numeric vector of three values

# We can do the same thing for character strings:
strings <- c('apple', 'banana', 'rocketship')
class(strings)
strings

# And factors:
factors <- as.factor(c('yes', 'no', 'maybe'))
class(factors)
factors
# Or any other class!

#' If we apply a function to a vector, it applies to every value in that vector
z
z * 3
sqrt(z)

#' Remember that every value in a vector MUST be the same class. R will try to
#' coerce values into the same class if it can. Let's see what happens when we
#' try to put a number and a string together:
bad_vector <- c(7, 'kerfuffle')
bad_vector
class(bad_vector)
#' Note that R coerced the number into a string to make it a single class.



# Data Frames -------------------------------------------------------------


#' A [dataframe] is tabular or rectangular data. It is very much like an excel
#' sheet, although it has some restrictions. It is made up of a series of
#' vectors, each one like a column in excel. Just like before, vectors can only
#' one class each, although you can have multiple vectors of different classes
#' in a data frame. Note that dataframes are often abbreviated as [df] in
#' examples.
df <- data.frame(
  numbers = z,
  strings = strings,
  factors = factors
)
class(df)
df

#' Take another look at the environment tab in the upper right. Note that our
#' df is now saved under the data tab. It tells us how many observations and
#' variables we have. If we click the arrow by the name, we can also see the
#' structure of the object. Here, it shows us the name, class, and the first
#' few values in each column. If we click on the grid symbol on the right, we
#' can open the dataframe to view it in a window. Alternatively, we can use the
#' View function. Note the capitalization here.
View(df)
#' However, it is best to learn different ways of exploring your data. When you
#' have very large dataframes, ocular inspection becomes unwieldy or impossible.

#' The structure function is very useful in exploring a df. It is the same thing
#' that is shown in the environment, but it is easier to see if we print it out
#' in the console. I end up using this about a hundred times per day.
str(df)

#' Try it with mtcars, an example dataset containing mph, number of cylinders,
#' horsepower, and other stats on a series of cars.
mtcars
str(mtcars)

#' Another good way to explore a df is by looking at the first or last few rows
head(mtcars)
tail(mtcars)

#' The defualt number of rows is 6, but you can specify how many rows you want
#' to see at once in the second argument.
head(mtcars, 10)

#' A df is made up of vectors. We can select them by using the name of the df,
#' the [$] operator, and then the name of the column.
df$numbers
df$strings

# Note that capitalization matters. See what happens when you try:
DF$numbers
#' This throws an error because our object is called df, not DF!

#' Here are some other functions to keep in mind when working with DFs:
names(df)
dim(df)
nrow(df)
ncol(df)



# Functions ---------------------------------------------------------------


#' We use functions to perform actions on objects. You've already seen the in
#' use! They look something like: `function_name(object)`
z <- c(3, 5, 2, 7)
mean(z)

#' If we want to perform multiple functions on an object, we can do it one by
#' one
mean_of_z <- mean(z)
mean_of_z

sqrt_of_mean_z <- sqrt(mean_of_z)
sqrt_of_mean_z

#' But it is much cleaner if we nest the functions together. Note here that when
#' we nest functions, they are performed from the inside out. The mean of z is
#' calculated first, and then we take the sqrt of that value.
sqrt(mean(z))

# Doing this the other way around gives a different answer
mean(sqrt(z))

#' Functions come in packages. Many packages for common functions come
#' pre-installed in RStudio, like `mean()` and `sqrt()`. Other packages have to be
#' downloaded.
install.packages('dplyr')

#' You only need to install a package on your computer once. In fact, if it is
#' already installed, and you try to install it again, it will throw an error!
#' More on avoiding this later. After installing it, you can use it every time
#' you open RStudio. However, EACH time you open a session in RStudio, you will
#' need to load the package.
library('dplyr')

#' dplyr, a mashup of 'data' and 'pliers', is a package in the [tidyverse], a
#' popular collection of packages that are intuitive, work well together, and
#' can perform a wide range of tasks for data science, like cleaning, wrangling,
#' analyzing, and visualizing data.

#' One of the tools available to us in the dplyr package, is the pipe operator:
#' [|>] or [%>%]. A very helpful shortcut here is [CTRL / CMD + SHIFT + M]. The
#' pipe sends the result of one operation into the next. For example, the next
#' three lines of code are exactly the same.
sqrt(16)
16 %>% sqrt()
16 |> sqrt()

# Note that pipe operators work differently than nested functions. Here, we
# perform the mean and sqrt functions on z and get the same result as before.
# However, they are written in a more intuitive order. Either way is fine.
z %>%
    mean() %>%
    sqrt()

#' Note that the two symbols for the pipe operator are almost, but not entirely
#' identical. [%>%] is the original pipe from dplyr. Or rather, it is pulled
#' from the magrittr package, but whatever, it is available to us when we load
#' dplyr. [|>] is the base R pipe, which was introduced about 10 years later.
#' They are almost identical, and you will likely see them both so I wanted to
#' introduce them. But they are not the same! I won't get into the details here,
#' but [%>%] has more capability, so I recommend using it even though the base R
#' pipe is objectively cooler looking.

#' Finally, you might see functions like this:
dplyr::glimpse(mtcars)
#' The [::] operator specifies which package the function is coming from. This
#' is quite useful for three reasons:
#'  1. It makes it clearer to the reader, which might be you later on, which
#'      package that function came from.
#'  2. Sometimes different packages have a function with the same name, known
#'      as a conflict. By specifying the package, we avoid that problem.
#'  3. If we have a package installed, we can use [::] to run a function from it
#'      without loading the package first.



# Regression --------------------------------------------------------------


# [] Load survey and run a simple t-test or regression here!
str(mtcars)
model <- lm(mpg ~ cyl + hp + wt, data = mtcars)
summary(model)



# Getting Help ------------------------------------------------------------


#' Knowing where to go for help is an important part of learning R. Even as a
#' reasonably experienced R user, I'm constantly pulling up documentation or
#' vignettes when I'm working through something new or complicated.

#' In the console, use a single question mark to pull up documentation for a
#' function that is loaded into your environment.
?mean
#' This will provide all sorts of useful information, like an explanation of how
#' to use the function, the arguments that it expects, how to interpret the
#' output, and sometimes links to other similar functions or details about the
#' methods being used. Hit [CTRL/CMD + Shift + 3] to make the help panel full
#' screen. I use this all day.

#' Note that this will only work if the function that package comes from is
#' loaded into the environment. For example, let's try the same thing with a
#' function from the skimr package:
?skim
#' No luck here!

#' If we don't know the package or don't have it loaded, we can use the double
#' question marks to search for it. This is slower, and it might come up with
#' results that you aren't interested in. But you can certainly find things this
#' way too.
??skim

#' By the way, skimr::skim is a ridiculously helpful function to remember.
install.packages('skimr')
library('skimr')
skim(mtcars)
#' We can see that it checks for missing data, gives completion rates by
#' variable, includes summary statistics, and also includes an itty bitty
#' histogram! Real classy, very helpful.

#' Back to business. Another useful place to go for help is vignettes. These are
#' usually short-ish sets of instructions written by the package author. It is
#' really the best place to figure out how to use a package.

#' To see a list of what vignettes are available in a package:
vignette(package = 'dplyr')

#' For an interactive list that can pull up a web browser:
browseVignettes(package = 'dplyr')

#' Another incredibly helpful resource will be Stack Overflow. It can be rather
#' snarky, but there is an invaluable heap of questions and answers there to
#' look through. Pay close attention to the guidelines for posting if you ask
#' a question. Namely - be sure to provide a reproducible example of your
#' problem. Plenty of documentation there about that.

#' Lastly, ChatGPT or other AI tools can be a pretty helpful when you're
#' learning R. You can ask it questions and have it generate code, but you can
#' also give it your broken code and have it suggest a fix. You can even
#' integrate Copilot into RStudio, although I've found it isn't as helpful as
#' you might think and that just going to ChatGPT works better for now at least.
#' A few things to keep in mind:

#' - AI does not know what works and what doesn't. It is often correct, but it
#'      wrong nearly as often. On the other side of things, if it tells you
#'      something correct and you say it is wrong, it will just apologize and
#'      change it. Be discerning and think critically.
#' - AI does not know what your intentions are. It doesn't have the bigger
#'      picture in mind. It is a great help for small problems, but less so if
#'      you're doing anything more elaborate. Best to break it into smaller
#'      chunks. But keep in mind that might come back to haunt you later.
#' - If AI is writing all your code, you aren't learning anything. The best way
#'      to use it in my experience is to understand WHY it suggested what it
#'      did. Did it use functions you haven't seen before? Look them up so you
#'      understand them and can use them again. I won't profess to know how AI
#'      will change coding in the next few years, but I imagine that even if AI
#'      writes all our code, we will still need to be able to understand it and
#'      think critically about it.
#' - Finally, keep in mind that everything ChatGPT knows about code was stolen
#'      from humans on Stack Overflow and similar sites without credit or
#'      compensation.



# Wrapping Up -------------------------------------------------------------


#' The next step will be to move on to the `load_and_wrangle.R` script. Before
#' we go, you may notice that your environment is now full of a bunch of
#' variables we assigned in this script. It is good practice to clear your
#' environment of this detritus from time to time so that you don't end up
#' thinking that df is one thing when it is actually another. There are a few
#' ways we can do this.

#' First, you will likely see the `rm()` function used for this. We can remove a
#' single object:
rm(x)
#' Note that x is now removed from the environment pane.

#' Or, we can use it to remove all the objects from the environment:
rm(list = ls())
#' Notice that your environment is now empty.

#' The motivation behind this practice is good, but the execution is not ideal.
#' The point of using it this way is to start fresh so that you can run your
#' analysis from the top. The problem with it is that it doesn't accomplish what
#' it is intended for. It does not unload packages, run the .Rprofile script,
#' reset global options, or various other things. Instead, best practice is to
#' use [CTRL / CMD + SHIFT + F10]. This completely resets your R session, and is
#' the same as closing R and restarting it. It is a clean wipe.

#' Whichever method you use, you will want to save any variables you care about
#' before you do it so you don't lose them. More on that later!
