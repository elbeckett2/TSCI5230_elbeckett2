#'---
#' title: "TSCI 5050: Introduction to Data Science"
#' author: 'Author One ^1^, Author Two ^1^'
#' abstract: |
#'  | Provide a summary of objectives, study design, setting, participants,
#'  | sample size, predictors, outcome, statistical analysis, results,
#'  | and conclusions.
#' documentclass: article
#' description: 'Manuscript'
#' clean: false
#' self_contained: true
#' number_sections: false
#' keep_md: true
#' fig_caption: true
#' output:
#'  html_document:
#'    toc: true
#'    toc_float: true
#'    code_folding: show
#' ---
#'
#+ init, echo=FALSE, message=FALSE, warning=FALSE
# init ----
# This part does not show up in your rendered report, only in the script,
# because we are using regular comments instead of #' comments
debug <- 0;
knitr::opts_chunk$set(echo=debug>-1, warning=debug>0, message=debug>0, class.output="scroll-20", attr.output='style="max-height: 150px; overflow-y: auto;"');

library(rio);# simple command for importing and exporting
library(pander); # format tables
#library(printr); # set limit on number of lines printed
library(dplyr); #add dplyr library

options(max.print=500);
panderOptions('table.split.table',Inf); panderOptions('table.split.cells',Inf);
whatisthis <- function(xx){
  list(class=class(xx),info=c(mode=mode(xx),storage.mode=storage.mode(xx)
                              ,typeof=typeof(xx)))};
# R basic syntax ----
#'
#' # R basic syntax
#'
#' ## Assignment
#'
#' To store a value as variable `foo` in R the convention is to use the
#' `<-` operator, not `=`. This makes it easier to distinguish stand-alone
#' expressions from function arguments.

#+ assignment_operator
foo <- 500;
bar <- foo <- 500;
bar <- foo;
#' It's not a formal rule, it's rarely even written down, but `foo`, `baz`,
#' `bat`, etc. are throw-away variables people use for testing. If you need more
#' test variables, just make up three letter ones that start with `b`.
#' If you see one of those in a script you're reviewing it means it is left over
#' from when that code was being debugged. Example code shared with this class
#' will usually use `foo` and friends to represent the parts of an expression
#' that should be replaced with whatever values you are using instead of being
#' used literally. Shorter than having to write `YOURFUNCTIONHERE` or
#' `YOURARGUMENTHERE` each time.
#'
#' This is not specific to R-- it's just a little quirk of programming culture
#' in general. A quirk with a practical consequence: _never use `foo`, `bar`,
#' `baz`, `bat`, etc. in your production (i.e. finalized) code_ because
#' otherwise you or somebody else debugging your code will attempt to use those
#' names as test variables and in some situations this could overwrite the
#' existing variables!
#'
#' ## Comments
#'
#' `#'` This indicates that this line should be formatted as text. It must be
#' the first two characters in that line in order to work.
#'
#' `#+` This indicates that the following lines (until the next #' or #+) should
#' be treated as a "code chunk". I.e. the next lines (but not this one) will be
#' run, the code will be displayed according to your settings and the results
#' will be displayed according to your settings.
#'
#' `#` This is an ordinary comment. Everything after it on the same line is not
#' executed.
#'
#' # Functions and Data Types
#'
#' ## Functions and Simple Data Types
#'
#' ### Numeric values. 
#' 
#' You can do arithmetic on them: `+`, `-`, `/`, `*`, `^`, `log()`,
#' `exp()`, `sqrt()`

#+ assignment_numeric
foo <- 2+2; foo
foo <- 5*2; foo
foo <- log(2); foo
foo <- exp(100); foo
log(foo)
bar <- 5*5; bar
whatisthis(bar);
print(foo <- 42)
print (bar <-67)
log(((foo*3)+(bar+2)*sqrt(144)+exp(10)))

#' ### Character strings. 
#' 
#' Create these by wrapping single (`'`) or double (`"`)
#' quotes around the value.
#+ assignment_string
a <- "Donot panic"; a
b <- 'Donot panic'; b
c <- "Don't panic"; c
d <- 'The "Heart of Gold" comes equipped with heated leather seats and an infinite improbability drive'; d
e <- 42; e
e <- "42"; e

#' ### Logical values.
#' 
#' These are `TRUE` or `FALSE`. They can be created using `>`, `<`,
#' `==`, `!=`, `>=`, `<=`, `&`, `|`, and `!`
#+ assignment_logical
a <- foo > 25; a           # greater than
b <- foo >= 40; b          # greater or equal to
c <- foo != 42; c          # not equal -- ! is the 'not' operator
d <- foo == 42; d          # equal -- == is the equal operator (as opposed to = which is assignment)
e <- foo <50 & bar<100 ; e #  AND operator
f <- foo<50 | bar>100; f   #  OR operator
g <- !(bar>50); g          #  NOT operator can be used with any TRUE/FALSE expression

#' ### Missing values
#' 
#' Missing values are represented by `NA` (no quotes for any of these). Null
#' values are _not_ the same as missing and they are represented by `NULL`. In
#' some circumstances you might also run into `Inf`, `-Inf`, and `NaN`. These
#' often indicate errors somewhere else in your code.

#' ### Dates and times
#' 
#' Dates and times. Can be created with the `Sys.Date()` or `Sys.time()`
#' functions or converted from a character string using `as.Date()`.

#+ assignment_datetime
Sys.Date()
Sys.time()
#?as.Date
my_date <- "2022-01-31"; #' create example date
print (my_date)       ; #' print my_date variable
class (my_date)       ; #' check class of the variable
new_date <- as.Date(my_date) ; #' convert character string
new_date                      ; #' print new_date
class(new_date)               ;#' check class of new_date
as.Date(new_date, tryFormats = c("%y-%m-%d"))

#' ### Factors
#' 
#' Factors are basically integers that have labels. They are a human-readable
#' alternative to using integer codes for discrete data. These will make more
#' sense after we talk about vectors in the next section.

#+ factor_example

#+ assignment_wierd

#' ## Data Structures
#'
#' Of course we usually need to work with variables bundled together, not single
#' values.
#'
#' ### Vectors
#'
#' The default data structure in R is a `vector`. You create one with the `c()`
#' command with any number of arguments. All items in a vector have to be the
#' same type.

#+ vectors_c
print(foo <- c(56,78,34,97,2,86))
print(baz <- c(34,23,94,3,12,53))
#' Since the default data structure in R is a `vector`, if you
#' create some sort of simple value you are creating a `vector` even if you are
#' not using `c()`... it just happens to be a `vector` of length 1. These
#' are identical, and both return `1` when used with the `length()` function.

#+ vectors_length1
length(foo)
#' If you want to create a sequence of consecutive integers, you can use the `:`
#' operator.

#+ vectors_sequence
25:76
65:38
-32:12
seq_len(12)
seq(-9.7, 8.3,by=0.1)

#' In most other languages, you need to use a `for` loop in order to perform
#' some sort of change to a series of values. In R, you often don't have to
#' when you are working with vectors because a lot of functions (including all
#' the arithmetic and logical ones above) and be applied to a vector and they
#' work. If the function involves multiple vectors (e.g. `+`), usually you'll
#' want all of them to be either the same length or length 1.

#+ vectors_operators
foo+6
foo+baz
foo>=34
baz<=23
bob <- baz<=23
c(baz,foo)
c(baz,foo,"76")
# These work with all arithmetic operators
#' You can assign names to some or all members of a vector when you create it.
#' You can also assign or re-assign names later using the `names()` function.

#+ vectors_names1, error=TRUE
jar <- c(a="cat", best="dog", c= "fish", slow="turtle")
print(jar)
# jar[best]
jar["best"]
jar[c("best","c")]
#' You can also use it to see the currently assigned names.

#+ vectors_names2
names(jar) <- c("libby", "beau", "bob", "bob2") # renaming
print(jar)
names(jar)
names(jar)[3]
names(jar)[3] <- "milo" # renames only one element
jar
#' You can subset a vector by using `[...]` with the `...` replaced by _another_
#' vector, of integers indicating which positions you want to extract. Or you
#' could use a vector of names.

#+ vectors_subset1
foo[3]
#' If you just need a single value, use a single name or number.

#+ vectors_subset2

#' If you need a series of adjacent values, use `:`. If you need several
#' adjacent series with interruptions between them, use `c()` with `:`
#' expressions separated by commas `,`.

#+ vectors_subset3
foo[c(1,2,3)]
foo[1:3]
foo[c(1:3,5:6)]
baz
bob
baz[bob] # pulled the vector less than equal to 23
print(foo)
summary(foo)
table(foo)
table(jar) # frequency table
bat <- sample(1:10, 30, replace = TRUE)
table(bat)
bat <- sample(1:10, 30, replace = TRUE)*1000
table(bat)
bat
head(bat) # top 6 elements
tail(bat) # last 6 elements
diff(bat) # difference between two values
sum(bat) # sums all values
seq_along(bat) # sequence all values
sum(bat, na.rm= TRUE) # for missing values
#' Other useful functions for exploring vectors: `length()`, `summary()`,
#' `table()`, `head()`, `tail()`, `sum()`, `diff()`, `seq_along()`.

#+ vectors_explore


#' Here are some aggregation functions. For these, make sure to use `na.rm=T` if
#' your vector has `NA`s in it... `max()`, `min()`, `mean()`, `median()`,
#' `quantile()`.
quantile(bat) # tells quantiles
quantile(bat, na.rm=TRUE)
sum(bat)
min(bat)
max(bat)
#+ vectors_aggregate

#' ### Data Frames
#'
#' You can bundle several vectors of the same length together into a
#' `data.frame` using the `data.frame()` command. A `data.frame` is a tabular
#' data structure where the columns can be different types from each other
#' (though within each column the type will be uniform because each column is
#' still a vector). Most data in R is in the form of a `data.frame` or a class
#' that inherits from `data.frame`. The `dplyr` package makes working with
#' `data.frames` easier and a lot of attention will be devoted to `dplyr`
#' [below](#data-frames-indepth). For now, here are some basic commands for
#' exploring `data.frames`: `dim()`, `nrow()`, `ncol()`, `names()`, `str()`,
#' and `summary()`.

#+ df_explore
dim(iris)
nrow(iris)
ncol(iris)
names(iris)
head(iris)
tail(iris)
head(iris,10)
str(iris)
summary(iris)
#' how to select rows
#+ df_subset
iris[3:20,]
iris[c(2:10,34,40:50,34,34,34),]
iris[-c(3:20),]
seq_len(nrow(iris))

sample(seq_len(nrow(iris)), 10)  # sample without replacement
sample(seq_len(nrow(iris)), 10, replace= TRUE) # sample with replacement
iris0 <- iris[sample(seq_len(nrow(iris)), 10),]

#' How to select coulmns in dataset
#+ df_columns, error=TRUE, results="hide"
iris[,1:3] # columns miss 4 and 5
iris[,c("Petal.Length","Petal.Width","Species")]
prevar <- c("Petal.Length","Petal.Width","Species")  # define columns together
iris[,prevar]



iris$Species # picks the column from dataset by adding $sign
outcome <- "Species"
iris$outcome
iris[[outcome]]
iris [["Species"]]

#' how to select columns and rows at same time
#+ df_columnsrows
iris[4:10,prevar]

#' # Datasets and `dplyr`
#+ Working with datasets and DPLYR
#'
#' The `%>%` operator takes the result on its left and gives it to the function
#' on its right as the first argument. This makes it easier to read a series of
#' steps from left to right instead of nesting functions inside one another.

#+ dplyr_pipe
iris %>% head()
iris %>% head(10)

#' ## Selecting rows and columns
#'
#' `select()` keeps columns. `filter()` keeps rows based on a logical condition.
#' `arrange()` changes the order of rows. `desc()` reverses the order for one
#' variable inside `arrange()`.

#+ dplyr_select_filter_arrange
iris %>% select(Sepal.Length,Sepal.Width,Species) %>% head()
iris %>% filter(Species=='setosa') %>% head()
iris %>% filter(Sepal.Length>5,Sepal.Width>3) %>% head()
iris %>% arrange(Sepal.Length,desc(Sepal.Width)) %>% head()

#' You can use `select()` helpers when it would be inconvenient to type every
#' column name separately.

#+ dplyr_select_helpers
iris %>% select(starts_with('Sepal')) %>% head()
iris %>% select(where(is.numeric)) %>% head()
iris %>% select(-Species) %>% head()

#' ## Creating or changing columns
#'
#' `mutate()` creates new columns or changes existing ones. New columns created
#' earlier in the same `mutate()` can be used to create later columns.

#+ dplyr_mutate
iris %>% mutate(Sepal.Area=Sepal.Length*Sepal.Width) %>% head()
iris %>% mutate(Petal.Ratio=Petal.Length/Petal.Width
                ,Inverse.Ratio=1/Petal.Ratio
                ,Species=toupper(Species)) %>% head()

#' `case_when()` is useful when the value you want in a new column depends on
#' several logical conditions.

#+ dplyr_case_when
iris %>% mutate(Sepal.Size=case_when(
  Sepal.Length<5 ~ 'small'
  ,Sepal.Length<6 ~ 'medium'
  ,TRUE ~ 'large'
)) %>% select(Sepal.Length,Sepal.Size) %>% head(10)

#' You can use `relocate()` to change where columns appear without changing
#' their values.

#+ dplyr_relocate
iris %>% mutate(Sepal.Area=Sepal.Length*Sepal.Width) %>%
  relocate(Sepal.Area) %>% head()

#' ## Summarizing data
#'
#' `summarise()` returns summary values instead of the original rows. `n()`
#' gives the number of rows being summarized.

#+ dplyr_summarise
iris %>% summarise(Median=median(Sepal.Length)
                   ,Average=mean(Sepal.Length)
                   ,N=n())

#' `group_by()` changes later operations so that they happen separately within
#' each group. It is most commonly followed by `summarise()` or `mutate()`.

#+ dplyr_group_by
iris %>% group_by(Species) %>% summarise(Median=median(Sepal.Length)
                                         ,Average=mean(Sepal.Length)
                                         ,N=n())
iris %>% group_by(Species) %>% mutate(Species.Mean=mean(Sepal.Length)) %>% head()

#' `across()` applies the same function to several columns. `where()` selects
#' columns based on what kind of data they contain.

#+ dplyr_across
iris %>% summarise(across(where(is.numeric),mean))
iris %>% group_by(Species) %>% summarise(across(where(is.numeric),mean))
iris %>% mutate(across(where(is.numeric),round)) %>% head()

#' You can apply more than one summary function to the same set of columns by
#' passing a named list of functions to `across()`.

#+ dplyr_across_multiple
iris %>% group_by(Species) %>%
  summarise(across(where(is.numeric)
                   ,list(mean=mean,median=median)))

#' ## Missing values in data frames
#'
#' `is.na()` identifies missing values. Many summary functions will return `NA`
#' if any input is missing unless you use `na.rm=TRUE`.

#+ df_missing
missing_example <- head(iris,6)
missing_example$Sepal.Length[c(2,5)] <- NA
missing_example
is.na(missing_example$Sepal.Length)
sum(is.na(missing_example$Sepal.Length))
mean(missing_example$Sepal.Length)
mean(missing_example$Sepal.Length,na.rm=TRUE)
missing_example %>% filter(!is.na(Sepal.Length))

#' `coalesce()` returns the first non-missing value. A common use is replacing
#' missing values with some explicitly chosen value.

#+ df_coalesce
missing_example %>% mutate(Sepal.Length=coalesce(Sepal.Length,0))

#' Be careful when doing this with real data. Zero and missing are usually
#' different things. The example above is demonstrating the syntax, not saying
#' that missing measurements should generally be replaced by zero.

#' ## Turning numeric values into categories
#'
#' `cut()` creates a factor by splitting numeric values at specified cutpoints.
#' Use `-Inf` and `Inf` when you want the first and last groups to include every
#' possible value below and above the interior cutpoints.

#+ df_cut
sepal_length_group <- cut(iris$Sepal.Length
                          ,c(-Inf,5,6,Inf)
                          ,labels=c('short','medium','long'))
sepal_length_group %>% table(useNA='ifany')
levels(sepal_length_group)

#' You can also add the resulting factor directly to a data frame.

#+ df_cut_mutate
iris %>% mutate(Sepal.Length.Group=cut(Sepal.Length
                                       ,c(-Inf,5,6,Inf)
                                       ,labels=c('short','medium','long'))) %>%
  head()

#' ## Combining data frames
#'
#' `bind_rows()` stacks data frames with compatible columns. The examples below
#' create their inputs directly from `iris`.

#+ df_bind_rows
iris_first <- head(iris,3)
iris_last <- tail(iris,3)
bind_rows(iris_first,iris_last)

#' `bind_cols()` puts data frames or vectors next to each other. This only makes
#' sense when the rows are already in the same order and refer to the same
#' observations.

#+ df_bind_cols
iris_measurements <- iris[,1:4]
iris_species <- iris["Species"]
bind_cols(iris_measurements,iris_species) %>% head()

#' ## Joining data frames
#'
#' Joins combine data frames by matching one or more key columns. Here we make
#' a small lookup table for the three species in `iris`.

#+ df_join
species_lookup <- data.frame(
  Species=levels(iris$Species)
  ,Group=c('A','B','C')
)
species_lookup

#' `left_join()` keeps every row from the data frame on the left and adds
#' matching information from the data frame on the right.

#+ df_left_join
iris %>% left_join(species_lookup,by='Species') %>% head()

#' `inner_join()` keeps only rows whose key occurs in both data frames.

#+ df_inner_join
species_lookup2 <- data.frame(
  Species=c('setosa','versicolor')
  ,Group=c('A','B')
)
iris %>% inner_join(species_lookup2,by='Species') %>% head()
iris %>% inner_join(species_lookup2,by='Species') %>% nrow()

#' ## Changing between wide and long data
#'
#' Some analyses need one row per observation with several measurement columns;
#' others need one row per measurement. `pivot_longer()` and `pivot_wider()`
#' change between these two arrangements. These functions are in the `tidyr`
#' package, so here we use `tidyr::` before the function name instead of loading
#' the whole package.

#+ df_reshape
iris_small <- head(iris,3) %>% mutate(ID=seq_len(n()))
iris_small

iris_long <- iris_small %>% tidyr::pivot_longer(
  cols=c(Sepal.Length,Sepal.Width,Petal.Length,Petal.Width)
  ,names_to='Measure'
  ,values_to='Value'
)
iris_long

iris_wide <- iris_long %>% tidyr::pivot_wider(
  names_from=Measure
  ,values_from=Value
)
iris_wide

#' ## Importing and exporting data
#'
#' `rio::export()` chooses the output format based on the file extension.
#' `rio::import()` does the same thing when reading a file.

#+ file_import_export
example_file <- tempfile(fileext='.csv')
rio::export(iris,example_file)
iris_imported <- rio::import(example_file)
head(iris_imported)
file.remove(example_file)

#' `list.files()` can be used to see files in a folder. `tempdir()` gives a
#' temporary folder that R can safely use for examples on any computer.

#+ file_list
example_folder <- tempdir()
list.files(example_folder)

#' If several data files are stored in one folder, `list.files()` with
#' `full.names=TRUE` returns the complete path to each file. Here we create two
#' files first so the example does not depend on anything already being on your
#' computer.

#+ file_import_multiple
example_folder <- tempfile()
dir.create(example_folder)
rio::export(head(iris,5),file.path(example_folder,'iris1.csv'))
rio::export(tail(iris,5),file.path(example_folder,'iris2.csv'))

example_files <- list.files(example_folder
                            ,pattern='[.]csv$'
                            ,full.names=TRUE)
example_files

example_data <- sapply(example_files,import,simplify=FALSE) %>%
  setNames(basename(example_files))
names(example_data)
example_data$iris1.csv
unlink(example_folder,recursive=TRUE)

#' # Randomness and reproducibility
#'
#' Functions such as `sample()`, `runif()`, and `rnorm()` generate random
#' values. Random functions normally give different results each time.

#+ randomness
sample(1:10,5)
runif(5)
rnorm(5,mean=10,sd=2)

#' `set.seed()` lets you reproduce the same random result. If you use the same
#' seed immediately before the same random operation, you get the same result.

#+ randomness_seed
set.seed(5050)
sample(1:10,5)
set.seed(5050)
sample(1:10,5)

#' This is useful when random sampling is part of an analysis and you want
#' somebody else to be able to reproduce exactly what you did.

#+ randomness_dataframe
set.seed(5050)
iris_sample <- iris[sample(seq_len(nrow(iris)),10),]
iris_sample

#' # Writing simple functions
#'
#' A function lets you give a name to a reusable operation. Arguments listed
#' inside `function()` become temporary variables whose values are supplied when
#' the function is called.

#+ simple_function
convert_inches <- function(cm){
  cm/2.54
}
convert_inches(10)
convert_inches(iris$Sepal.Length) %>% head()

#' Functions can have more than one argument.

#+ simple_function_multiple
range_size <- function(low,high){
  high-low
}
range_size(3,8)
range_size(min(iris$Sepal.Length),max(iris$Sepal.Length))

#' Arguments can also have default values. If a value with a default is not
#' supplied when the function is called, R uses the default.

#+ simple_function_default
convert_temperature <- function(x,from='C'){
  if(from=='C') return(x*9/5+32)
  if(from=='F') return((x-32)*5/9)
}
convert_temperature(0)
convert_temperature(32,from='F')
