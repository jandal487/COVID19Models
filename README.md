# Modeling 2019 Novel Coronavirus (2019-nCoV)
This project repository contains script to understand and model covid19 dataset taken from www.kaggle.com in dataset's sections. The corresponding URL is: https://www.kaggle.com/sudalairajkumar/novel-corona-virus-2019-dataset

2019 Novel Coronavirus (2019-nCoV) is a virus (more specifically, a coronavirus) identified as the cause of an outbreak of respiratory illness first detected in Wuhan, China. Early on, many of the patients in the outbreak in Wuhan, China reportedly had some link to a large seafood and animal market, suggesting animal-to-person spread. However, a growing number of patients reportedly have not had exposure to animal markets, indicating person-to-person spread is occurring. At this time, it’s unclear how easily or sustainably this virus is spreading between people - CDC

![Alt text](covid19.jpg?raw=true? "COVID19(https://www.thedenverchannel.com/news/national/coronavirus/here-are-the-current-statuses-of-possible-novel-coronavirus-cases-in-colorado)")

## Getting started
The instructions below will give you important hints on how to get the R 
functions of this project up and running on your local/remote machine, for your 
own usage, development and/or testing purposes.


### Coding golden rules
In order to collaborate to this project, please follow the following coding 
rules:
* File naming: utils_functions.R (lower case snake case),
* Function naming: CalcAverageCons (fully CamelCase),
* Variable naming: variableName (leading lower case letter followed by CamelCase),
* Constants: kConstantName (leading k, followed by CamelCase),
* Use always ``` <- ``` for value assignment and not ``` = ```,
* Use always ``` = ``` for function arguments and not ``` <- ```,
* Spacing: Place a space around all binary operators (``` = ```, ``` + ```, ``` * ```, ``` >= ```, etc.), except in function arguments,
* Never hardcode usernames, passwords and system access information in your 
developments and upload them onto the GITHUB repository,
* Never upload real data onto the GITHUB repository.
* Last but not least, exploit the potential of modern practices and technology...
![Alt text](out_tech.jpg?raw=true? "Outdated Technology")

### Prerequisites
In order to be able to use this project reporitory, the following R libraries 
are required:
* ggplot2,
* reshape2,
* dplyr


### Unit testing
To perform all available *Unit Tests* of the developed functions in the project, 
run:

* the following command in an R console
```> devtools::load_all("."); print(testthat::test_dir("tests/", reporter = "summary"))```,

* or the linux shell command below
```$ Rscript -e 'devtools::load_all("."); print(testthat::test_dir("tests/", reporter = "summary"))'```,

both from the root directory of the project.

P.S: The Unit Tests are not included at the moment.


### R Package bulding
There are two types of packages that can be built out of this project, a 
*binary* package for R running on Windows OS, or a *source* package for R 
running on Linux / UNIX OS. 

* To build a *binary* package, adapt and run the following commands 
on an R console:

```> devtools::create("path/to/package/package_name")```, if not created yet

```> devtools::document(roclets=c('rd', 'collate', 'namespace'))```

```> devtools::build(binary = TRUE, args = c('--preclean'))```

* To build a *source* package, adapt and run the following commands 
on an R console:

```> devtools::create("path/to/package/package_name")```, if not created yet

```> devtools::document(roclets=c('rd', 'collate', 'namespace'))```

```> devtools::build()```


### Author, dependencies  and build version
See DESCRIPTION file.

### License
See LICENSE file.
