# About the Data: <h4>
This data approach student achievement in secondary education of two Portuguese schools. The data attributes include student grades, demographic, social and school related features) and it was collected by using school reports and questionnaires. Two datasets are provided regarding the performance in two distinct subjects: Mathematics (mat) and Portuguese language (por). In [Cortez and Silva, 2008], the two datasets were modeled under binary/five-level classification and regression tasks. Important note: the target attribute G3 has a strong correlation with attributes G2 and G1. This occurs because G3 is the final year grade (issued at the 3rd period), while G1 and G2 correspond to the 1st and 2nd period grades. It is more difficult to predict G3 without G2 and G1, but such prediction is much more useful (see paper source for more details).
Source: https://www.kaggle.com/dipam7/student-grade-prediction

# Using this Shiny App we will explore the following:<h6>

	* What are the parameters influences Grades e.g.
		* Education Of Parents
		* Financial Situation
		* Males Vs Female/ Age of the Students
		* Commute time
	* Unsupervised Learning from the Data/ Student Segments
	* Predict Students Grade: Decision Tree/ K Nearest Neighbor
	

# Feature Description <h6>
**school**

**student's school** (binary: 'GP' - Gabriel Pereira or 'MS' - Mousinho da Silveira)
**sex**

student's sex (binary: 'F' - female or 'M' - male)

**age**

student's age (numeric: from 15 to 22)

**address**

student's home address type (binary: 'U' - urban or 'R' - rural)

**famsize**

family size (binary: 'LE3' - less or equal to 3 or 'GT3' - greater than 3)
Pstatus

**parent's cohabitation status** (binary: 'T' - living together or 'A' - apart)
Medu

**mother's education** (numeric: 0 - none, 1 - primary education (4th grade), 2 â€“ 5th to 9th grade, 3 â€“ secondary education or 4 â€“ higher education)
Fedu

**father's education** (numeric: 0 - none, 1 - primary education (4th grade), 2 â€“ 5th to 9th grade, 3 â€“ secondary education or 4 â€“ higher education)
Mjob

**mother's job** (nominal: 'teacher', 'health' care related, civil 'services' (e.g. administrative or police), 'at_home' or 'other')
Fjob

**father's job** (nominal: 'teacher', 'health' care related, civil 'services' (e.g. administrative or police), 'at_home' or 'other')
reason

**reason to choose this school** (nominal: close to 'home', school 'reputation', 'course' preference or 'other')
**guardian**

student's guardian (nominal: 'mother', 'father' or 'other')

**traveltime**

**home to school travel time** (numeric: 1 - <15 min., 2 - 15 to 30 min., 3 - 30 min. to 1 hour, or 4 - >1 hour)

**studytime**

weekly study time (numeric: 1 - <2 hours, 2 - 2 to 5 hours, 3 - 5 to 10 hours, or 4 - >10 hours)

**failures**

number of past class failures (numeric: n if 1<=n<3, else 4)

**schoolsup**

extra educational support (binary: yes or no)

**famsup**

family educational support (binary: yes or no)

**paid**

extra paid classes within the course subject (Math or Portuguese) (binary: yes or no)
activities

**extra-curricular activities** (binary: yes or no)

**nursery**

attended nursery school (binary: yes or no)

**higher**

wants to take higher education (binary: yes or no)

**internet**

Internet access at home (binary: yes or no)

**romantic**

with a romantic relationship (binary: yes or no)

**famrel**

quality of family relationships (numeric: from 1 - very bad to 5 - excellent)

**freetime**

free time after school (numeric: from 1 - very low to 5 - very high)

**goout**

going out with friends (numeric: from 1 - very low to 5 - very high)

**Dalc**

workday alcohol consumption (numeric: from 1 - very low to 5 - very high)

**Walc**

weekend alcohol consumption (numeric: from 1 - very low to 5 - very high)

**health**

current health status (numeric: from 1 - very bad to 5 - very good)

**absences**

number of school absences (numeric: from 0 to 93)

**G1**

first period grade (numeric: from 0 to 20)

**G2**

second period grade (numeric: from 0 to 20)

**G3**

final grade (numeric: from 0 to 20)


# About the App: <h4>

The Shiny app has following pages and corresponding sections:

**Sample**

This page contains summary of raw data. User can download the CSV file.
This page also shows the numerical summary of raw data.

**Data Exploration**
The top section of this page contains the correlation plot among all predictor variables.
The bottom section of the page shows relationship between predictor variables and response score variable. User can select as many predictor variables s/he needs.

**Cluster**
The cluster page shows the k-means cluster plots. 
The top section line plot helps user to select optimal number of clusters.

The bottom section has the drop down for user to select number of cluster and the graph refreshes accordingly.           

**Predict**

The first drop down filter lets user select the supervised model s/he needs to use in study. Models used are 1. Regression Tree and, 2. Bagging

Regression Tree: The top graph is to help user select optimal number of tree.
The bottom section is for user to select custom values of hyper parameters needed to create the tree.

Bagging: The top section shows the importance of predictor variables as found from from bagging model.
The bottom section allows user to select folds for cross validation and rerun the model.