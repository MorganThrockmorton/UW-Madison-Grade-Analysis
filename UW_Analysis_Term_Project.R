library(readr)

#----------------------------------------------------------------------------
# Import data from UW website for term_code information.
# Copied and pasted data into Excel and will import from there.

term_codes <- na.omit(data.frame(read_csv("D:/uw-madison-courses/uw-madison-courses/term_codes.csv", skip = 4)))
# used na.omit since I did not clean the data in Excel and wanted to remove empty rows.
# Skipped the first few rows during the import as well since they were empty.

# Create separate variables for codes associated with the fall and spring terms.
fall <- term_codes[,c(1,4)]
spring <- term_codes[,c(1,2)]

# Add a fall or spring column to factor later.
fall$semester <- 'Fall'
spring$semester <- 'Spring'

# rename columns to merge
colnames(fall) <- c('year','term_code','semester')
colnames(spring) <- c('year','term_code','semester')
term <- rbind(fall,spring)

#----------------------------------------------------------------------------

# Import course_offerings and courses to clean and merge.

course_offerings <- data.frame(read_csv("D:/uw-madison-courses/uw-madison-courses/course_offerings.csv"))[,-4]
# do not need the names column (4)
# contains missing values and names are in "courses" data

courses <- data.frame(read_csv("D:/uw-madison-courses/uw-madison-courses/courses.csv"))[,-3]
# removed column (3) since the number is not unique.

# rename column names for merging
colnames(courses) <- c('course_uuid','course_name')
colnames(course_offerings)[1] <- 'course_offering_uuid'

# merge data frames and remove course_uuid since it is not in any other tables
courses <- merge(course_offerings,courses)[,-1]

# merge together courses and term and remove term_code since it is not in any other tables
course_info <- merge(courses,term)[,-1]

#----------------------------------------------------------------------------

subjects <- data.frame(read_csv("D:/uw-madison-courses/uw-madison-courses/subjects.csv"))[,-3]
# do not want the subject abbreviation, column (3)
subject_memberships <- data.frame(read_csv("D:/uw-madison-courses/uw-madison-courses/subject_memberships.csv"))

# rename columns and merge these tables together, removing subject_code after merge
colnames(subjects) <- c('subject_code','subject_name')
course_subjects <- merge(subject_memberships,subjects)[,-1]

#----------------------------------------------------------------------------

# import teachings and instructors, rename columns, merge together, then remove instructor_id

teachings <- data.frame(read_csv("D:/uw-madison-courses/uw-madison-courses/teachings.csv"))
instructors <- data.frame(read_csv("D:/uw-madison-courses/uw-madison-courses/instructors.csv"))

colnames(instructors) <- c('instructor_id','instructor_name')
professors <- merge(teachings,instructors)[,-1]

#----------------------------------------------------------------------------

# import grade_distributions and leave as be for right now.
grade_distributions <- data.frame(read_csv("D:/uw-madison-courses/uw-madison-courses/grade_distributions.csv"))

#----------------------------------------------------------------------------

# import sections and remove room_uuid and schedule_uuid since I will not be using those tables.
sections <- data.frame(read_csv("D:/uw-madison-courses/uw-madison-courses/sections.csv"))[,-c(5,6)]

# rename columns to later merge
colnames(sections)[1] <- 'section_uuid'
colnames(sections)[4] <- 'section_number'

#----------------------------------------------------------------------------

# gives an example of a course_offering_uuid that is associated with more than one subject
# second line gives the course_name associated with the course_offering_uuid
course_subjects[which(course_subjects$course_offering_uuid == 'b273329d-6200-3afd-af85-f795ad91f90f'),-1]
course_info[which(course_info$course_offering_uuid == 'b273329d-6200-3afd-af85-f795ad91f90f'),-1]

# In my analysis I showed that I decided to keep these extra values.
# Next step will be to merge all of the course_information from course_subjects and course_info together with grade_distributions
course_information <- merge(course_subjects,course_info)
course_grades <- merge(course_information,grade_distributions)

#----------------------------------------------------------------------------

# merging together lost 391 values so I wanted to do a full_join to understand why some of the values were lost
course_sections <- dplyr::full_join(sections,professors)

# counting the number of times a course_offering_uuid and a section_number repeats itself
x <- plyr::count(course_sections, vars = c('course_offering_uuid','section_number'))
y <- x[which(x$freq == 1),]

# removing these duplicated values and then removing freq column
# removed 63267 of these classes
madison_sections <- merge(y,course_sections)[,-3]

#----------------------------------------------------------------------------
# merging all the data into one large data frame
madison_grades <- merge(madison_sections,course_grades)

# removing section_number and section_uuid since they will not be necessary for analysis
madison_grades <- madison_grades[,-c(2,3)]

# rename the columns to make it easier for myself
colnames(madison_grades) <- c('course_uuid','section_type','instructor','subject','course_name','year','semester',
                              'A','AB','B','BC','C','D','F','satisfactory','unsatisfactory','credit','no_credit',
                              'progress','incomplete','no_work','no_report','other')


# factor all categorical data types
madison_grades$course_uuid <- factor(madison_grades$course_uuid)
madison_grades$section_type <- factor(madison_grades$section_type)
madison_grades$instructor <- factor(madison_grades$instructor)
madison_grades$subject <- factor(madison_grades$subject)
madison_grades$course_name <- factor(madison_grades$course_name)
madison_grades$year <- factor(madison_grades$year)
madison_grades$semester <- factor(madison_grades$semester)

#----------------------------------------------------------------------------

# count the total number of graded students (those who received a letter grade)
madison_grades$total_graded_students <- apply(madison_grades[,8:14], MARGIN = 1, sum)

# count the total number of students who passed or failed the class (including credit or no credit)
madison_grades$total_pass_fail_students <- apply(madison_grades[,8:16], MARGIN = 1, sum)

# count the total number of students
madison_grades$all_students <- apply(madison_grades[,8:23], MARGIN = 1, sum)

#----------------------------------------------------------------------------

# see if there are any rows which have no recorded students (are empty)
dim(madison_grades[which(madison_grades$all_students == 0),])  # 98194

# remove the rows where there are no students
madison_grades <- madison_grades[-which(madison_grades$all_students == 0),]

#----------------------------------------------------------------------------

# calculate the proportion of students who are failed or passed individual courses
madison_grades$total_passing <- apply(madison_grades[,c(8,9,10,11,12,15)], MARGIN = 1, sum)
madison_grades$total_failing <- apply(madison_grades[,c(13,14,16)], MARGIN = 1, sum)

#----------------------------------------------------------------------------

# calculate the weight of each letter grade
madison_grades$weighted_A <- madison_grades$A*4
madison_grades$weighted_AB <- madison_grades$AB*3.5
madison_grades$weighted_B <- madison_grades$B*3
madison_grades$weighted_BC <- madison_grades$BC*2.5
madison_grades$weighted_C <- madison_grades$C*2
madison_grades$weighted_D <- madison_grades$D
madison_grades$weighted_F <- 0

# calculate the sum of the total weight 
madison_grades$total_weight <- apply(madison_grades[,29:35], MARGIN = 1, sum)

# calculates gpa for each course
madison_grades$class_gpa <- madison_grades$total_weight / madison_grades$total_graded_students

#----------------------------------------------------------------------------

# to calculate the average gpa of each course_uuid, I will use tapply
# then I will convert these to a data frame and change the column names to merge
average_gpa <- tapply(madison_grades$class_gpa, madison_grades$course_uuid, mean)
average_gpa <- data.frame(course_uuid = names(average_gpa), average_gpa)

madison_grades <- plyr::join(madison_grades,average_gpa)

#----------------------------------------------------------------------------

# calculate the z score for every class_gpa
madison_grades$normalized_gpa <- (madison_grades$class_gpa - mean(madison_grades$class_gpa, na.rm = T))/sd(madison_grades$class_gpa, na.rm = T)

# classify each as above or below the average. 
# might be useful later for graphs or maching learning purposes.
madison_grades$gpa_classification <- ifelse(madison_grades$normalized_gpa < 0, 'below', 'above')

#----------------------------------------------------------------------------
library(ggplot2)

# shows the distribution average_gpa's based and color codes it by the year 
g1 <- ggplot(madison_grades, aes(average_gpa)) + theme_bw()
g1_colors <- scale_fill_manual(values =c('#BA0000','#C90000','#D40202','#E60101','#F60404','#FF2E2E','#F54545','#F35656','#E96D6D','#E67D7D','#EE9C9C','#F5B9B9'))
g1_labs <- labs(title='Histogram of Average GPA', x = 'Average GPA', y = 'Count')
g1_hist <- geom_histogram(aes(fill=year), binwidth = .19, col = 'black', size = .1)
g1 + g1_labs + g1_hist + g1_colors

#----------------------------------------------------------------------------

# calculates the average gpa by subject and then orders it from lowest gpa to highest
# change the row names because the rownames being the subject is ugly
subject_gpa_summary <- tapply(madison_grades$average_gpa[!is.na(madison_grades$average_gpa)], madison_grades$subject[!is.na(madison_grades$average_gpa)], mean)
subject_gpa_summary <- na.omit(data.frame(subject = names(subject_gpa_summary), average_gpa = subject_gpa_summary))
subject_gpa_summary <- subject_gpa_summary[order(subject_gpa_summary$average_gpa),]
rownames(subject_gpa_summary) <- c(1:190)


#----------------------------------------------------------------------------

# counts the total number of grades students by subject
# I am going to merge these with subject_gpa_summary to compare gpa to number of graded students
total_students <- tapply(madison_grades$total_graded_students, madison_grades$subject, sum)
total_students <- na.omit(data.frame(subject = names(total_students), total_graded_students = total_students))
gpa_analysis <- merge(subject_gpa_summary,total_students)
gpa_analysis <- gpa_analysis[order(gpa_analysis$average_gpa),]


#----------------------------------------------------------------------------

# create a scatterplot that shows relationship of average gpa to number of graded students
p1 <- ggplot(gpa_analysis, aes(total_graded_students,average_gpa))
p1_labs <- labs(title = "Jitter Plot", subtitle = "Total Students vs Average GPA by Subject", x = 'Total Graded Students', y = 'Average GPA')
p1 + p1_labs + geom_jitter(width =.5, size = 1.5, col = 'red') + theme_bw()

#----------------------------------------------------------------------------

# looks at the quantile of the distributin of the total number of graded students
quantile(gpa_analysis$total_graded_students)

# I only want to consider subjects where the number of students is above the 25th percentile
upper_75th <- gpa_analysis[which(gpa_analysis$total_graded_students > 2134),]


# create a lollipop chart of top 10 classes with the highest gpa
top10 <- tail(upper_75th, n = 10)
top10$subject <- reorder(top10$subject,top10$average_gpa)
p2 <- ggplot(top10, aes(subject,average_gpa)) + theme_bw() + ylim(3.8,4)
p2_labs <- labs(title = 'Top 10 Average GPA By Subject', y = 'Average GPA')
p2_lollipop <- geom_segment(aes(x = subject , xend = subject, y = 3.8, yend = average_gpa), color = 'red')
p2 + p2_labs + p2_lollipop + geom_point(size = 3, color = 'red') + coord_flip()

# create a lollipop chart of the top 10 classes with the lowest gpa
bottom10 <- head(upper_75th, n = 10)
bottom10$subject <- reorder(bottom10$subject,bottom10$average_gpa)
p3 <- ggplot(bottom10, aes(subject,average_gpa)) + theme_bw() + ylim(3.1,3.4)
p3_labs <- labs(title = 'Bottom 10 Average GPA by Subject', y = 'Average GPA')
p3_lollipop <- geom_segment(aes(x = subject, xend = subject, y = 3.1, yend = average_gpa), color = 'blue')
p3 + p3_labs + p3_lollipop + geom_point(size = 3, color = 'blue') + coord_flip()

#----------------------------------------------------------------------------

# create a data frame where there are no NA values for average_gpa
# this will make using tapply() easier since I will not have to remove the NA values then
madison_analysis <- madison_grades[!is.na(madison_grades$average_gpa),]

# calculate the average gpa of each course by the subject mathematics
# rename columns and then order them from lowest to highest
math_gpa <- tapply(madison_analysis$average_gpa[which(madison_analysis$subject == 'Mathematics')], 
                   madison_analysis$course_name[which(madison_analysis$subject == 'Mathematics')], mean)
math_gpa <- na.omit(data.frame(course = names(math_gpa), average_gpa = math_gpa))
math_gpa <- math_gpa[order(math_gpa$average_gpa),]
rownames(math_gpa) <- c(1:dim(math_gpa)[1])

#----------------------------------------------------------------------------

# create a variable that stores the top 20 lowest average gpa
math_top20 <- madison_analysis[madison_analysis$course_name %in% head(math_gpa$course, n = 20),]

# create a stem and leaf plot using the information in math_top20
p4 <- ggplot(math_top20, aes(course_name, average_gpa)) + theme_bw()
p4_labs <- labs(title = 'Mathematics Box Plot', subtitle = "Top 20 Lowest Average GPA", x = "Course Name",y = 'Class GPA')
p4 + geom_boxplot(varwidth = T, fill = 'red') + p4_labs +  coord_flip()

#----------------------------------------------------------------------------

# calculate the average gpa of each instructor for each year
yearly_gpa <- aggregate(madison_analysis$class_gpa, by = list(madison_analysis$instructor, madison_analysis$year), mean)
colnames(yearly_gpa) <- c('instructor','year','year_gpa')

# count the number of years a teacher taught
# will only consider professors that taught every year from 2006 to 2017
w <- plyr::count(yearly_gpa, vars = c('instructor'))
w <- w[which(w$freq == 12),]

# merge w with yearly_gpa
# get rid of the frequency
yearly_gpa <- merge(yearly_gpa,w)
yearly_gpa <- yearly_gpa[,-4]

# calculate the variance of the average gpa of each instructor by year
# change the column names and then order it from lowest to highest variance
gpa_var <- aggregate(year_gpa ~ instructor, data = yearly_gpa, var)
colnames(gpa_var)[2] <- 'variance'
gpa_var <- gpa_var[order(gpa_var$variance),]

#----------------------------------------------------------------------------

# create a variable that stores information related to professor "Fabian waleffe"
# desregard columns from madison analysis that were irrelevant 
fabian <- madison_analysis[which(madison_analysis$instructor == 'FABIAN WALEFFE'), c(3:7,37)]

# create a summary of class gpa by course and year
# merge together the data frames
fabian_summary <- aggregate(class_gpa ~ course_name + year, data = fabian, mean)
colnames(fabian_summary)[3] <- 'avg_gpa' 
fabian <- merge(fabian,fabian_summary)

#----------------------------------------------------------------------------

# plot information on fabian to understand grade distribution of course by year
p5 <- ggplot(fabian, aes(x = year, y = avg_gpa, fill = course_name)) + theme_bw()
p5_labs <- labs(title = 'Fabian Waleffe', subtitle = 'Average GPA of Courses by Year',x = 'Year', y = 'Average GPA')
p5_color <- scale_fill_brewer(palette = 'Blues')
p5 + p5_labs + p5_color + geom_bar(stat = 'identity', position = 'dodge', col = 'black')

#----------------------------------------------------------------------------
# subset the data to include data only from 2017
uw_2017 <- madison_analysis[which(madison_analysis$year == '2017'),]

# count how many classes a particular professor taught for each subject
# then count the sum of all the total classes taught by professors 
# order on frequency to find the subject with the most classes taught by professors
a <- plyr::count(uw_2017, vars = c('subject','instructor'))
b <- aggregate(freq ~ subject, a, sum)
b <- b[order(b$freq),]

#----------------------------------------------------------------------------------

# further subset the data to include only the subject mathematics 
uw_math <- uw_2017[which(uw_2017$subject == 'Mathematics'),]

# count the number of classes taught individual professor
math_prof <- a[which(a$subject == 'Mathematics'),]
math_prof <- math_prof[order(math_prof$freq),]

# count the number of graded students by professor and then order it 
students <- aggregate(total_graded_students ~ instructor, uw_math, sum)
students <- students[order(students$total_graded_students),]


#----------------------------------------------------------------------------------

library(rvest)

# scrape the data for the top 5 math professors with the most amount of students in 2017.
# cleaning process is the same for all of these. 

prof1 <- html_text(html_nodes(read_html('https://www.ratemyprofessors.com/ShowRatings.jsp?tid=1852275'),'.breakdown-header .grade'))
prof1 <- trimws(prof1, which = c('both'))
prof1 <- as.numeric(unlist(strsplit(prof1,split = '%')))
#Daniel Erman

prof2 <- html_text(html_nodes(read_html('https://www.ratemyprofessors.com/ShowRatings.jsp?tid=2043547'),'.breakdown-header .grade'))
prof2 <- trimws(prof2, which = c('both'))
prof2 <- as.numeric(unlist(strsplit(prof2,split = '%')))
#Tonghai Yang

prof3 <- html_text(html_nodes(read_html('https://www.ratemyprofessors.com/ShowRatings.jsp?tid=1408142'),'.breakdown-header .grade'))
prof3 <- trimws(prof3, which = c('both'))
prof3 <- as.numeric(unlist(strsplit(prof3,split = '%')))
#Laurentiu Maxim

prof4 <- html_text(html_nodes(read_html('https://www.ratemyprofessors.com/ShowRatings.jsp?tid=1356509'),'.breakdown-header .grade'))
prof4 <- trimws(prof4, which = c('both'))
prof4 <- as.numeric(unlist(strsplit(prof4,split = '%')))
#Joe Miller

prof5 <- html_text(html_nodes(read_html('https://www.ratemyprofessors.com/ShowRatings.jsp?tid=2094033'),'.breakdown-header .grade'))
prof5 <- trimws(prof5, which = c('both'))
prof5 <- as.numeric(unlist(strsplit(prof5,split = '%')))
#Alexander Hanhart

ratings <- c(prof1[1], prof2[1], prof3[1], prof4[1], prof5[1])
take_again <- c(prof1[2], prof2[2],prof3[2], prof4[2], prof5[2])
difficulty <- c(prof1[3], prof2[3],prof3[3], prof4[3], prof5[3])
instructor <- c('DANIEL ERMAN', 'TONGHAI YANG', 'LAURENTIU MAXIM', 'JOSEPH MILLER', 'ALEXANDER HANHART')

prof_ratings <- data.frame(ratings, take_again, difficulty, instructor)

#----------------------------------------------------------------------------------

# subset the data only for the math professors I scraped data for
# I used madison_analysis instead of uw_math since I wanted to include all the classes the professor taught
# the ratings on rate my professor are not necessarily from 2017
prof_analysis <- madison_analysis[madison_analysis$instructor %in% tail(students$instructor, n = 5),-c(1,2,8:24,26,29:36,38:40)]
prof_analysis$p_failing <- prof_analysis$total_failing / prof_analysis$total_pass_fail_students
prof_analysis$p_passing <- 1 - prof_analysis$p_failing

# calculate the average_gpa for each professor
prof_gpa <- aggregate(class_gpa ~ instructor, prof_analysis, mean)
colnames(prof_gpa)[2] <- "prof_avg"

# 
prof_analysis <- merge(prof_analysis,prof_gpa)
prof_analysis <- merge(prof_analysis,prof_ratings)
prof_analysis$difficulty <- factor(prof_analysis$difficulty)

#----------------------------------------------------------------------------------

#use a scatter plot and facet information on professor
p6 <- ggplot(prof_analysis, aes(x = p_passing, y = class_gpa, size = ratings, col = difficulty)) + theme_bw()
p6_labs <- labs(title = 'Analysis of Mathematics Professors', x = 'Proportion Passing', y = 'Class GPA')
p6_color <- scale_color_manual(values = c('#82bbed','#5690c4','#2666a0','#0c5496','#003668'))
p6_guides <- guides(colour = guide_legend(override.aes = list(size = 6)))
p6 + p6_labs + facet_grid(.~instructor) + geom_point() + p6_guides + p6_color

#----------------------------------------------------------------------------------

# observe professor from previous analysis
prof_analysis[which(prof_analysis$instructor == 'DANIEL ERMAN'),c(1,3,9)]


#----------------------------------------------------------------------------------

rm(course_offerings)
rm(courses)
rm(fall)
rm(instructors)
rm(teachings)
rm(term)
rm(term_codes)
rm(subjects)
rm(subject_memberships)
rm(spring)
rm(course_info)
rm(course_subjects)
rm(course_information)
rm(grade_distributions)
rm(sections)
rm(professors)
rm(course_grades)
rm(course_sections)
rm(x)
rm(y)
rm(madison_sections)
rm(average_gpa)
rm(g1)
rm(g1_colors)
rm(g1_hist)
rm(g1_labs)
rm(p1)
rm(p1_labs)
rm(p2)
rm(p2_labs)
rm(p2_lollipop)
rm(p3)
rm(p3_labs)
rm(p3_lollipop)
rm(total_students)
rm(subject_gpa_summary)
rm(gpa_analysis)
rm(top10)
rm(bottom10)
rm(upper_75th)
rm(p4)
rm(p4_labs)
rm(math_gpa)
rm(math_top20)
rm(madison_grades)
rm(w)
rm(yearly_gpa)
rm(gpa_var)
rm(p5)
rm(p5_color)
rm(p5_labs)
rm(fabian)
rm(fabian_summary)
rm(a)
rm(b)
rm(uw_2017)
rm(prof1)
rm(prof2)
rm(prof3)
rm(prof4)
rm(prof5)
rm(students)
rm(math_prof)
rm(difficulty)
rm(name)
rm(ratings)
rm(take_again)
rm(p6)
rm(p6_color)
rm(p6_guides)
rm(p6_plot)
rm(p6_labs)
rm(prof_analysis)
rm(uw_math)
rm(prof_gpa)
rm(students)
rm(prof_ratings)
rm(difficulty)
rm(instructor)
rm(name)
rm(prof1)
rm(prof2)
rm(prof3)
rm(prof4)
rm(prof5)
rm(ratings)
rm(take_again)
rm(madison_analysis)
