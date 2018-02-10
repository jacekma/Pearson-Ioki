# My first concern is always, to visually inspect the data in Excel, have a look how it looks like, 
#scroll down a little bit, set some filters, see how the data spread etc - generally  to get some
#preliminary overview of the data

#Then my concern was to load the data into R.

setwd("/Users/Jacek/Desktop/audition_project_2018/")
data_ioki <- read.csv(file = "data2018.csv", sep = ";", na.strings=c("","NA"))
# na.strings argument was used since data set contained blank cells that were as a matter of fact NA's 

## They I realize, there are as a matter of fact, many learners from many different countries, that take some units. 
#Some take more units, some takes less - no idea why. Also when I run frequncy table on countries, I how astound that
# there are so many countries that are so underrepresented and that there were so enormous disproporionts
#I realized many any country-wise analysis would require dropping lest frequent countries, I set the threshold at 500, 
#although one could choose different value

## Here I guess I must have made some mistake


data_ioki2 %>% select(learner_id, country) %>% 
  filter(complete.cases(learner_id, country)) %>%
  distinct() %>% 
  count(country, sort = T)

# Because If I sum up the number of learners it is 12117. But if I run...  
nlevels(as.factor(data_ioki2$learner_id))

#... it says 12112. So I must have somehow lost 5 entries, but frankly speaking, I couldnt figure how - sorry about that- 
#maybe there were some duplications?

## Then I wanted to check whether country has some influence on avg_score. BTW I made avg_score my main variable
#of interest since this is some sort of proxy how and whether the students really learn something

p<-ggplot(data_ioki2, aes(x = reorder(country, -avg_score), y = avg_score), color = country) +
  geom_bar(stat = "summary", fun.y = "mean", fill ="dark green") +
  ggtitle("Average scores for all units for each of the country") +
  xlab(paste("Countries")) 
plot(p)


#Then I wanted to check if all units are covered equally

sort(decreasing = T, table(data_ioki$unit))

# but they dont, I still  dont know how, but I stated that in the report. I guess some didnt make it yet to the 
#chapters, or maybe they dropped out? Anyway, it was difficult to run some comparisons between students, so I moved
#to another aspects.

#tapply(data_ioki2$avg_score, data_ioki2$unit, mean) I know there is a way to run these things with summarise and mean
#within one pipe, but havent figured that out yet...

## Oh and at this point 

data_ioki2%>% select(avg_score, completion, unit)  %>% 
filter(unit == "1")%>% 
  ggplot(mapping = aes(x = avg_score, y = completion)) +
  geom_point() + 
  geom_smooth(se = FALSE)+
  geom_hline(yintercept = mean(data_ioki2$completion), linetype="dotted", size = 1, color = "red") +
  geom_vline(xintercept = mean(data_ioki2$avg_score), linetype="dotted", size = 1, color = "red") 

# If I havent filtered out other units, the plot would be so heavy and so hard to use and I would take a lot of time to open it

## Anyway, I still have that idea that I havent analysed so many issues. I have left out completely in_course  variable
# But I run some analyses, and the vast majority of the learners where type "t", also type "t" was slightly better than "f"
# so this is one could expected, because its easier with the teacher.