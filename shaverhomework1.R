#Homework 1 Katie Shaver
#using fivethirtyeight
library(fivethirtyeight)

#Problem 1
data("congress_age")
#congress_age
library(dplyr)
#median of ALL of congress
medianage<-summarize(
group_by(congress_age,congress),
medianage = median(age)
)
library(ggplot2)
ggplot(medianage,aes(x=congress,y=medianage)) +
       geom_line(color="green") +
         geom_point(color="green") +
  labs(x = "Congress Number", y="Median Age", 
       title="Median Age of Congress Members Over Time")
#Looks like median age definitely increases over time overall
#Minimum of age ~49 around congress ~97
#Min age decreases from congress 80-97, then increases
#Pay attention to y axis, the overall range for median is only 8 years
congress_agenew<-summarize(
  group_by(congress_age,congress,chamber),
  medianage = median(age)
)
#I chose to plot both chambers on the same graph rather than faceting
#for ease of comparison
ggplot(congress_agenew, aes(x=congress, y=medianage, color = chamber)) +
  geom_line() +
  geom_point()+
  labs(x = "Congress Number", y="Median Age", 
       title="Median Age of Congress Members Over Time by Chamber"
  )
#Senate (blue) consistently has higher age compared to house
#Overall age trends the same between chambers

#Problem 2
data("college_grad_students")
#Add lines connecting the two points from the same major
lines<-data.frame(
  xg=college_grad_students$grad_unemployment_rate,
  yg=college_grad_students$grad_median,
  xn=college_grad_students$nongrad_unemployment_rate,
  yn=college_grad_students$nongrad_median
)
ggplot(college_grad_students) +
  geom_smooth(aes(x=grad_unemployment_rate, y=grad_median, color="Graduate"), method="lm", se=TRUE) +
  geom_point(aes(x=grad_unemployment_rate, y=grad_median, color="Graduate")) +
  labs(x="Unemployment Rate", y="Median Salary",
       title="Median Earnings based on Unemployment Rate") +
  geom_smooth(aes(x=nongrad_unemployment_rate, y=nongrad_median, color="Non-Graduate"), method="lm", se=TRUE) +
  geom_point(aes(x=nongrad_unemployment_rate, y=nongrad_median, color="Non-Graduate")) +
geom_segment(data=lines, aes(x=xg,y=yg, xend=xn, yend=yn))
#Add lines connecting the two points from the same major
lines<-data.frame(
  xg=college_grad_students$grad_unemployment_rate,
  yg=college_grad_students$grad_median,
  xn=college_grad_students$nongrad_unemployment_rate,
  yn=college_grad_students$nongrad_median
)
ggplot(college_grad_students)+
  geom_segment(data=lines, aes(x=xg,y=yg, xend=xn, yend=yn)
#Data looks SUPER MESSY, going to try binning
#add more spacing and lines at lower end of x axis to see the trend better
stackeddata<-data.frame(majordouble=rep(college_grad_students$major,2),
                        unemprate=c(college_grad_students$grad_unemployment_rate,
                                    college_grad_students$nongrad_unemployment_rate
                                    ),
                        group=rep(c("Graduate","Non-graduate"),
                                  each=nrow(college_grad_students)
                                  )
)
ggplot(stackeddata,aes(x=majordouble, y=unemprate, fill=group))+ 
  geom_col()
ggplot(college_grad_students, aes(x = major)) +
  geom_col(aes(y = grad_unemployment_rate), fill = "red") +
  geom_col(aes(y = grad_unemployment_rate+nongrad_unemployment_rate), fill = "blue", position="stack")

   

#Problem 3
data("bechdel")
ggplot(bechdel, aes(x=budget_2013, y=intgross_2013)) + 
geom_point() +
  geom_smooth(method="lm", se=TRUE)+
  scale_x_log10() +
  scale_y_log10()+
  labs(
    x="log(Movie Budget adj. 2013 Inflation)",
    y="log(Worldwide Gross Revenue)",
    title="Movie budget vs. Worldwide gross revenue"
  )
#Data looked a bit messy as just a scatterplot, decided to add LOBF
#Adding diff colors for if the movie passes Bechdel
ggplot(bechdel, aes(x=budget_2013, y=intgross_2013, color=binary)) + 
  geom_point() +
  geom_smooth(method="lm", se=TRUE)+
  scale_x_log10() +
  scale_y_log10()+
  labs(
    x="log(Movie Budget adj. 2013 Inflation)",
    y="log(Worldwide Gross Revenue)",
    title="Movie budget vs. Worldwide gross revenue"
  )


            