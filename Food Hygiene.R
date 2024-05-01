```{r setup, message=FALSE}

library(tidyverse)
library(janitor)
library(gridExtra)
library(Hmisc)
library(emmeans)
library(dplyr)
options(width=100)
```

---
  
  # 2019-20-enforcement-data-food-hygiene
  
  ```{r}
Food_Hygiene_data <- read_csv("2019-20-enforcement-data-food-hygiene.csv")
str(Food_Hygiene_data)
summary(Food_Hygiene_data)

Food_Hygiene_data <- na.omit(Food_Hygiene_data)
```

```{r}
Food_Hygiene_data$`Total%ofInterventionsachieved-premisesratedA`[Food_Hygiene_data$`Total%ofInterventionsachieved-premisesratedA` == "NR"] = "100"
Food_Hygiene_data$`Total%ofInterventionsachieved-premisesratedA` <- as.numeric(Food_Hygiene_data$`Total%ofInterventionsachieved-premisesratedA`)
Food_Hygiene_data$`Total%ofBroadlyCompliantestablishments-A`[Food_Hygiene_data$`Total%ofBroadlyCompliantestablishments-A` == "NP"] = "0"
Food_Hygiene_data$`Total%ofBroadlyCompliantestablishments-A` <- as.numeric(Food_Hygiene_data$`Total%ofBroadlyCompliantestablishments-A`)
summary(Food_Hygiene_data)
```

### Distribution Across the Local Authorities (LAs) of the Percentage of Enforcement Actions Successfully Achieved

ALL IMPACTS LEVEL COMBINED : 
  ```{r}
ggplot(data = Food_Hygiene_data) +
  geom_histogram(binwidth = 1, mapping = aes(x=`Total%ofInterventionsachieved(premisesratedA-E)`, fill = LAType)) +
  labs(x="% of Interventions Achieved (Rated A-E)", y="Number of Interventions Achieved", title="Distribution of Successful Interventions (All Impact Levels Combined)")

AllImpact_Plot <- ggplot(data = Food_Hygiene_data) +
  geom_histogram(binwidth = 1, mapping = aes(x=`Total%ofInterventionsachieved(premisesratedA-E)`, fill = LAType)) +
  labs(x="% of Interventions Achieved (Rated A-E)", y="Number of Interventions Achieved", title="Distribution of Successful Interventions (All Impact Levels)", caption = "Figure 1")

```

INDIVIDUAL IMPACTS LEVEL (A-E) :
  ```{r}

Individual_Plot <- grid.arrange(ggplot(data = Food_Hygiene_data) +
                                  geom_histogram(binwidth = 1, mapping = aes(x=`Total%ofInterventionsachieved-premisesratedA`, fill=LAType)) +
                                  labs(x="% of Interventions Achieved (Rated A)", y="Interventions") +
                                  theme(legend.position = 'hidden'),ggplot(data = Food_Hygiene_data) +
                                  geom_histogram(binwidth = 1, mapping = aes(x=`Total%ofInterventionsachieved-premisesratedB`, fill=LAType)) +
                                  labs(x="% of Interventions Achieved (Rated B)", y="Interventions") +
                                  theme(legend.position = 'hidden'),ggplot(data = Food_Hygiene_data) +
                                  geom_histogram(binwidth = 1, mapping = aes(x=`Total%ofInterventionsachieved-premisesratedC`, fill=LAType)) +
                                  labs(x="% of Interventions Achieved (Rated C)", y="Interventions") +
                                  theme(legend.position = 'hidden'),ggplot(data = Food_Hygiene_data) +
                                  geom_histogram(binwidth = 1, mapping = aes(x=`Total%ofInterventionsachieved-premisesratedD`, fill=LAType)) +
                                  labs(x="% of Interventions Achieved (Rated D)", y="Interventions") +
                                  theme(legend.position = 'hidden'),ggplot(data = Food_Hygiene_data) +
                                  geom_histogram(binwidth = 1, mapping = aes(x=`Total%ofInterventionsachieved-premisesratedE`, fill=LAType)) +
                                  labs(x="% of Interventions Achieved (Rated E)", y="Interventions") +
                                  theme(legend.position = 'hidden')
                                , nrow=3,top = "Individual Impact Level(A-E) Distribution")
```

The distribution of successful interventions is uniform across all the establishments with varying impact ratings from A-E.

### Relationship Between Total percentage of Successful Responses and FTE Food Safety Employees

RELATIONSHIP BETWEEN PROPORTION OF SUCCESSFUL RESPONSES AND THE NUMBER OF FTE FOOD SAFETY EMPLOYEES IN EACH LOCAL AUTHORITY :
  
  ```{r}
rcorr(as.matrix(select(Food_Hygiene_data, `Total%ofInterventionsachieved(premisesratedA-E)`, `ProfessionalFullTimeEquivalentPosts-occupied *`)))

ggplot(Food_Hygiene_data, aes(x=`Total%ofInterventionsachieved(premisesratedA-E)`, y=`ProfessionalFullTimeEquivalentPosts-occupied *`)) + geom_point() + geom_smooth() + labs(y="Number of FTE Employees", x="Successful Interventions(Responses)", title = "Relationship between Successful Responses and FTE Food Safety Employees")

```

```{r}
Success_FTE <- lm(`Total%ofInterventionsachieved(premisesratedA-E)`~`ProfessionalFullTimeEquivalentPosts-occupied *`, data = Food_Hygiene_data)
summary(Success_FTE)
cbind(coef(Success_FTE), confint(Success_FTE))
```
RELATIONSHIP BETWEEN PROPORTION OF SUCCESSFUL RESPONSES AND THE NUMBER OF FTE FOOD SAFETY EMPLOYEES AS A PROPORTION OF THE NUMBER OF ESTABLISHMENTS IN THE LOCAL AUTHORITY :
  
  ```{r}
Food_Hygiene_data$Totalestablishments<- Food_Hygiene_data$`Totalestablishments(includingnotyetrated&outside)`-Food_Hygiene_data$Establishmentsnotyetratedforintervention-Food_Hygiene_data$Establishmentsoutsidetheprogramme

Food_Hygiene_data$ProportionofEmployees = Food_Hygiene_data$`ProfessionalFullTimeEquivalentPosts-occupied *`/Food_Hygiene_data$Totalestablishments

rcorr(as.matrix(select(Food_Hygiene_data, `Total%ofInterventionsachieved(premisesratedA-E)`, ProportionofEmployees)))

ggplot(Food_Hygiene_data, aes(x=`Total%ofInterventionsachieved(premisesratedA-E)`, y=ProportionofEmployees)) + geom_point() + geom_smooth() + labs(y="Proportion of Employees", x="Successful Interventions")
```

```{r}
Success_Prop.Employee <- lm(`Total%ofInterventionsachieved(premisesratedA-E)`~ProportionofEmployees, data = Food_Hygiene_data)
summary(Success_Prop.Employee)
cbind(coef(Success_Prop.Employee), confint(Success_Prop.Employee))
```


```{r,echo=FALSE, message=FALSE, warning=FALSE}
AllImpact_Plot
```

```{r, echo=FALSE, message=FALSE, warning=FALSE}
p1 <- ggplot(data = Food_Hygiene_data) +
  geom_histogram(binwidth = 1, mapping = aes(x=`Total%ofInterventionsachieved-premisesratedA`, fill=LAType)) +
  labs(x="% of Interventions Achieved (Rated A)", y="Interventions") +
  theme(legend.position = 'hidden')
p2 <- ggplot(data = Food_Hygiene_data) +
  geom_histogram(binwidth = 1, mapping = aes(x=`Total%ofInterventionsachieved-premisesratedB`, fill=LAType)) +
  labs(x="% of Interventions Achieved (Rated B)", y=element_blank()) +
  theme(legend.position = 'hidden')
p3 <- ggplot(data = Food_Hygiene_data) +
  geom_histogram(binwidth = 1, mapping = aes(x=`Total%ofInterventionsachieved-premisesratedC`, fill=LAType)) +
  labs(x="% of Interventions Achieved (Rated C)", y="Interventions") +
  theme(legend.position = 'hidden')
p4 <- ggplot(data = Food_Hygiene_data) +
  geom_histogram(binwidth = 1, mapping = aes(x=`Total%ofInterventionsachieved-premisesratedD`, fill=LAType)) +
  labs(x="% of Interventions Achieved (Rated D)", y=element_blank()) +
  theme(legend.position = 'hidden')
p5 <- ggplot(data = Food_Hygiene_data) +
  geom_histogram(binwidth = 1, mapping = aes(x=`Total%ofInterventionsachieved-premisesratedE`, fill=LAType)) +
  labs(x="% of Interventions Achieved (Rated E)", y="Interventions") +
  theme(legend.position = 'hidden')

grid.arrange(p1,p2,p3,p4,p5,nrow=3,top = "Individual Impact Level(A-E) Distribution", bottom="Figure 2")
```

```{r, echo=FALSE, message=FALSE, warning=FALSE}
ggplot(Food_Hygiene_data, aes(x=`Total%ofInterventionsachieved(premisesratedA-E)`, y=`ProfessionalFullTimeEquivalentPosts-occupied *`)) + geom_point() + geom_smooth() + labs(y="Number of FTE Employees", x="Successful Interventions(Responses)", title = "Relationship between Successful Responses and FTE Food Safety Employees",caption = "Figure 3" )
```


```{r, echo=FALSE, message=FALSE, warning=FALSE}
ggplot(Food_Hygiene_data, aes(x=`Total%ofInterventionsachieved(premisesratedA-E)`, y=ProportionofEmployees)) + geom_point() + geom_smooth() + labs(y="Proportion of Employees", x="Successful Interventions", caption = "Figure 4")
```
---