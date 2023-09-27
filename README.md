- 👋 Hi, I’m @Prasoon1105
- 👀 I’m interested in ...
- 🌱 I’m currently learning ...
- 💞️ I’m looking to collaborate on ...
- 📫 How to reach me ...

<!---
Prasoon1105/Prasoon1105 is a ✨ special ✨ repository because its `README.md` (this file) appears on your GitHub profile.
You can click the Preview link to take a look at your changes.
--->
if (!require("pacman")) install.packages("pacman")
## packages for this script
if (!require("caret")) install.packages('caret', dependencies = TRUE) #just in case caret has not been installed previously to this machine
## Install pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

## load packages (including pacman) with pacman
pacman::p_load(pacman, tidyverse, gmodels,ROCR, rpart, rpart.plot,caret)

pacman::p_load(pacman, psych,tidyverse, gmodels,caret,ROCR)

Wholesale_customers <- read.csv("Wholesale customers data.csv")
Wholesale_factors <- c("Channel", "Region")
Wholesale_customers[Wholesale_factors] <- lapply(Wholesale_customers[Wholesale_factors], as.factor)
str(Wholesale_customers)


numeric_vars <- Wholesale_customers %>%
  select_if(is.numeric) 
describe(numeric_vars[,3:5])

head(Wholesale_customers)
view(Wholesale_customers)
plot_list <- list()
materials <- c("Fresh","Milk", "Grocery","Frozen", "Detergents_Paper", "Delicassen")
Wholesale_customers[materials] <- lapply(Wholesale_customers[materials], as.factor)
for (var_name in materials) {
  plot <- ggplot(data = Wholesale_customers, aes(x = .data[[var_name]], fill = .data[[var_name]])) +
    geom_density() +
    labs(title = var_name) +
    theme_minimal()
  plot_list[[var_name]] <- plot
}

gridExtra::grid.arrange(grobs = plot_list, ncol = 3)


table(Wholesale_customers$Region,Wholesale_customers$Channel)
prop.table(table(Wholesale_customers$Region,Wholesale_customers$Channel))*100


chisq.test(Wholesale_customers$Region,Wholesale_customers$Channel) 

RegionTable <- xtabs(~Region + Channel,  data=Wholesale_customers)
print(RegionTable)
summary(RegionTable)



CrossTable(Wholesale_customers$Region,Wholesale_customers$Channel, digits=1, max.width = 5, expected=TRUE, prop.r=FALSE, prop.c=FALSE,
           prop.t=TRUE, prop.chisq=FALSE, chisq = TRUE, fisher=FALSE, mcnemar=FALSE,
           resid=FALSE, sresid=FALSE, asresid=FALSE,
           missing.include=FALSE,
           format=c("SPSS"), dnn = NULL)



plot1 <- ggplot(data = Wholesale_customers, aes(x = Milk, fill = factor(Region))) +
  geom_density(alpha = 0.3) +
  labs(title = "Density Plot of Milk by Region") +
  xlab("Region") +
  ylab("Density") +
  theme_minimal()
print(plot1)


plot2 <- ggplot(data = Wholesale_customers, aes(x = Frozen, fill = factor(Channel))) +
  geom_density(alpha = 0.3) +
  labs(title = "Density Plot of Frozen by Channel") +
  xlab("Frozen") +
  ylab("Density") +
  theme_minimal()
print(plot2)




set.seed(111)
train_ind <- Wholesale_customers$Channel %>%
  createDataPartition(p = 0.70, list = FALSE) #this is splitting the iris data in 70% estimation and 30% testing/holdout sample
Wholesale_train <- Wholesale_customers[train_ind, ] # new data frame of 1000 observations
Wholesale_test <- Wholesale_customers[-train_ind, ]


summary(Wholesale_train$Channel) #note the % of survived is 38.3%
summary(Wholesale_test$Channel)  #not % saved is 38.2%

rm(train_ind)

model1<- glm(Channel ~  Region + Milk   + Grocery,                    data=Wholesale_train, family="binomial")
summary(model1)

model2<- glm(Channel ~ Region + Milk   + Grocery + Frozen + Detergents_Paper, data=Wholesale_train, family="binomial")
summary(model2)


Wholesale_model <- rpart(formula = Channel ~ Region + Milk + Grocery + Frozen + Detergents_Paper,
                        data = Wholesale_train, 
                        method =  "class")
rpart.plot(x = Wholesale_model, yesno = 2, type = 0, extra = 0)


Wholesale_prediction <- predict(object = Wholesale_model,  
                               newdata = Wholesale_test,   
                               type = "class",
) 

confusionMatrix(data = Wholesale_prediction,       
                reference = Wholesale_test$Channel) 
