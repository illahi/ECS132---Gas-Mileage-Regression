colNames = c(" mpg ", " cylinders ", " displacement ", " horsepower ",
" weight ", " acceleration ", " modelyear ", " origin ", " carname ")

carData <- read . table ("auto - mpg . data ", col . names = colNames )

### Delete the attribute " carname " along with its data as it
### serves no purpose in this project
carData = carData [ -9]

summary (lm( carData $ mpg ~ carData $ displacement
+ carData $ horsepower + carData $ weight ))

### Delete all rows with empty data
for (i in 1:398){
if( carData [i ,4] == "?")
carData = carData [-i ,]
}

### Fix " horsepower " attributes into numeric values
hpV = as. vector ( carData $ horsepower )

hpV2 = as. integer ( hpV )
carData $ horsepower = hpV2

corMatrix = cov ( carData )

### Looking at the intercorellation matrix , we can extract
### important attributes that will help us compute mpg .
### Because displacement and weight have a high correlation
### value , we fix the equation so that they modify each
### other .
red2 = lm( mpg ~ displacement * weight + horsepower
, data = carData )
full = lm( mpg ~ displacement + cylinders + weight +
horsepower + acceleration + modelyear , data = carData )

anova (red1 , full )

### A 95% confidence interval is given by
CI = predict (red2 , data . frame ( displacement = 307 ,
horsepower = 130 , weight = 3504) , interval = " confidence ")
CI [2]
### and
CI [3]

### Test whether our equation is within the 95%
### confidence interval
for (i in 1:6){
print ( predict (red2 , data . frame ( displacement =
carData $ displacement [i], horsepower =
carData $ horsepower [i], weight = carData $ weight [i]),
interval = " confidence "))
}

head ( carData )