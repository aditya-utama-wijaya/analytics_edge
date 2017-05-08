# Load the dataset into a data frame called Poll
Poll = read.csv('4a-AnonymityPoll.csv')

# How many people participated in the poll?
nrow(Poll)

# How many interviewees responded that they use a smartphone?
# How many interviewees responded that they don't use a smartphone?
# How many interviewees did not respond to he question?
table(Poll$Smartphone)
summary(Poll$Smartphone)

# Which are states in the Midwest census region?
MidwestInterviewees = subset(Poll, Region == 'Midwest')
sort(table(MidwestInterviewees$State))

# How many interviewees reported not having used the internet and not having used a smartphone?
# How many interviewees reported having used the internet and having used a smartphone?
# How many interviewees reported having used the internet but not having used a smartphone?
# How many interviewees reported having used a smartphone but not having used the internet?
table(Poll$Internet.Use, Poll$Smartphone)

# How many interviewees have a missing value for their internet use?
# How many interviewees have a missing value for their smartphone use?
summary(Poll)

# Use the subset function to obtain a data frame, which is limited to interviewees who reported internet use or who reported smartphone use!
limited = subset(Poll, Internet.Use == 1 | Smartphone == 1)
nrow(limited)

# Which variables have missing values in the limited data frame?
summary(limited)

# What is the average number of poeces of personal information on the internet?
mean(limited$Info.On.Internet)

# How many interviewees reported a value of 0 for Info.On.Internet?
# How many interviewees reported the maximum value of 11 for Info.On.Internet?
table(limited$Info.On.Internet)

# What proportion of interviewees who answered the Worry.About.Info question worry about how much information is available about them on the internet?
mean(limited$Worry.About.Info, na.RM = TRUE)

# What proportion of interviewees who answered the Anonymity.Possible question think it is possible to be completely anonymous on the internet?
mean(limited$Anonymity.Possible, na.rm = TRUE)

# What proportion of interviewees who answered the Tried.Masking.Identity question have tried masking their identity on the internet?
mean(limited$Tried.Masking.Identity, na.rm = TRUE)

# What proportion of interviewees who answered the Privacy.Laws.Effective question find US privacy laws effective?
mean(limited$Privacy.Laws.Effective, na.rm = TRUE)

# What is the best represented age group in the population?
hist(limited$Age)

# What is the largest number of interviewees that have exactly the same value in their Age variable and the same value in their Info.On.Internet variable?
max(table(limited$Age, limited$Info.On.Internet))

# jitter adds or subtracts a small amount of random noise to the values passed to it, and 2 runs will yield different results
jitter(c(1, 2, 3))
jitter(c(1, 2, 3))

plot(jitter(limited$Age), jitter(limited$Info.On.Internet))

# What is the average Info.On.Internet value for smartphone users?
# What is the average Info.On.Internet value for non-smartphone users?
tapply(limited$Info.On.Internet, limited$Smartphone, mean, na.rm = TRUE)

# What proportion of smartphone users who answered the Tried.Masking.Identity question have tried masking their identity when using the internet?
# What proportion of non-smartphone users who answered the Tried.Masking.Identity question have tried masking their identity when using the internet?
tapply(limited$Tried.Masking.Identity, limited$Smartphone, summary, na.rm = TRUE)

