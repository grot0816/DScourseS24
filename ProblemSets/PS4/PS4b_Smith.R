library(sparklyr)
install.packages("sparklyr")
library(sparklyr)

#getting error on spark_connect
sc=spark_connect(master = "local")
spark_install_find(version=3.5,hadoop_version = 3.5,installed_only=TRUE)
df1=tibble("iris")

df=copy_to(sc, df1)
class(df1)
class(df) 

df %>%
  select(Sepal_Length,Species)%>%
  head() %>%
  print()

df %>%
  filter(Sepal_Length>5.5)%>%
  head()%>%
  print()

df %>%
  select(Sepal_Length,Species)%>%
  filter(Sepal_Length>5.5)%>%
  head()%>%
  print()

df2=df%>%
  group_by(Species)%>%
  summarize(mean=mean(Sepal_Length),count=n())%>%
  head()%>%
  print()

df2%>%
  arrange(Species)%>%
  head()%>%
  print()
  
  
