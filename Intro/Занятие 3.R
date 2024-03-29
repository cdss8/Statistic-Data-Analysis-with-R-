################################
## ������ � ���������������   ##
## ������� � ��������         ##
################################

## ������ � ��������������� (����� ����� ���������) -----

# ���������� ������� 100 �������� �� ���������� ����������� ������������ 
# � ������� ������� � ��������� ����������
x=rnorm(100,0,1) 
length(x)
sum(x)
sum(x)/length(x)
mean(x)

## ������� -----------

hist(x) # �����������  

palette() # �������� ������� ������
colors()  # ������ ������� ������

hist(x,col="blue") 
hist(x,density=6,angle=45, col="blue") 
hist(x,density=c(1:10),angle=45, col="blue") 

x11()   # ����� ����������� ����
hist(x,density=c(1:10),angle=45, col=colors()[610:620]) 

x11()
hist(x,freq=FALSE,density=6,angle=45, col=colors()[610:622]) # ����������� ������������� ������

# ��������� �������� ������� hist
hist(x,plot=FALSE)$breaks # ������� ���������� ���������
hist(x,plot=FALSE)$counts # ���������� �������

x11()
qqnorm(x, col='red')  # ����������� ������

lines(c(-3,3),c(-3,3))

## ������ � ��������� �������
getwd()
#setwd("D:/Stat2017/R/")

data1=read.table("kids_ru.txt",header=TRUE, sep='\t')
data1
table(data1$���)

x11()

plot(data1$���~data1$����)
plot(���~����, data=data1) # �� �� �����, ���������� ������ ������� �� ����

plot(���~����, data=data1, 
     xlab="����", ylab="���", main="����������� ����-���")

plot(data1$���~data1$����, data=data1, 
     xlab="����", ylab="���", main="����������� ����-���",
     col="blue")

plot(data1$���~data1$����, data=data1, 
     xlab="����", ylab="���", main="����������� ����-���",
     col="blue", pch= 6 )

plot(data1$���~data1$����, data=data1, 
     xlab="����", ylab="���", main="����������� ����-���",
     col="blue", type="o")


data_s=data1[order(data1$����),]

plot(data_s$���~data_s$����, data=data_s, 
     xlab="����", ylab="���", main="����������� ����-���",
     col="blue", type="o")

cor(data1$���,data1$����) # ���������� ������� (cor ������ ������ �������� ����-��)
cor.test(data1$���,data1$����, method="spearman") # ���������� ��������

boxplot(���~���, data=data1) # ��������� �������, ������� � �����
boxplot(����~���, data=data1)

## ���������� ������������ ������ ------

x11()
layout(matrix(1:6, 3, 2))
layout.show(6)

mat = matrix(c(1,1,2,3,4,4), 3, 2)
layout(mat)
layout.show(4)

mat_b=matrix(c(1:2),1,2)
mat_b
layout(mat_b)
layout.show(2)

#l=layout(mat)
#layout.show(l)

boxplot(���~���, data=data1) # ������ (�������������) � ������ ����
boxplot(����~���, data=data1) # ������ �� ������ ����

