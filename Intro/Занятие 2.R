#########################################
### ������� ����������                ###
### ������ ������ �� �������� �����   ###
### ������� ������                    ###
### ������ � data.frame (�����������) ###
#########################################

## �������� � ��������� ���� ---------

# getwd() ���������� ������� ���� �� ��������� ����� � ������� �������  
# setwd("c:/�") � �������� ���������� �� ���������

getwd()

setwd("H:/R") # ���������� ������ ����!

# ���������, ��� � ��������� ����� ���������� ����������� �����

## ������ ������ �� ����� txt -------------

data1=read.table("kids_ru.txt",header=TRUE, sep='\t')
data1

## ����������, ������ � �������� ------------

data1[,2]

data1$����

data1[data1$���=="f",]

data1[data1$���=="m",]

data1[order(data1$���, data1$����),]

dim(data1[data1$���=="f",])
dim(data1[data1$���=="m",])

## ������� ������ � ��������� ����
data_f=data1[data1$���=="f",]
? write.table # ���� ����� ������ ������� ����������
write.table(data_f,"kids_f.txt", sep="\t")

data_m=data1[data1$���=="m",]
write.table(data_m,"kids_all.txt", sep="\t", quote=FALSE)
write.table(data_f,"kids_all.txt", sep="\t", append=TRUE, quote=FALSE)

numb_kid=nrow(data1)+1
new_kid=c(numb_kid,"f",134,27,30,117)
write(new_kid,"kids_all.txt", sep="\t",append=TRUE, ncolumns=length(new_kid))

## ��������� ������� ------------

installed.packages() # �������� ���� ������������� �������
# install.packages() # ���������� ������
# library() # �������� ������ 

## ������ ������ �� ����� xls ------------
# ������������� ������ � ������� *.xls 
#����� � ������� ����������� ������� read.xls() 
# ���������� ReadWriteXls � ������������ ����������� 
# ���  � ������� ����� ������� ������� read_excel() ������ readxl.

# ��� ������ ��������� ������ readxl,
# ���� ����� ��� �� ����������
install.packages("readxl") # ���������� ������
library(readxl) # �������� ������ 

data2=read_excel("kids_ru.xlsx")
data2

install.packages("xlsx")
library(xlsx)
write.xlsx(data2, "mydata.xlsx")

# ���� �������� �������� � ���������� ������� ��� ������ xls  ������, 
# ����� �� ��������� � csv � ������������� ��������� read.csv(), 
# ���������� read.table(), ������ ������ ����������� (, ��� ;)

data3=read.csv("kids_ru.csv",header=TRUE, sep=";")
data3
