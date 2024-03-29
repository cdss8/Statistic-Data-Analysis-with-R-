##################################
# ������� ������������         ###
# �������                      ###
# ���� � ��������� ������ � R  ###   
# ������� � �������            ###
# # ������ � ������� ���� list ###
# ������ � ������� ���� factor ###
# ������ � ������� ���� array  ###
# ������ � ������� data.frame  ###
# ����������� �����            ###
##################################

## �������� --------

x=5
x
y=2
x+y
z=x+y
xy=x*y
x; y; z; xy
x6 <- x+6
y-7 = y7    # ��������� � ��� ������� �� ��������
y-7 ->  y7  # ������� �������� � ��� �������

age=c(65,63,38,40,15) # ������� ����� ������ � ���� �������
age

a=2; b=5; c=-1.5
coeffs=c(a,b,c)
coeffs

## ������� ������������ --------------

ls() # �������� ���� ��������� ��������
ls(pat = "^x") # �������, ��� ��� ���������� � x 
ls(pat = "y") # �������, ��� ��� �������� y 

rm(x) # ������� x
ls()
rm(x6, y7) # ������� ��������� ��������
ls()
rm(list = ls(pat = "x")); ls() # ������� ��� �������, ��� ��� �������� x 
rm(list = ls()) # ������� ��� ������� = �������� ������� ������������

# Ctrl + L - �������� �������, �� ��� �� ������� ������� ������������.


## ������� ------------------------

# ����� �����
help.search("remove objects")
help.search("linear model")
# ���������� � ���������� �������, ������, �������
?rm   
help(rm) # ������ �����������

## ���� � ��������� ������ � R --------------

x=double(3) # ��� �������� ���� double
x
y=integer(1); y=3
z=double(1) ; z=4.7
is.integer(z)  # �������� ����
is.double(z)
z=as.integer(z)
z
is.integer(z)
is.double(z)

mode(x); mode(y); mode(z)

# ���������� ����������
# TRUE = T; FALSE = F
C = c(TRUE, F, T, FALSE); C; mode(C); length(C); class(C)

# ��������� ����������

A = "�� ������� ����� R"
A
mode(A)
length(A)

# ��������� �������������������
i=1:100   # ���������� 100 ����� �������� �� 1 �� 100
x=1+0.01*i   # ���������� �������� �� 1 �� 3 � ����� 0.01
y=seq(1,10,by=0.1)  # ������ � ����� 0.1 �� 1 �� 10
y=seq(1,10,0.1)  # ��� ��������� �by� ����� ���� �������

y=seq(-3,3,length.out=10)   	# ������� ������������������ 10 �������� �� -3 �� 3.
s=sample(1:6, 10, TRUE)   	# ���������� ��������� ��������� �������   �������� �� 1 �� 6 (������ 10-���������� ������������� ��������� �����) �������� TRUE ��������� ��������� ���������� ���������                                                           ���������� ���������.
s

## ������� � ������� ----------------------
n = vector("numeric", 5)
c = vector("character", 4)
l = vector("logical", 3)
n;c;l

n = numeric(5)
c = character(5)
l = logical(5)
n;c;l

i
i[10]
i[50:60]
i[i>80]
head(i)  # ��������� �������� ������� (�� ��������� 6)
tail(i)  # ������ �������� ������� (�� ��������� 6)
head(i,10)
tail(i,3)

i10=i[i%%10==0]
i10

cnd1 = c(T, F)
i1=i[cnd1]
i1

cnd2 = c(F, T)
i2=i[cnd2]
i2

## ���������, ��� ����� ���� ��������� ��������

cnd3 = c(F,F,T)
i3=i[cnd3]
i3

# ������� 
diag(1,4) 			# ��������� ������� ������� 4�4
d=diag (c(1,2,3,4,5),5);d  

m=matrix(c(1,2,3,4,5,6),2,3);m  

m=matrix(c(1,2,3,4,5,6),byrow=TRUE, 2,3);m 

m=matrix(c(1:6),byrow=TRUE, 2,3)
m

m[,2]=0;m
m[1,]=11:13;m

n_col = paste("V", 1:3, sep = "")
n_col
n_row = paste("I", 1:2, sep = "")
n_row
colnames(m) = n_col; # ����� ��������
rownames(m) = n_row; # ����� �����
m
dimnames(m) = list(n_row, n_col) # ���������� �� ����� ����������
m

M = matrix(,5,4)
M
data.entry(M) # ��� ������� �� "var" ����� �������� ��� ��������������� ������� � numeric �� character
M

x=matrix(c(1:6),2,3)
y=matrix(c(7:12),2,3)
cbind(x,y) # ���������� ������ �� ��������
rbind(x,y) # ���������� ������ �� �������

## ������ � ������� ���� list ------------------
# ������������� ������������ �������� ������� ����
x=c(1:5)
y=c("a","b","c","d")
z=matrix(c(1:6),2,3)
list_1=list(x,y,z)
list_1

list_1[[1]]
list_1[[3]][,2]

names(list_1) <- c("num","char","matr")
list_1

list_1$char # � ���� ������ �������� ��������� �� �����

## ������ � ������� ���� factor ----------------------

# ������ ���� factor ������������� ��� ������ � ��������������� �����������

grade=sample(2:5, 25, replace=TRUE)
grade
grade_f= factor(grade, 
                labels = c("unsatisfactory","satisfactory", "good","excellent"))
grade_f
class(grade_f)

gender=c("male","male","female","male","female","female","female",
         "male","female","male")
gender
is.factor(gender)
class(gender)
gender_f=factor(gender)
is.factor(gender_f)
gender_f

# ���� ������ �����������
order_grade=ordered(grade_f,levels=c("unsatisfactory", "satisfactory", "good", "excellent"))

order_grade

## ������ � ������� ���� array -----------

# ������ ���� array ������������ ����� ����������� ������.
# ��������, ������� 24 �������� � ���������� �� ��� ���������� ������ ����������� 2�4�3.

a=array(c(1:24),dim=c(2,4,3)); a

# ����� ������ ����� ������ ���������� �������
dimnames(a)<-list(c("region1","region2"),
                  c("winter","spring","summer","autumn"),
                  paste("year", 2015:2017, sep = ""))

a
a[,3,]
a[1,,2]

## ������ � ���� data.frame -----------------------

numbers = c(1:10)
names = c(
"���� ������",
"������� �������",
"������� ��������",
"����� �������",
"������� ������",
"����� ��������",
"����� �������",
"������ ��������",
"���� �������",
"������� �����")
names
length(names)

gender = c ('m','f','m','f','f','f','m','m','f','m')
grades = sample(c(3:5),10, rep=TRUE)
grades

data = data.frame (numbers, names, gender, grades)
data

grades2 = matrix (sample (c(3:5),30,rep=TRUE), 10,3)
grades2

data2= data.frame (numbers, names, gender, grades2)
data2

data2= data.frame (numbers, names, gender, grades=grades2)
data2
colnames(data2)=c("N","Name","Gender","Maths","Physics","History")
data2

data2[,2]
data2[2,]
data2[3,5]

data2$Maths
mean(data2$Maths)
maths=data2$Maths
mean(maths)

mat2=as.matrix(data2)
mat2
mat2$Maths
mat2[,4]
maths_m=mat2[,4]
mean(maths_m)

# ���������, ������ �������� ����� ������

mat3=matrix(sample(1:24,24, FALSE),4,6);mat3
as.data.frame(mat3) #  �������� �������� �� �������
df=as.data.frame(m)
mode(mat3)
class(mat3)
mode(df)
class(df)

## ����������� ����� ------------------------

## ���������� -> ��������
logic = c(T, F, F, T, F, T, T) 
as.numeric(logic)

## ��������� -> ��������
char_num = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10") 
as.numeric(char_num)
char_let = c("A", "B", "C", "D", "F")
char_let
char_let = LETTERS[1:5]
char_let
as.numeric(char_let)

## �������� -> ����������
num = 0:5
as.logical(num)

## ��������� -> ����������
char = c("FALSE", "F", "T", "false", "f", "TRUE", "A", "%","5")
as.logical(char) 

## �������� -> ���������
as.character(num)

## ���������� -> ���������
logic = c(T, F)
as.character(logic)


