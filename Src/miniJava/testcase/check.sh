rm -f out.txt
touch out.txt

../bin/mac/miniJava A.java >> out.txt
../bin/mac/miniJava Factorial.java >> out.txt
../bin/mac/miniJava Fibonacci.java >> out.txt
../bin/mac/miniJava GCD.java >> out.txt
../bin/mac/miniJava Prime.java >> out.txt
../bin/mac/miniJava Power.java >> out.txt
../bin/mac/miniJava Sum.java >> out.txt
../bin/mac/miniJava Short.java >> out.txt
../bin/mac/miniJava Deep.java >> out.txt

rm -f ans.txt
touch ans.txt

javac A.java
javac Factorial.java
javac Fibonacci.java
javac GCD.java
javac Prime.java
javac Power.java
javac Sum.java
javac Short.java
javac Deep.java

java A >> ans.txt
java Factorial >> ans.txt
java Fibonacci >> ans.txt
java GCD >> ans.txt
java Prime >> ans.txt
java Power >> ans.txt
java Sum >> ans.txt
java Short >> ans.txt
java Deep >> ans.txt

rm *.class

diff ans.txt out.txt
