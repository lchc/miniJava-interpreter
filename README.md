# miniJava-interpreter

An interpreter for miniJava language implemented in Haskell.

For information about miniJava language, see
[The MiniJava Project](http://www.cambridge.org/us/features/052182060X/#minijava).

## Functionality Achieved

miniJava-interpreter currently have following functionalities:

- parse miniJava code and generate the abstract syntax tree (AST)
- report syntactic errors in miniJava code
- interpret simple miniJava code

The parsing process parse the code using recursive descent algorithm,
and the interpreting process evaluate the program using call-by-value strategy.

## Installation

Glasgow Haskell Compiler (GHC) is required for installation.

- Mac OS X: run `make` under project directory

- Linux: run `make linux` under project directory

- Windows: double click file `make.bat` under project directory

## Usage

After installation, you can find executable files `miniJavac` and `miniJava` 
under directory `bin/mac`, `bin/linux` or `bin/win`.

### Parsing

Run `miniJavac` with an argument which is the name of the source file.

### Interpreting

Run `miniJava` with an argument which is the name of the source file.

It can currently interpret miniJava code without object-oriented functionality.
Formally, it can interpret code which has the properties below:

- it has a single class declaration
- it has a single main method as well as some other static methods/fields in the class body
- types of its variables, expressions and methods' parameters must be int or boolean
- types of its methods' return value must be int, boolean or void
- it does not contain any semantic errors

Aside from these, you can use other functionalities (if, while, recursion, etc.) as you want.

## ToDo

- syntactic errors correcting
- semantic errors reporting
- object-oriented interpreting
