# miniJava-interpreter

An interpreter for miniJava language implemented in Haskell.

For information about miniJava language, see
[The MiniJava Project](http://www.cambridge.org/us/features/052182060X/#minijava).

## Functionality Achieved

miniJava-interpreter currently have following functionalities:

- parse miniJava code and generate the abstract syntax tree (AST)
- interpret simple miniJava code

The parsing process parse the code using recursive descent algorithm,
and the interpreting process evaluate the program using call-by-value strategy.

## Installation

Glasgow Haskell Compiler (GHC) is required for installation.

- Mac OS X: run `make` under project directory

- Windows: double click file `make.bat` under project directory

## Usage

After installation, you can find executable files `miniJavac` and `miniJava` 
under directory `bin/mac` or `bin/win`.

### Parsing

Run `miniJavac` with an argument which is the name of the source file.

### Interpreting

Run `miniJava` with an argument which is the name of the source file.

There are test codes under directory `testcase’
