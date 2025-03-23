This package contains Interpreter.rkt, as well as a number of accessory files. Interpreter.rkt
defines all interpreter functionality, and has the interpret function that can be called
to interpret a file.
	(interpret "filename")

The accessory files include:
 - Test.rkt: runs tests from Project 1
 - Test2.rkt: runs tests from Project 2
 - /tests: directory containing all tests from Project 1
 - /tests2: directory containing all tests from Project 2
 - simpleParser.rkt: parser for code text
 - lex.rkt: accessory to simpleParser.rkt

The naming convention within Interpreter.rkt uses prefixes to denotes which functions return 
state modifications, and which return values. 

The prefix:
 - S_ Indicates a function that returns state
 - V_ Indicates a function that returns a value
 - SV_ Indicates a function that returns both a state and a value