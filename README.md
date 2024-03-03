# Recursive Descent Parser Project - Hallee Pham
COMP-SCI 441 Programming Languages (Spring 2024)

## Deliverables
* Racket Source File -> "ParserFinalCopy.rkt"
* References and Outside Sources -> "PARSER PROJECT RESOURCES.docx"
* Use of LLMs -> "LLM PROMPTS.docx"

## Notes
- The program only produces a list of tokens from the input file and provides an uncompleted/incorrect final verdict
- All the functions for each nonterminal are written, however, the logic of the failure branches is incorrect
- The logic of the rest of the function should be correct
- All of the code was written by myself (no help from LLMs) except for a portion of the tokenizing functions (taken and then modified from the example code on Canvas)
- I wasn't able to figure out how to properly chain the results of each function to one another correctly before the due date
- I spent well over 45+ hours on this project (learning Racket, understanding recursive descent parsers, programming), but had to stop this project to prioritize other upcoming projects and exams

## Summary
- The program properly:
  - Scans in all tokens (including parentheses)
  - Detects a scanning error (unknown symbol)
  - Uses a functional programming style with no side effects
  - Uses monadic types (either)
- The program CANNOT:
  - Produce a correct final verdict
  - Determine which line has a syntax error
  - Fully parse a file with multiple lines of code
