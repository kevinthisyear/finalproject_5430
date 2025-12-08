# Code Converter for R and Python

This project provides a simple two-way translator that converts R code to Python and Python code to R, focusing on introductory concepts from STAT 1601 and STAT 1602.

## Features
- Basic syntax translation (assignments, vectors/lists, operators)
- Functions, loops, and conditionals
- Tidyverse pipelines to pandas
- ggplot to matplotlib

## Files
- RtoPython.R — R to Python translator
- pythonTOr.py — Python to R translator

## Usage
Run the translator for the language of your input file.

R:
source("RtoPython.R")

Python:
python pythonTOr.py

## Scope
Designed for beginner-level workflows; not a full compiler.
