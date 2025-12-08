\documentclass{article}
\usepackage[margin=1in]{geometry}

\begin{document}

\title{Code Converter for R and Python}
\author{Sophia, Jaelyn, Kevin, Ananya, and Bailey}
\date{}
\maketitle

\section*{Overview}
This project provides a two-way code translator that converts R code into Python and Python code back into R. The tool focuses on introductory programming topics commonly taught in STAT 1601 and STAT 1602, where students frequently switch between the two languages.

\section*{Functionality}
The translator supports:
\begin{itemize}
    \item Assignments, vectors, lists, operators, indexing, and input/output
    \item Functions, loops, and conditional statements
    \item Tidyverse pipelines translated into equivalent pandas workflows
    \item Basic ggplot visualizations translated into matplotlib
    \item Pandas data manipulation translated back into tidyverse-style R
\end{itemize}

\section*{Files}
\begin{itemize}
    \item \texttt{RtoPython.R}: Translator written in R for converting R to Python
    \item \texttt{pythonTOr.py}: Translator written in Python for converting Python to R
\end{itemize}

\section*{Usage}
To use the translators:
\begin{itemize}
    \item In R: \texttt{source("RtoPython.R")}
    \item In Python: \texttt{python pythonTOr.py}
\end{itemize}
Each script reads an input file provided by the user and outputs the translated code.

\section*{Scope and Limitations}
The tool is designed for introductory-level syntax and workflows. It does not fully support advanced object-oriented features, deeply nested structures, or complex pipelines beyond common patterns.

\section*{Purpose}
The goal of this project is to reduce the syntactic barriers between R and Python so students can focus on the statistical concepts rather than language-specific details.

\end{document}
