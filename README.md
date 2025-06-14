# NPP Project: Reader for SQL

A tool SQL Reader is implemented for the NPP Project and works as a simple command-line tool for running SQL queries on CSV files.

## Overview

Key features: 
- The tool accepts simple SQL queries and verifies them before running them on the data; 
- It lets users view CSV files for column type detection.  
- Users can export their results to the designated output destination using this tool.

 Libraries used: 
 - CSV parsing operations are made possible by the `cassava` library.  
 - The SQL query parser is the `megaparsec` library.

## Pipeline

1. **Parse Arguments**  
The application checks whether users have chosen pipeline-auto mode: CSV path, query and output path, or if they have only entered the CSV path to run in interactive terminal mode (where query and output path will be asked later).

2. **Load CSV**  
The program reads and verifies the validity of CSV files using `cassava`.

3. **Preview of the Dataset**  
The application automatically determines the data types of each column while displaying the names of the columns of the data set.

4. **Query Input and Validation**  
When a query is supplied or the query path is requested, the application uses `megaparsec` to parse SQL queries.  
The system verifies that the number of column values, other elements, and type and column names are compatible.

5. **Execution**  
Before showing a preview of the output results, the system runs the query on the dataset.

6. **Results**  
If a path is specified, the application saves the result to that path, otherwise, it requests one.

## Example Usage

Before run, install all dependencies:
```bash
% cabal install --lib \
    base \
    bytestring \
    cassava \
    vector \
    containers \
    megaparsec \
    directory \
    temporary \
    --package-env .
```

Build and run code (from root directory):
```bash
% ghc -i./src Main.hs -outputdir build -o sqlreader

# Interactive mode (you'll need to specify query and output path later)
% ./sqlreader ./data/imdb.csv

# Auto mode
% ./sqlreader ./data/imdb.csv "SELECT Series_Title FROM data WHERE Released_Year > 2015" ./output.csv
```

To run tests, write in terminal (from root directory):
```bash
% ghc -i./test -i./src test/run_tests.hs -outputdir build_tests -o run-tests
% ./run-tests
```
