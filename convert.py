import pandas as pd
import sqlite3

db = sqlite3.connect('loan_book.sqlite3')
dfs = pd.read_excel('22500001-23000000_loan_book.xlsx', sheet_name=None)
for table, df in dfs.items():
    df.to_sql("sheet2", db)
