# -*- coding: utf-8 -*-
"""Homework7Tests.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1YsxiEhuU6xAfD2XrGVWrWhIUK6mUyVMe
"""

import unittest

class testWhatData(unittest.TestCase):

  def test_excelToDataframe_OutputType(self): 
    path1 = 'https://docs.google.com/spreadsheets/d/e/2PACX-1vRr9evtYQFBBPcyxE_zy_ixp8QVaDwkSIHddTRJ1j7FRI9kb5zYj3wMYIQVVt0ZYA/pub?output=xlsx'
    dataframe1 = excelToDataframe(path1)
    self.assertTrue(type(dataframe1)== pd.core.frame.DataFrame, 'excelToDataframe needs to return a dataframe')
  
  def test_runRegularization_OutputType(self):
    dataframe1 = excelToDataframe(path1)
    lassoRegArray = runRegularization(dataframe1)
    self.assertTrue(type(lassoRegArray)== np.ndarray, 'run Regularization needs to return a numpy array')

  def test_runRegularization_OutputLength(self):
    dataframe1 = excelToDataframe(path1)
    lassoRegArray = runRegularization(dataframe1)
    self.assertTrue(len(lassoRegArray)== 13, 'run Regularization needs to return a numpy array of 13 items') 

  def test_makeList_OutputType(self):
    dataframe1 = excelToDataframe(path1)
    lassoRegArray = runRegularization(dataframe1)
    lassoRegList = makeList(lassoRegArray, dataframe1)
    self.assertTrue(type(lassoRegList)== list, 'makeList needs to return a list') 

  def test_makeList_OutputLength(self):
    dataframe1 = excelToDataframe(path1)
    lassoRegArray = runRegularization(dataframe1)
    lassoRegList = makeList(lassoRegArray, dataframe1)
    self.assertTrue(len(lassoRegList)== 12, 'makeList needs to return a list of 13') 
  
  def test_isItSignificant_OutputType(self):
    dataframe1 = excelToDataframe(path1)
    lassoRegArray = runRegularization(dataframe1)
    lassoRegList = makeList(lassoRegArray, dataframe1)
    SignificantList = isItSignificant(lassoRegList, dataframe1)
    self.assertTrue(type(SignificantList)== list, 'isItSignificant needs to return a list')


path1 = 'https://docs.google.com/spreadsheets/d/e/2PACX-1vRr9evtYQFBBPcyxE_zy_ixp8QVaDwkSIHddTRJ1j7FRI9kb5zYj3wMYIQVVt0ZYA/pub?output=xlsx'
unittest.main(argv=[''], verbosity=2, exit = False)