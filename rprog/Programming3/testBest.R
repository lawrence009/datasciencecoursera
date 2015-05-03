#test cases
source('best.R')

print('test #1')
print(best("TX", "heart attack") == "CYPRESS FAIRBANKS MEDICAL CENTER")

print('test #2')
print(best("TX", "heart failure") == "FORT DUNCAN MEDICAL CENTER")

print('test #3')
print(best("MD", "heart attack") == "JOHNS HOPKINS HOSPITAL, THE")

print('test #4')
print(best("MD", "pneumonia") == "GREATER BALTIMORE MEDICAL CENTER")
