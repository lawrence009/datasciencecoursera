#test cases
source('rankhospital.R')

print('test #1')
print(rankhospital("TX", "heart failure", 4) == "DETAR HOSPITAL NAVARRO")

print('test #2')
print(rankhospital("MD", "heart attack", "worst") == "HARFORD MEMORIAL HOSPITAL")

print('test #3')
print(is.na(rankhospital("MN", "heart attack", 5000) == NA))

print('test #4')
print(rankhospital("CA", "heart failure", "best") == "CENTINELA HOSPITAL MEDICAL CENTER")
