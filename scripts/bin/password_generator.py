import random

lower = "abcdefghijklmnopqrstuvwxyz"
upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
numbers = "1234567890"
symbols = "[]{}()*;/.,"

char = lower + upper + numbers + symbols

length = 16
password = "".join(random.sample(char, length))
print(password)
