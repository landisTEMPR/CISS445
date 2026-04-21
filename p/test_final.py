# Integer division
print(10 // 3)
print(7 // 2)

# Variables persist after while
x = 0
while x < 3:
    y = 42
    x = x + 1
print(y)

# Variables persist after if
if True:
    z = 99
print(z)

# Nested if-else
a = 5
b = 0
if a > 0:
    if b == 0:
        print(a)
    else:
        print(b)
else:
    print(0)

# Mixed mode
print(1 + True + 2.0)
