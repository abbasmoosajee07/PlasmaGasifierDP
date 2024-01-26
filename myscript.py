# myscript.py
import numpy as np
import math
import matplotlib.pyplot as plt


def calculate_square_root(number):
    return math.sqrt(number)

result = calculate_square_root(25)
print(f"Square root of 25: {result}")


# Generate data
x = np.linspace(0, 2*np.pi, 100)
y = np.sin(x)

# Create a plot
plt.plot(x, y, label='Sine Function')
plt.title('Sine Function Plot')
plt.xlabel('x')
plt.ylabel('sin(x)')
plt.legend()

from sympy import symbols, Eq, solve

# Create symbols
x, y = symbols('x y')

# Use symbols in expressions
expression1 = x + y
expression2 = x**2 - y

# Display expressions
print("Expression 1:", expression1)
print("Expression 2:", expression2)

# Solve equations
equation1 = Eq(expression1, 0)
equation2 = Eq(expression2, 0)

# Solve for x and y
def get_solutions():
    equations = {'x': 2, 'y': 3}
    return equations

solutions = get_solutions()
print(solutions)

# Display solutions
print("Solutions:")
for variable, value in solutions.items():
    print(f"{variable}: {value}")

# Save the plot to a file (optional)
# plt.savefig('sine_plot.png')

# Show the plot
plt.show()
