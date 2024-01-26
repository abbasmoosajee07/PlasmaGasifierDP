# Define the path to the Python script
python_script_path <- "~/Univeristy Coursework/Local Design Project/PlasmaGasifier-DP/myscript.py"

# Use reticulate to run the Python script from R
source_python(python_script_path)

# Example: Call Python function from R
result <- py$calculate_square_root(25)
cat("Square root of 25:", result, "\n")
