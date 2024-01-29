using Pkg
# Pkg.add("JuMP")
Pkg.add("GLPK")  # or Pkg.add("Cbc") for another solver

using JuMP
using GLPK  # or using Cbc for another solver

# Create a JuMP model
model = Model(GLPK.Optimizer)  # or use Cbc.Optimizer for another solver

# Decision variables
@variable(model, p[1:10] >= 0)

# Objective function: Maximize the sum of probabilities for Z >= 5
@objective(model, Max, sum(p[5:10]))

# Constraints
@constraint(model, sum(p) == 1)  # Normalization constraint
@constraint(model, sum(i * p[i] for i in 1:10) == 4)  # Average value constraint

# Solve the linear program
optimize!(model)

# Check if the optimization was successful
if termination_status(model) == MOI.OPTIMAL
    # Retrieve the optimal values of p
    optimal_probabilities = value.(p)
    println("Optimal Probability Distribution: ", optimal_probabilities)
else
    println("No optimal solution found.")
end

