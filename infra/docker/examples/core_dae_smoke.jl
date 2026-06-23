using ModelingToolkit
using SciMLBase
using Sundials

function residual!(out, du, u, p, t)
    out[1] = du[1] - u[2]
    out[2] = u[2] + p
    nothing
end

function main()
    @independent_variables t
    @variables x(t) y(t)
    @parameters p
    D = Differential(t)
    eqs = [D(x) ~ y, y ~ -p]
    @named sys = ODESystem(eqs, t, [x, y], [p])

    @assert nameof(sys) == :sys
    @assert length(unknowns(sys)) == 2

    differential = [true, false]

    prob = DAEProblem(
        residual!,
        [-2.0, 0.0],
        [1.0, -2.0],
        (0.0, 0.1),
        2.0;
        differential_vars = differential,
    )
    sol = solve(prob, IDA())

    @assert isapprox(sol.u[end][1], 0.8; atol = 1e-3)
    @assert isapprox(sol.u[end][2], -2.0; atol = 1e-6)

    println("core julia dae smoke passed")
end

main()
