Parsing took 58 milliseconds.
Success!
StoredDefinition {
    class_list: {
        "Quadrotor": ClassDefinition {
            name: "Quadrotor",
            encapsulated: false,
            extends: [
                Extend {
                    comp: "RigidBody6DOF",
                },
            ],
            components: {
                "l": Component {
                    name: "l",
                    type_name: "Real",
                    variability: Parameter(
                        "parameter",
                    ),
                },
                "m1": Component {
                    name: "m1",
                    type_name: "Motor",
                },
                "m2": Component {
                    name: "m2",
                    type_name: "Motor",
                },
                "m3": Component {
                    name: "m3",
                    type_name: "Motor",
                },
                "m4": Component {
                    name: "m4",
                    type_name: "Motor",
                },
                "u1": Component {
                    name: "u1",
                    type_name: "Real",
                    causality: Input(
                        "input",
                    ),
                },
                "u2": Component {
                    name: "u2",
                    type_name: "Real",
                    causality: Input(
                        "input",
                    ),
                },
                "u3": Component {
                    name: "u3",
                    type_name: "Real",
                    causality: Input(
                        "input",
                    ),
                },
                "u4": Component {
                    name: "u4",
                    type_name: "Real",
                    causality: Input(
                        "input",
                    ),
                },
            },
            equations: [
                Simple {
                    lhs: "Fx",
                    rhs: UnsignedInteger("0"),
                },
                Simple {
                    lhs: "Fy",
                    rhs: UnsignedInteger("0"),
                },
                If {
                    cond_blocks: [
                        EquationBlock {
                            cond: Lt("<") {
                                lhs: "h",
                                rhs: UnsignedInteger("0"),
                            },
                            eqs: [
                                Simple {
                                    lhs: "Fz",
                                    rhs: Add("+") {
                                        lhs: Add("+") {
                                            lhs: Sub("-") {
                                                lhs: Add("+") {
                                                    lhs: Add("+") {
                                                        lhs: Add("+") {
                                                            lhs: "m1.thrust",
                                                            rhs: "m2.thrust",
                                                        },
                                                        rhs: "m3.thrust",
                                                    },
                                                    rhs: "m4.thrust",
                                                },
                                                rhs: Mul("*") {
                                                    lhs: "h",
                                                    rhs: UnsignedReal("0.001"),
                                                },
                                            },
                                            rhs: Mul("*") {
                                                lhs: "W",
                                                rhs: UnsignedReal("0.001"),
                                            },
                                        },
                                        rhs: Mul("*") {
                                            lhs: "m",
                                            rhs: "g",
                                        },
                                    },
                                },
                            ],
                        },
                    ],
                    else_block: Some(
                        [
                            Simple {
                                lhs: "Fz",
                                rhs: Add("+") {
                                    lhs: Add("+") {
                                        lhs: Add("+") {
                                            lhs: Add("+") {
                                                lhs: "m1.thrust",
                                                rhs: "m2.thrust",
                                            },
                                            rhs: "m3.thrust",
                                        },
                                        rhs: "m4.thrust",
                                    },
                                    rhs: Mul("*") {
                                        lhs: "m",
                                        rhs: "g",
                                    },
                                },
                            },
                        ],
                    ),
                },
                Simple {
                    lhs: "Mx",
                    rhs: UnsignedInteger("0"),
                },
                Simple {
                    lhs: "My",
                    rhs: UnsignedInteger("0"),
                },
                Simple {
                    lhs: "Mz",
                    rhs: UnsignedInteger("0"),
                },
                Simple {
                    lhs: "m1.omega_ref",
                    rhs: "u1",
                },
                Simple {
                    lhs: "m2.omega_ref",
                    rhs: "u2",
                },
                Simple {
                    lhs: "m3.omega_ref",
                    rhs: "u3",
                },
                Simple {
                    lhs: "m4.omega_ref",
                    rhs: "u4",
                },
            ],
            initial_equations: [],
            algorithms: [],
            initial_algorithms: [],
        },
        "RigidBody6DOF": ClassDefinition {
            name: "RigidBody6DOF",
            encapsulated: false,
            extends: [],
            components: {
                "m": Component {
                    name: "m",
                    type_name: "Real",
                    variability: Parameter(
                        "parameter",
                    ),
                },
                "g": Component {
                    name: "g",
                    type_name: "Real",
                    variability: Parameter(
                        "parameter",
                    ),
                },
                "Jx": Component {
                    name: "Jx",
                    type_name: "Real",
                    variability: Parameter(
                        "parameter",
                    ),
                },
                "Jy": Component {
                    name: "Jy",
                    type_name: "Real",
                    variability: Parameter(
                        "parameter",
                    ),
                },
                "Jz": Component {
                    name: "Jz",
                    type_name: "Real",
                    variability: Parameter(
                        "parameter",
                    ),
                },
                "Jxz": Component {
                    name: "Jxz",
                    type_name: "Real",
                    variability: Parameter(
                        "parameter",
                    ),
                },
                "Lambda": Component {
                    name: "Lambda",
                    type_name: "Real",
                    variability: Parameter(
                        "parameter",
                    ),
                },
                "x": Component {
                    name: "x",
                    type_name: "Real",
                },
                "y": Component {
                    name: "y",
                    type_name: "Real",
                },
                "h": Component {
                    name: "h",
                    type_name: "Real",
                },
                "P": Component {
                    name: "P",
                    type_name: "Real",
                },
                "Q": Component {
                    name: "Q",
                    type_name: "Real",
                },
                "R": Component {
                    name: "R",
                    type_name: "Real",
                },
                "U": Component {
                    name: "U",
                    type_name: "Real",
                },
                "V": Component {
                    name: "V",
                    type_name: "Real",
                },
                "W": Component {
                    name: "W",
                    type_name: "Real",
                },
                "Fx": Component {
                    name: "Fx",
                    type_name: "Real",
                },
                "Fy": Component {
                    name: "Fy",
                    type_name: "Real",
                },
                "Fz": Component {
                    name: "Fz",
                    type_name: "Real",
                },
                "Mx": Component {
                    name: "Mx",
                    type_name: "Real",
                },
                "My": Component {
                    name: "My",
                    type_name: "Real",
                },
                "Mz": Component {
                    name: "Mz",
                    type_name: "Real",
                },
                "phi": Component {
                    name: "phi",
                    type_name: "Real",
                },
                "theta": Component {
                    name: "theta",
                    type_name: "Real",
                },
                "psi": Component {
                    name: "psi",
                    type_name: "Real",
                },
            },
            equations: [
                Simple {
                    lhs: FunctionCall {
                        comp: "der",
                        args: [
                            "x",
                        ],
                    },
                    rhs: Add("+") {
                        lhs: Add("+") {
                            lhs: Mul("*") {
                                lhs: Mul("*") {
                                    lhs: "U",
                                    rhs: FunctionCall {
                                        comp: "cos",
                                        args: [
                                            "theta",
                                        ],
                                    },
                                },
                                rhs: FunctionCall {
                                    comp: "cos",
                                    args: [
                                        "psi",
                                    ],
                                },
                            },
                            rhs: Mul("*") {
                                lhs: "V",
                                rhs: Add("+") {
                                    lhs: Mul("*") {
                                        lhs: FunctionCall {
                                            comp: "cos",
                                            args: [
                                                "phi",
                                            ],
                                        },
                                        rhs: FunctionCall {
                                            comp: "sin",
                                            args: [
                                                "psi",
                                            ],
                                        },
                                    },
                                    rhs: Mul("*") {
                                        lhs: Mul("*") {
                                            lhs: FunctionCall {
                                                comp: "sin",
                                                args: [
                                                    "phi",
                                                ],
                                            },
                                            rhs: FunctionCall {
                                                comp: "sin",
                                                args: [
                                                    "theta",
                                                ],
                                            },
                                        },
                                        rhs: FunctionCall {
                                            comp: "cos",
                                            args: [
                                                "psi",
                                            ],
                                        },
                                    },
                                },
                            },
                        },
                        rhs: Mul("*") {
                            lhs: "W",
                            rhs: Add("+") {
                                lhs: Mul("*") {
                                    lhs: FunctionCall {
                                        comp: "sin",
                                        args: [
                                            "phi",
                                        ],
                                    },
                                    rhs: FunctionCall {
                                        comp: "sin",
                                        args: [
                                            "psi",
                                        ],
                                    },
                                },
                                rhs: Mul("*") {
                                    lhs: Mul("*") {
                                        lhs: FunctionCall {
                                            comp: "cos",
                                            args: [
                                                "phi",
                                            ],
                                        },
                                        rhs: FunctionCall {
                                            comp: "sin",
                                            args: [
                                                "theta",
                                            ],
                                        },
                                    },
                                    rhs: FunctionCall {
                                        comp: "cos",
                                        args: [
                                            "psi",
                                        ],
                                    },
                                },
                            },
                        },
                    },
                },
                Simple {
                    lhs: FunctionCall {
                        comp: "der",
                        args: [
                            "y",
                        ],
                    },
                    rhs: Add("+") {
                        lhs: Add("+") {
                            lhs: Mul("*") {
                                lhs: Mul("*") {
                                    lhs: "U",
                                    rhs: FunctionCall {
                                        comp: "cos",
                                        args: [
                                            "theta",
                                        ],
                                    },
                                },
                                rhs: FunctionCall {
                                    comp: "sin",
                                    args: [
                                        "psi",
                                    ],
                                },
                            },
                            rhs: Mul("*") {
                                lhs: "V",
                                rhs: Add("+") {
                                    lhs: Mul("*") {
                                        lhs: FunctionCall {
                                            comp: "cos",
                                            args: [
                                                "phi",
                                            ],
                                        },
                                        rhs: FunctionCall {
                                            comp: "cos",
                                            args: [
                                                "psi",
                                            ],
                                        },
                                    },
                                    rhs: Mul("*") {
                                        lhs: Mul("*") {
                                            lhs: FunctionCall {
                                                comp: "sin",
                                                args: [
                                                    "phi",
                                                ],
                                            },
                                            rhs: FunctionCall {
                                                comp: "sin",
                                                args: [
                                                    "theta",
                                                ],
                                            },
                                        },
                                        rhs: FunctionCall {
                                            comp: "sin",
                                            args: [
                                                "psi",
                                            ],
                                        },
                                    },
                                },
                            },
                        },
                        rhs: Mul("*") {
                            lhs: "W",
                            rhs: Add("+") {
                                lhs: Mul("*") {
                                    lhs: FunctionCall {
                                        comp: "sin",
                                        args: [
                                            "phi",
                                        ],
                                    },
                                    rhs: FunctionCall {
                                        comp: "cos",
                                        args: [
                                            "psi",
                                        ],
                                    },
                                },
                                rhs: Mul("*") {
                                    lhs: Mul("*") {
                                        lhs: FunctionCall {
                                            comp: "cos",
                                            args: [
                                                "phi",
                                            ],
                                        },
                                        rhs: FunctionCall {
                                            comp: "sin",
                                            args: [
                                                "theta",
                                            ],
                                        },
                                    },
                                    rhs: FunctionCall {
                                        comp: "sin",
                                        args: [
                                            "psi",
                                        ],
                                    },
                                },
                            },
                        },
                    },
                },
                Simple {
                    lhs: FunctionCall {
                        comp: "der",
                        args: [
                            "h",
                        ],
                    },
                    rhs: Sub("-") {
                        lhs: Sub("-") {
                            lhs: Mul("*") {
                                lhs: "U",
                                rhs: FunctionCall {
                                    comp: "sin",
                                    args: [
                                        "theta",
                                    ],
                                },
                            },
                            rhs: Mul("*") {
                                lhs: Mul("*") {
                                    lhs: "V",
                                    rhs: FunctionCall {
                                        comp: "sin",
                                        args: [
                                            "phi",
                                        ],
                                    },
                                },
                                rhs: FunctionCall {
                                    comp: "cos",
                                    args: [
                                        "theta",
                                    ],
                                },
                            },
                        },
                        rhs: Mul("*") {
                            lhs: Mul("*") {
                                lhs: "W",
                                rhs: FunctionCall {
                                    comp: "cos",
                                    args: [
                                        "phi",
                                    ],
                                },
                            },
                            rhs: FunctionCall {
                                comp: "cos",
                                args: [
                                    "theta",
                                ],
                            },
                        },
                    },
                },
                Simple {
                    lhs: FunctionCall {
                        comp: "der",
                        args: [
                            "U",
                        ],
                    },
                    rhs: Add("+") {
                        lhs: Sub("-") {
                            lhs: Sub("-") {
                                lhs: Mul("*") {
                                    lhs: "R",
                                    rhs: "V",
                                },
                                rhs: Mul("*") {
                                    lhs: "Q",
                                    rhs: "W",
                                },
                            },
                            rhs: Mul("*") {
                                lhs: "g",
                                rhs: FunctionCall {
                                    comp: "sin",
                                    args: [
                                        "theta",
                                    ],
                                },
                            },
                        },
                        rhs: Div("/") {
                            lhs: "Fx",
                            rhs: "m",
                        },
                    },
                },
                Simple {
                    lhs: FunctionCall {
                        comp: "der",
                        args: [
                            "V",
                        ],
                    },
                    rhs: Add("+") {
                        lhs: Add("+") {
                            lhs: Add("+") {
                                lhs: Mul("*") {
                                    lhs: "R",
                                    rhs: "U",
                                },
                                rhs: Mul("*") {
                                    lhs: "P",
                                    rhs: "W",
                                },
                            },
                            rhs: Mul("*") {
                                lhs: Mul("*") {
                                    lhs: "g",
                                    rhs: FunctionCall {
                                        comp: "sin",
                                        args: [
                                            "phi",
                                        ],
                                    },
                                },
                                rhs: FunctionCall {
                                    comp: "cos",
                                    args: [
                                        "theta",
                                    ],
                                },
                            },
                        },
                        rhs: Div("/") {
                            lhs: "Fy",
                            rhs: "m",
                        },
                    },
                },
                Simple {
                    lhs: FunctionCall {
                        comp: "der",
                        args: [
                            "W",
                        ],
                    },
                    rhs: Add("+") {
                        lhs: Add("+") {
                            lhs: Sub("-") {
                                lhs: Mul("*") {
                                    lhs: "Q",
                                    rhs: "U",
                                },
                                rhs: Mul("*") {
                                    lhs: "P",
                                    rhs: "V",
                                },
                            },
                            rhs: Mul("*") {
                                lhs: Mul("*") {
                                    lhs: "g",
                                    rhs: FunctionCall {
                                        comp: "cos",
                                        args: [
                                            "phi",
                                        ],
                                    },
                                },
                                rhs: FunctionCall {
                                    comp: "cos",
                                    args: [
                                        "theta",
                                    ],
                                },
                            },
                        },
                        rhs: Div("/") {
                            lhs: "Fz",
                            rhs: "m",
                        },
                    },
                },
                Simple {
                    lhs: FunctionCall {
                        comp: "der",
                        args: [
                            "phi",
                        ],
                    },
                    rhs: UnsignedInteger("0"),
                },
                Simple {
                    lhs: FunctionCall {
                        comp: "der",
                        args: [
                            "theta",
                        ],
                    },
                    rhs: UnsignedInteger("0"),
                },
                Simple {
                    lhs: FunctionCall {
                        comp: "der",
                        args: [
                            "psi",
                        ],
                    },
                    rhs: UnsignedInteger("0"),
                },
                Simple {
                    lhs: Mul("*") {
                        lhs: "Lambda",
                        rhs: FunctionCall {
                            comp: "der",
                            args: [
                                "P",
                            ],
                        },
                    },
                    rhs: UnsignedInteger("0"),
                },
                Simple {
                    lhs: Mul("*") {
                        lhs: "Jy",
                        rhs: FunctionCall {
                            comp: "der",
                            args: [
                                "Q",
                            ],
                        },
                    },
                    rhs: UnsignedInteger("0"),
                },
                Simple {
                    lhs: Mul("*") {
                        lhs: "Lambda",
                        rhs: FunctionCall {
                            comp: "der",
                            args: [
                                "R",
                            ],
                        },
                    },
                    rhs: UnsignedInteger("0"),
                },
            ],
            initial_equations: [],
            algorithms: [],
            initial_algorithms: [],
        },
        "Motor": ClassDefinition {
            name: "Motor",
            encapsulated: false,
            extends: [],
            components: {
                "Cm": Component {
                    name: "Cm",
                    type_name: "Real",
                    variability: Parameter(
                        "parameter",
                    ),
                },
                "Ct": Component {
                    name: "Ct",
                    type_name: "Real",
                    variability: Parameter(
                        "parameter",
                    ),
                },
                "tau": Component {
                    name: "tau",
                    type_name: "Real",
                    variability: Parameter(
                        "parameter",
                    ),
                },
                "omega_ref": Component {
                    name: "omega_ref",
                    type_name: "Real",
                },
                "omega": Component {
                    name: "omega",
                    type_name: "Real",
                },
                "thrust": Component {
                    name: "thrust",
                    type_name: "Real",
                },
                "moment": Component {
                    name: "moment",
                    type_name: "Real",
                },
            },
            equations: [
                Simple {
                    lhs: FunctionCall {
                        comp: "der",
                        args: [
                            "omega",
                        ],
                    },
                    rhs: Mul("*") {
                        lhs: Div("/") {
                            lhs: UnsignedInteger("1"),
                            rhs: "tau",
                        },
                        rhs: Sub("-") {
                            lhs: "omega_ref",
                            rhs: "omega",
                        },
                    },
                },
                Simple {
                    lhs: "thrust",
                    rhs: Mul("*") {
                        lhs: Mul("*") {
                            lhs: "Ct",
                            rhs: "omega",
                        },
                        rhs: "omega",
                    },
                },
                Simple {
                    lhs: "moment",
                    rhs: Mul("*") {
                        lhs: "Cm",
                        rhs: "thrust",
                    },
                },
            ],
            initial_equations: [],
            algorithms: [],
            initial_algorithms: [],
        },
    },
    within: None,
}
ClassDefinition {
    name: "Quadrotor",
    encapsulated: false,
    extends: [
        Extend {
            comp: "RigidBody6DOF",
        },
    ],
    components: {
        "l": Component {
            name: "l",
            type_name: "Real",
            variability: Parameter(
                "parameter",
            ),
        },
        "m1_moment": Component {
            name: "m1_moment",
            type_name: "Real",
        },
        "m2_moment": Component {
            name: "m2_moment",
            type_name: "Real",
        },
        "m3_moment": Component {
            name: "m3_moment",
            type_name: "Real",
        },
        "m4_moment": Component {
            name: "m4_moment",
            type_name: "Real",
        },
        "u1": Component {
            name: "u1",
            type_name: "Real",
            causality: Input(
                "input",
            ),
        },
        "u2": Component {
            name: "u2",
            type_name: "Real",
            causality: Input(
                "input",
            ),
        },
        "u3": Component {
            name: "u3",
            type_name: "Real",
            causality: Input(
                "input",
            ),
        },
        "u4": Component {
            name: "u4",
            type_name: "Real",
            causality: Input(
                "input",
            ),
        },
        "m": Component {
            name: "m",
            type_name: "Real",
            variability: Parameter(
                "parameter",
            ),
        },
        "g": Component {
            name: "g",
            type_name: "Real",
            variability: Parameter(
                "parameter",
            ),
        },
        "Jx": Component {
            name: "Jx",
            type_name: "Real",
            variability: Parameter(
                "parameter",
            ),
        },
        "Jy": Component {
            name: "Jy",
            type_name: "Real",
            variability: Parameter(
                "parameter",
            ),
        },
        "Jz": Component {
            name: "Jz",
            type_name: "Real",
            variability: Parameter(
                "parameter",
            ),
        },
        "Jxz": Component {
            name: "Jxz",
            type_name: "Real",
            variability: Parameter(
                "parameter",
            ),
        },
        "Lambda": Component {
            name: "Lambda",
            type_name: "Real",
            variability: Parameter(
                "parameter",
            ),
        },
        "x": Component {
            name: "x",
            type_name: "Real",
        },
        "y": Component {
            name: "y",
            type_name: "Real",
        },
        "h": Component {
            name: "h",
            type_name: "Real",
        },
        "P": Component {
            name: "P",
            type_name: "Real",
        },
        "Q": Component {
            name: "Q",
            type_name: "Real",
        },
        "R": Component {
            name: "R",
            type_name: "Real",
        },
        "U": Component {
            name: "U",
            type_name: "Real",
        },
        "V": Component {
            name: "V",
            type_name: "Real",
        },
        "W": Component {
            name: "W",
            type_name: "Real",
        },
        "Fx": Component {
            name: "Fx",
            type_name: "Real",
        },
        "Fy": Component {
            name: "Fy",
            type_name: "Real",
        },
        "Fz": Component {
            name: "Fz",
            type_name: "Real",
        },
        "Mx": Component {
            name: "Mx",
            type_name: "Real",
        },
        "My": Component {
            name: "My",
            type_name: "Real",
        },
        "Mz": Component {
            name: "Mz",
            type_name: "Real",
        },
        "phi": Component {
            name: "phi",
            type_name: "Real",
        },
        "theta": Component {
            name: "theta",
            type_name: "Real",
        },
        "psi": Component {
            name: "psi",
            type_name: "Real",
        },
        "m1_Cm": Component {
            name: "m1_Cm",
            type_name: "Real",
            variability: Parameter(
                "parameter",
            ),
        },
        "m1_Ct": Component {
            name: "m1_Ct",
            type_name: "Real",
            variability: Parameter(
                "parameter",
            ),
        },
        "m1_tau": Component {
            name: "m1_tau",
            type_name: "Real",
            variability: Parameter(
                "parameter",
            ),
        },
        "m1_omega_ref": Component {
            name: "m1_omega_ref",
            type_name: "Real",
        },
        "m1_omega": Component {
            name: "m1_omega",
            type_name: "Real",
        },
        "m1_thrust": Component {
            name: "m1_thrust",
            type_name: "Real",
        },
        "m2_Cm": Component {
            name: "m2_Cm",
            type_name: "Real",
            variability: Parameter(
                "parameter",
            ),
        },
        "m2_Ct": Component {
            name: "m2_Ct",
            type_name: "Real",
            variability: Parameter(
                "parameter",
            ),
        },
        "m2_tau": Component {
            name: "m2_tau",
            type_name: "Real",
            variability: Parameter(
                "parameter",
            ),
        },
        "m2_omega_ref": Component {
            name: "m2_omega_ref",
            type_name: "Real",
        },
        "m2_omega": Component {
            name: "m2_omega",
            type_name: "Real",
        },
        "m2_thrust": Component {
            name: "m2_thrust",
            type_name: "Real",
        },
        "m3_Cm": Component {
            name: "m3_Cm",
            type_name: "Real",
            variability: Parameter(
                "parameter",
            ),
        },
        "m3_Ct": Component {
            name: "m3_Ct",
            type_name: "Real",
            variability: Parameter(
                "parameter",
            ),
        },
        "m3_tau": Component {
            name: "m3_tau",
            type_name: "Real",
            variability: Parameter(
                "parameter",
            ),
        },
        "m3_omega_ref": Component {
            name: "m3_omega_ref",
            type_name: "Real",
        },
        "m3_omega": Component {
            name: "m3_omega",
            type_name: "Real",
        },
        "m3_thrust": Component {
            name: "m3_thrust",
            type_name: "Real",
        },
        "m4_Cm": Component {
            name: "m4_Cm",
            type_name: "Real",
            variability: Parameter(
                "parameter",
            ),
        },
        "m4_Ct": Component {
            name: "m4_Ct",
            type_name: "Real",
            variability: Parameter(
                "parameter",
            ),
        },
        "m4_tau": Component {
            name: "m4_tau",
            type_name: "Real",
            variability: Parameter(
                "parameter",
            ),
        },
        "m4_omega_ref": Component {
            name: "m4_omega_ref",
            type_name: "Real",
        },
        "m4_omega": Component {
            name: "m4_omega",
            type_name: "Real",
        },
        "m4_thrust": Component {
            name: "m4_thrust",
            type_name: "Real",
        },
    },
    equations: [
        Simple {
            lhs: "Fx",
            rhs: UnsignedInteger("0"),
        },
        Simple {
            lhs: "Fy",
            rhs: UnsignedInteger("0"),
        },
        If {
            cond_blocks: [
                EquationBlock {
                    cond: Lt("<") {
                        lhs: "h",
                        rhs: UnsignedInteger("0"),
                    },
                    eqs: [
                        Simple {
                            lhs: "Fz",
                            rhs: Add("+") {
                                lhs: Add("+") {
                                    lhs: Sub("-") {
                                        lhs: Add("+") {
                                            lhs: Add("+") {
                                                lhs: Add("+") {
                                                    lhs: "m1.thrust",
                                                    rhs: "m2.thrust",
                                                },
                                                rhs: "m3.thrust",
                                            },
                                            rhs: "m4.thrust",
                                        },
                                        rhs: Mul("*") {
                                            lhs: "h",
                                            rhs: UnsignedReal("0.001"),
                                        },
                                    },
                                    rhs: Mul("*") {
                                        lhs: "W",
                                        rhs: UnsignedReal("0.001"),
                                    },
                                },
                                rhs: Mul("*") {
                                    lhs: "m",
                                    rhs: "g",
                                },
                            },
                        },
                    ],
                },
            ],
            else_block: Some(
                [
                    Simple {
                        lhs: "Fz",
                        rhs: Add("+") {
                            lhs: Add("+") {
                                lhs: Add("+") {
                                    lhs: Add("+") {
                                        lhs: "m1.thrust",
                                        rhs: "m2.thrust",
                                    },
                                    rhs: "m3.thrust",
                                },
                                rhs: "m4.thrust",
                            },
                            rhs: Mul("*") {
                                lhs: "m",
                                rhs: "g",
                            },
                        },
                    },
                ],
            ),
        },
        Simple {
            lhs: "Mx",
            rhs: UnsignedInteger("0"),
        },
        Simple {
            lhs: "My",
            rhs: UnsignedInteger("0"),
        },
        Simple {
            lhs: "Mz",
            rhs: UnsignedInteger("0"),
        },
        Simple {
            lhs: "m1_omega_ref",
            rhs: "u1",
        },
        Simple {
            lhs: "m2_omega_ref",
            rhs: "u2",
        },
        Simple {
            lhs: "m3_omega_ref",
            rhs: "u3",
        },
        Simple {
            lhs: "m4_omega_ref",
            rhs: "u4",
        },
        Simple {
            lhs: FunctionCall {
                comp: "der",
                args: [
                    "x",
                ],
            },
            rhs: Add("+") {
                lhs: Add("+") {
                    lhs: Mul("*") {
                        lhs: Mul("*") {
                            lhs: "U",
                            rhs: FunctionCall {
                                comp: "cos",
                                args: [
                                    "theta",
                                ],
                            },
                        },
                        rhs: FunctionCall {
                            comp: "cos",
                            args: [
                                "psi",
                            ],
                        },
                    },
                    rhs: Mul("*") {
                        lhs: "V",
                        rhs: Add("+") {
                            lhs: Mul("*") {
                                lhs: FunctionCall {
                                    comp: "cos",
                                    args: [
                                        "phi",
                                    ],
                                },
                                rhs: FunctionCall {
                                    comp: "sin",
                                    args: [
                                        "psi",
                                    ],
                                },
                            },
                            rhs: Mul("*") {
                                lhs: Mul("*") {
                                    lhs: FunctionCall {
                                        comp: "sin",
                                        args: [
                                            "phi",
                                        ],
                                    },
                                    rhs: FunctionCall {
                                        comp: "sin",
                                        args: [
                                            "theta",
                                        ],
                                    },
                                },
                                rhs: FunctionCall {
                                    comp: "cos",
                                    args: [
                                        "psi",
                                    ],
                                },
                            },
                        },
                    },
                },
                rhs: Mul("*") {
                    lhs: "W",
                    rhs: Add("+") {
                        lhs: Mul("*") {
                            lhs: FunctionCall {
                                comp: "sin",
                                args: [
                                    "phi",
                                ],
                            },
                            rhs: FunctionCall {
                                comp: "sin",
                                args: [
                                    "psi",
                                ],
                            },
                        },
                        rhs: Mul("*") {
                            lhs: Mul("*") {
                                lhs: FunctionCall {
                                    comp: "cos",
                                    args: [
                                        "phi",
                                    ],
                                },
                                rhs: FunctionCall {
                                    comp: "sin",
                                    args: [
                                        "theta",
                                    ],
                                },
                            },
                            rhs: FunctionCall {
                                comp: "cos",
                                args: [
                                    "psi",
                                ],
                            },
                        },
                    },
                },
            },
        },
        Simple {
            lhs: FunctionCall {
                comp: "der",
                args: [
                    "y",
                ],
            },
            rhs: Add("+") {
                lhs: Add("+") {
                    lhs: Mul("*") {
                        lhs: Mul("*") {
                            lhs: "U",
                            rhs: FunctionCall {
                                comp: "cos",
                                args: [
                                    "theta",
                                ],
                            },
                        },
                        rhs: FunctionCall {
                            comp: "sin",
                            args: [
                                "psi",
                            ],
                        },
                    },
                    rhs: Mul("*") {
                        lhs: "V",
                        rhs: Add("+") {
                            lhs: Mul("*") {
                                lhs: FunctionCall {
                                    comp: "cos",
                                    args: [
                                        "phi",
                                    ],
                                },
                                rhs: FunctionCall {
                                    comp: "cos",
                                    args: [
                                        "psi",
                                    ],
                                },
                            },
                            rhs: Mul("*") {
                                lhs: Mul("*") {
                                    lhs: FunctionCall {
                                        comp: "sin",
                                        args: [
                                            "phi",
                                        ],
                                    },
                                    rhs: FunctionCall {
                                        comp: "sin",
                                        args: [
                                            "theta",
                                        ],
                                    },
                                },
                                rhs: FunctionCall {
                                    comp: "sin",
                                    args: [
                                        "psi",
                                    ],
                                },
                            },
                        },
                    },
                },
                rhs: Mul("*") {
                    lhs: "W",
                    rhs: Add("+") {
                        lhs: Mul("*") {
                            lhs: FunctionCall {
                                comp: "sin",
                                args: [
                                    "phi",
                                ],
                            },
                            rhs: FunctionCall {
                                comp: "cos",
                                args: [
                                    "psi",
                                ],
                            },
                        },
                        rhs: Mul("*") {
                            lhs: Mul("*") {
                                lhs: FunctionCall {
                                    comp: "cos",
                                    args: [
                                        "phi",
                                    ],
                                },
                                rhs: FunctionCall {
                                    comp: "sin",
                                    args: [
                                        "theta",
                                    ],
                                },
                            },
                            rhs: FunctionCall {
                                comp: "sin",
                                args: [
                                    "psi",
                                ],
                            },
                        },
                    },
                },
            },
        },
        Simple {
            lhs: FunctionCall {
                comp: "der",
                args: [
                    "h",
                ],
            },
            rhs: Sub("-") {
                lhs: Sub("-") {
                    lhs: Mul("*") {
                        lhs: "U",
                        rhs: FunctionCall {
                            comp: "sin",
                            args: [
                                "theta",
                            ],
                        },
                    },
                    rhs: Mul("*") {
                        lhs: Mul("*") {
                            lhs: "V",
                            rhs: FunctionCall {
                                comp: "sin",
                                args: [
                                    "phi",
                                ],
                            },
                        },
                        rhs: FunctionCall {
                            comp: "cos",
                            args: [
                                "theta",
                            ],
                        },
                    },
                },
                rhs: Mul("*") {
                    lhs: Mul("*") {
                        lhs: "W",
                        rhs: FunctionCall {
                            comp: "cos",
                            args: [
                                "phi",
                            ],
                        },
                    },
                    rhs: FunctionCall {
                        comp: "cos",
                        args: [
                            "theta",
                        ],
                    },
                },
            },
        },
        Simple {
            lhs: FunctionCall {
                comp: "der",
                args: [
                    "U",
                ],
            },
            rhs: Add("+") {
                lhs: Sub("-") {
                    lhs: Sub("-") {
                        lhs: Mul("*") {
                            lhs: "R",
                            rhs: "V",
                        },
                        rhs: Mul("*") {
                            lhs: "Q",
                            rhs: "W",
                        },
                    },
                    rhs: Mul("*") {
                        lhs: "g",
                        rhs: FunctionCall {
                            comp: "sin",
                            args: [
                                "theta",
                            ],
                        },
                    },
                },
                rhs: Div("/") {
                    lhs: "Fx",
                    rhs: "m",
                },
            },
        },
        Simple {
            lhs: FunctionCall {
                comp: "der",
                args: [
                    "V",
                ],
            },
            rhs: Add("+") {
                lhs: Add("+") {
                    lhs: Add("+") {
                        lhs: Mul("*") {
                            lhs: "R",
                            rhs: "U",
                        },
                        rhs: Mul("*") {
                            lhs: "P",
                            rhs: "W",
                        },
                    },
                    rhs: Mul("*") {
                        lhs: Mul("*") {
                            lhs: "g",
                            rhs: FunctionCall {
                                comp: "sin",
                                args: [
                                    "phi",
                                ],
                            },
                        },
                        rhs: FunctionCall {
                            comp: "cos",
                            args: [
                                "theta",
                            ],
                        },
                    },
                },
                rhs: Div("/") {
                    lhs: "Fy",
                    rhs: "m",
                },
            },
        },
        Simple {
            lhs: FunctionCall {
                comp: "der",
                args: [
                    "W",
                ],
            },
            rhs: Add("+") {
                lhs: Add("+") {
                    lhs: Sub("-") {
                        lhs: Mul("*") {
                            lhs: "Q",
                            rhs: "U",
                        },
                        rhs: Mul("*") {
                            lhs: "P",
                            rhs: "V",
                        },
                    },
                    rhs: Mul("*") {
                        lhs: Mul("*") {
                            lhs: "g",
                            rhs: FunctionCall {
                                comp: "cos",
                                args: [
                                    "phi",
                                ],
                            },
                        },
                        rhs: FunctionCall {
                            comp: "cos",
                            args: [
                                "theta",
                            ],
                        },
                    },
                },
                rhs: Div("/") {
                    lhs: "Fz",
                    rhs: "m",
                },
            },
        },
        Simple {
            lhs: FunctionCall {
                comp: "der",
                args: [
                    "phi",
                ],
            },
            rhs: UnsignedInteger("0"),
        },
        Simple {
            lhs: FunctionCall {
                comp: "der",
                args: [
                    "theta",
                ],
            },
            rhs: UnsignedInteger("0"),
        },
        Simple {
            lhs: FunctionCall {
                comp: "der",
                args: [
                    "psi",
                ],
            },
            rhs: UnsignedInteger("0"),
        },
        Simple {
            lhs: Mul("*") {
                lhs: "Lambda",
                rhs: FunctionCall {
                    comp: "der",
                    args: [
                        "P",
                    ],
                },
            },
            rhs: UnsignedInteger("0"),
        },
        Simple {
            lhs: Mul("*") {
                lhs: "Jy",
                rhs: FunctionCall {
                    comp: "der",
                    args: [
                        "Q",
                    ],
                },
            },
            rhs: UnsignedInteger("0"),
        },
        Simple {
            lhs: Mul("*") {
                lhs: "Lambda",
                rhs: FunctionCall {
                    comp: "der",
                    args: [
                        "R",
                    ],
                },
            },
            rhs: UnsignedInteger("0"),
        },
        Simple {
            lhs: FunctionCall {
                comp: "der",
                args: [
                    "m1_omega",
                ],
            },
            rhs: Mul("*") {
                lhs: Div("/") {
                    lhs: UnsignedInteger("1"),
                    rhs: "m1_tau",
                },
                rhs: Sub("-") {
                    lhs: "m1_omega_ref",
                    rhs: "m1_omega",
                },
            },
        },
        Simple {
            lhs: "m1_thrust",
            rhs: Mul("*") {
                lhs: Mul("*") {
                    lhs: "m1_Ct",
                    rhs: "m1_omega",
                },
                rhs: "m1_omega",
            },
        },
        Simple {
            lhs: "m1_moment",
            rhs: Mul("*") {
                lhs: "m1_Cm",
                rhs: "m1_thrust",
            },
        },
        Simple {
            lhs: FunctionCall {
                comp: "der",
                args: [
                    "m2_omega",
                ],
            },
            rhs: Mul("*") {
                lhs: Div("/") {
                    lhs: UnsignedInteger("1"),
                    rhs: "m2_tau",
                },
                rhs: Sub("-") {
                    lhs: "m2_omega_ref",
                    rhs: "m2_omega",
                },
            },
        },
        Simple {
            lhs: "m2_thrust",
            rhs: Mul("*") {
                lhs: Mul("*") {
                    lhs: "m2_Ct",
                    rhs: "m2_omega",
                },
                rhs: "m2_omega",
            },
        },
        Simple {
            lhs: "m2_moment",
            rhs: Mul("*") {
                lhs: "m2_Cm",
                rhs: "m2_thrust",
            },
        },
        Simple {
            lhs: FunctionCall {
                comp: "der",
                args: [
                    "m3_omega",
                ],
            },
            rhs: Mul("*") {
                lhs: Div("/") {
                    lhs: UnsignedInteger("1"),
                    rhs: "m3_tau",
                },
                rhs: Sub("-") {
                    lhs: "m3_omega_ref",
                    rhs: "m3_omega",
                },
            },
        },
        Simple {
            lhs: "m3_thrust",
            rhs: Mul("*") {
                lhs: Mul("*") {
                    lhs: "m3_Ct",
                    rhs: "m3_omega",
                },
                rhs: "m3_omega",
            },
        },
        Simple {
            lhs: "m3_moment",
            rhs: Mul("*") {
                lhs: "m3_Cm",
                rhs: "m3_thrust",
            },
        },
        Simple {
            lhs: FunctionCall {
                comp: "der",
                args: [
                    "m4_omega",
                ],
            },
            rhs: Mul("*") {
                lhs: Div("/") {
                    lhs: UnsignedInteger("1"),
                    rhs: "m4_tau",
                },
                rhs: Sub("-") {
                    lhs: "m4_omega_ref",
                    rhs: "m4_omega",
                },
            },
        },
        Simple {
            lhs: "m4_thrust",
            rhs: Mul("*") {
                lhs: Mul("*") {
                    lhs: "m4_Ct",
                    rhs: "m4_omega",
                },
                rhs: "m4_omega",
            },
        },
        Simple {
            lhs: "m4_moment",
            rhs: Mul("*") {
                lhs: "m4_Cm",
                rhs: "m4_thrust",
            },
        },
    ],
    initial_equations: [],
    algorithms: [],
    initial_algorithms: [],
}
Dae {
    p: [
        Component {
            name: "l",
            type_name: "Real",
            variability: Parameter(
                "parameter",
            ),
        },
        Component {
            name: "m",
            type_name: "Real",
            variability: Parameter(
                "parameter",
            ),
        },
        Component {
            name: "g",
            type_name: "Real",
            variability: Parameter(
                "parameter",
            ),
        },
        Component {
            name: "Jx",
            type_name: "Real",
            variability: Parameter(
                "parameter",
            ),
        },
        Component {
            name: "Jy",
            type_name: "Real",
            variability: Parameter(
                "parameter",
            ),
        },
        Component {
            name: "Jz",
            type_name: "Real",
            variability: Parameter(
                "parameter",
            ),
        },
        Component {
            name: "Jxz",
            type_name: "Real",
            variability: Parameter(
                "parameter",
            ),
        },
        Component {
            name: "Lambda",
            type_name: "Real",
            variability: Parameter(
                "parameter",
            ),
        },
        Component {
            name: "m1_Cm",
            type_name: "Real",
            variability: Parameter(
                "parameter",
            ),
        },
        Component {
            name: "m1_Ct",
            type_name: "Real",
            variability: Parameter(
                "parameter",
            ),
        },
        Component {
            name: "m1_tau",
            type_name: "Real",
            variability: Parameter(
                "parameter",
            ),
        },
        Component {
            name: "m2_Cm",
            type_name: "Real",
            variability: Parameter(
                "parameter",
            ),
        },
        Component {
            name: "m2_Ct",
            type_name: "Real",
            variability: Parameter(
                "parameter",
            ),
        },
        Component {
            name: "m2_tau",
            type_name: "Real",
            variability: Parameter(
                "parameter",
            ),
        },
        Component {
            name: "m3_Cm",
            type_name: "Real",
            variability: Parameter(
                "parameter",
            ),
        },
        Component {
            name: "m3_Ct",
            type_name: "Real",
            variability: Parameter(
                "parameter",
            ),
        },
        Component {
            name: "m3_tau",
            type_name: "Real",
            variability: Parameter(
                "parameter",
            ),
        },
        Component {
            name: "m4_Cm",
            type_name: "Real",
            variability: Parameter(
                "parameter",
            ),
        },
        Component {
            name: "m4_Ct",
            type_name: "Real",
            variability: Parameter(
                "parameter",
            ),
        },
        Component {
            name: "m4_tau",
            type_name: "Real",
            variability: Parameter(
                "parameter",
            ),
        },
    ],
    cp: [],
    t: Component {
        name: "t",
        type_name: "Real",
    },
    x: [
        Component {
            name: "x",
            type_name: "Real",
        },
        Component {
            name: "y",
            type_name: "Real",
        },
        Component {
            name: "h",
            type_name: "Real",
        },
        Component {
            name: "P",
            type_name: "Real",
        },
        Component {
            name: "Q",
            type_name: "Real",
        },
        Component {
            name: "R",
            type_name: "Real",
        },
        Component {
            name: "U",
            type_name: "Real",
        },
        Component {
            name: "V",
            type_name: "Real",
        },
        Component {
            name: "W",
            type_name: "Real",
        },
        Component {
            name: "phi",
            type_name: "Real",
        },
        Component {
            name: "theta",
            type_name: "Real",
        },
        Component {
            name: "psi",
            type_name: "Real",
        },
        Component {
            name: "m1_omega",
            type_name: "Real",
        },
        Component {
            name: "m2_omega",
            type_name: "Real",
        },
        Component {
            name: "m3_omega",
            type_name: "Real",
        },
        Component {
            name: "m4_omega",
            type_name: "Real",
        },
    ],
    x_dot: [
        Component {
            name: "der_x",
            type_name: "Real",
        },
        Component {
            name: "der_y",
            type_name: "Real",
        },
        Component {
            name: "der_h",
            type_name: "Real",
        },
        Component {
            name: "der_P",
            type_name: "Real",
        },
        Component {
            name: "der_Q",
            type_name: "Real",
        },
        Component {
            name: "der_R",
            type_name: "Real",
        },
        Component {
            name: "der_U",
            type_name: "Real",
        },
        Component {
            name: "der_V",
            type_name: "Real",
        },
        Component {
            name: "der_W",
            type_name: "Real",
        },
        Component {
            name: "der_phi",
            type_name: "Real",
        },
        Component {
            name: "der_theta",
            type_name: "Real",
        },
        Component {
            name: "der_psi",
            type_name: "Real",
        },
        Component {
            name: "der_m1_omega",
            type_name: "Real",
        },
        Component {
            name: "der_m2_omega",
            type_name: "Real",
        },
        Component {
            name: "der_m3_omega",
            type_name: "Real",
        },
        Component {
            name: "der_m4_omega",
            type_name: "Real",
        },
    ],
    y: [
        Component {
            name: "m1_moment",
            type_name: "Real",
        },
        Component {
            name: "m2_moment",
            type_name: "Real",
        },
        Component {
            name: "m3_moment",
            type_name: "Real",
        },
        Component {
            name: "m4_moment",
            type_name: "Real",
        },
        Component {
            name: "Fx",
            type_name: "Real",
        },
        Component {
            name: "Fy",
            type_name: "Real",
        },
        Component {
            name: "Fz",
            type_name: "Real",
        },
        Component {
            name: "Mx",
            type_name: "Real",
        },
        Component {
            name: "My",
            type_name: "Real",
        },
        Component {
            name: "Mz",
            type_name: "Real",
        },
        Component {
            name: "m1_omega_ref",
            type_name: "Real",
        },
        Component {
            name: "m1_thrust",
            type_name: "Real",
        },
        Component {
            name: "m2_omega_ref",
            type_name: "Real",
        },
        Component {
            name: "m2_thrust",
            type_name: "Real",
        },
        Component {
            name: "m3_omega_ref",
            type_name: "Real",
        },
        Component {
            name: "m3_thrust",
            type_name: "Real",
        },
        Component {
            name: "m4_omega_ref",
            type_name: "Real",
        },
        Component {
            name: "m4_thrust",
            type_name: "Real",
        },
    ],
    u: [
        Component {
            name: "u1",
            type_name: "Real",
            causality: Input(
                "input",
            ),
        },
        Component {
            name: "u2",
            type_name: "Real",
            causality: Input(
                "input",
            ),
        },
        Component {
            name: "u3",
            type_name: "Real",
            causality: Input(
                "input",
            ),
        },
        Component {
            name: "u4",
            type_name: "Real",
            causality: Input(
                "input",
            ),
        },
    ],
    pre_z: [],
    pre_x: [
        Component {
            name: "pre_x",
            type_name: "Real",
        },
        Component {
            name: "pre_y",
            type_name: "Real",
        },
        Component {
            name: "pre_h",
            type_name: "Real",
        },
        Component {
            name: "pre_P",
            type_name: "Real",
        },
        Component {
            name: "pre_Q",
            type_name: "Real",
        },
        Component {
            name: "pre_R",
            type_name: "Real",
        },
        Component {
            name: "pre_U",
            type_name: "Real",
        },
        Component {
            name: "pre_V",
            type_name: "Real",
        },
        Component {
            name: "pre_W",
            type_name: "Real",
        },
        Component {
            name: "pre_phi",
            type_name: "Real",
        },
        Component {
            name: "pre_theta",
            type_name: "Real",
        },
        Component {
            name: "pre_psi",
            type_name: "Real",
        },
        Component {
            name: "pre_m1_omega",
            type_name: "Real",
        },
        Component {
            name: "pre_m2_omega",
            type_name: "Real",
        },
        Component {
            name: "pre_m3_omega",
            type_name: "Real",
        },
        Component {
            name: "pre_m4_omega",
            type_name: "Real",
        },
    ],
    pre_m: [],
    z: [],
    m: [],
    c: {},
    fx: [
        Simple {
            lhs: "Fx",
            rhs: UnsignedInteger("0"),
        },
        Simple {
            lhs: "Fy",
            rhs: UnsignedInteger("0"),
        },
        If {
            cond_blocks: [
                EquationBlock {
                    cond: Lt("<") {
                        lhs: "h",
                        rhs: UnsignedInteger("0"),
                    },
                    eqs: [
                        Simple {
                            lhs: "Fz",
                            rhs: Add("+") {
                                lhs: Add("+") {
                                    lhs: Sub("-") {
                                        lhs: Add("+") {
                                            lhs: Add("+") {
                                                lhs: Add("+") {
                                                    lhs: "m1.thrust",
                                                    rhs: "m2.thrust",
                                                },
                                                rhs: "m3.thrust",
                                            },
                                            rhs: "m4.thrust",
                                        },
                                        rhs: Mul("*") {
                                            lhs: "h",
                                            rhs: UnsignedReal("0.001"),
                                        },
                                    },
                                    rhs: Mul("*") {
                                        lhs: "W",
                                        rhs: UnsignedReal("0.001"),
                                    },
                                },
                                rhs: Mul("*") {
                                    lhs: "m",
                                    rhs: "g",
                                },
                            },
                        },
                    ],
                },
            ],
            else_block: Some(
                [
                    Simple {
                        lhs: "Fz",
                        rhs: Add("+") {
                            lhs: Add("+") {
                                lhs: Add("+") {
                                    lhs: Add("+") {
                                        lhs: "m1.thrust",
                                        rhs: "m2.thrust",
                                    },
                                    rhs: "m3.thrust",
                                },
                                rhs: "m4.thrust",
                            },
                            rhs: Mul("*") {
                                lhs: "m",
                                rhs: "g",
                            },
                        },
                    },
                ],
            ),
        },
        Simple {
            lhs: "Mx",
            rhs: UnsignedInteger("0"),
        },
        Simple {
            lhs: "My",
            rhs: UnsignedInteger("0"),
        },
        Simple {
            lhs: "Mz",
            rhs: UnsignedInteger("0"),
        },
        Simple {
            lhs: "m1_omega_ref",
            rhs: "u1",
        },
        Simple {
            lhs: "m2_omega_ref",
            rhs: "u2",
        },
        Simple {
            lhs: "m3_omega_ref",
            rhs: "u3",
        },
        Simple {
            lhs: "m4_omega_ref",
            rhs: "u4",
        },
        Simple {
            lhs: "der_x",
            rhs: Add("+") {
                lhs: Add("+") {
                    lhs: Mul("*") {
                        lhs: Mul("*") {
                            lhs: "U",
                            rhs: FunctionCall {
                                comp: "cos",
                                args: [
                                    "theta",
                                ],
                            },
                        },
                        rhs: FunctionCall {
                            comp: "cos",
                            args: [
                                "psi",
                            ],
                        },
                    },
                    rhs: Mul("*") {
                        lhs: "V",
                        rhs: Add("+") {
                            lhs: Mul("*") {
                                lhs: FunctionCall {
                                    comp: "cos",
                                    args: [
                                        "phi",
                                    ],
                                },
                                rhs: FunctionCall {
                                    comp: "sin",
                                    args: [
                                        "psi",
                                    ],
                                },
                            },
                            rhs: Mul("*") {
                                lhs: Mul("*") {
                                    lhs: FunctionCall {
                                        comp: "sin",
                                        args: [
                                            "phi",
                                        ],
                                    },
                                    rhs: FunctionCall {
                                        comp: "sin",
                                        args: [
                                            "theta",
                                        ],
                                    },
                                },
                                rhs: FunctionCall {
                                    comp: "cos",
                                    args: [
                                        "psi",
                                    ],
                                },
                            },
                        },
                    },
                },
                rhs: Mul("*") {
                    lhs: "W",
                    rhs: Add("+") {
                        lhs: Mul("*") {
                            lhs: FunctionCall {
                                comp: "sin",
                                args: [
                                    "phi",
                                ],
                            },
                            rhs: FunctionCall {
                                comp: "sin",
                                args: [
                                    "psi",
                                ],
                            },
                        },
                        rhs: Mul("*") {
                            lhs: Mul("*") {
                                lhs: FunctionCall {
                                    comp: "cos",
                                    args: [
                                        "phi",
                                    ],
                                },
                                rhs: FunctionCall {
                                    comp: "sin",
                                    args: [
                                        "theta",
                                    ],
                                },
                            },
                            rhs: FunctionCall {
                                comp: "cos",
                                args: [
                                    "psi",
                                ],
                            },
                        },
                    },
                },
            },
        },
        Simple {
            lhs: "der_y",
            rhs: Add("+") {
                lhs: Add("+") {
                    lhs: Mul("*") {
                        lhs: Mul("*") {
                            lhs: "U",
                            rhs: FunctionCall {
                                comp: "cos",
                                args: [
                                    "theta",
                                ],
                            },
                        },
                        rhs: FunctionCall {
                            comp: "sin",
                            args: [
                                "psi",
                            ],
                        },
                    },
                    rhs: Mul("*") {
                        lhs: "V",
                        rhs: Add("+") {
                            lhs: Mul("*") {
                                lhs: FunctionCall {
                                    comp: "cos",
                                    args: [
                                        "phi",
                                    ],
                                },
                                rhs: FunctionCall {
                                    comp: "cos",
                                    args: [
                                        "psi",
                                    ],
                                },
                            },
                            rhs: Mul("*") {
                                lhs: Mul("*") {
                                    lhs: FunctionCall {
                                        comp: "sin",
                                        args: [
                                            "phi",
                                        ],
                                    },
                                    rhs: FunctionCall {
                                        comp: "sin",
                                        args: [
                                            "theta",
                                        ],
                                    },
                                },
                                rhs: FunctionCall {
                                    comp: "sin",
                                    args: [
                                        "psi",
                                    ],
                                },
                            },
                        },
                    },
                },
                rhs: Mul("*") {
                    lhs: "W",
                    rhs: Add("+") {
                        lhs: Mul("*") {
                            lhs: FunctionCall {
                                comp: "sin",
                                args: [
                                    "phi",
                                ],
                            },
                            rhs: FunctionCall {
                                comp: "cos",
                                args: [
                                    "psi",
                                ],
                            },
                        },
                        rhs: Mul("*") {
                            lhs: Mul("*") {
                                lhs: FunctionCall {
                                    comp: "cos",
                                    args: [
                                        "phi",
                                    ],
                                },
                                rhs: FunctionCall {
                                    comp: "sin",
                                    args: [
                                        "theta",
                                    ],
                                },
                            },
                            rhs: FunctionCall {
                                comp: "sin",
                                args: [
                                    "psi",
                                ],
                            },
                        },
                    },
                },
            },
        },
        Simple {
            lhs: "der_h",
            rhs: Sub("-") {
                lhs: Sub("-") {
                    lhs: Mul("*") {
                        lhs: "U",
                        rhs: FunctionCall {
                            comp: "sin",
                            args: [
                                "theta",
                            ],
                        },
                    },
                    rhs: Mul("*") {
                        lhs: Mul("*") {
                            lhs: "V",
                            rhs: FunctionCall {
                                comp: "sin",
                                args: [
                                    "phi",
                                ],
                            },
                        },
                        rhs: FunctionCall {
                            comp: "cos",
                            args: [
                                "theta",
                            ],
                        },
                    },
                },
                rhs: Mul("*") {
                    lhs: Mul("*") {
                        lhs: "W",
                        rhs: FunctionCall {
                            comp: "cos",
                            args: [
                                "phi",
                            ],
                        },
                    },
                    rhs: FunctionCall {
                        comp: "cos",
                        args: [
                            "theta",
                        ],
                    },
                },
            },
        },
        Simple {
            lhs: "der_U",
            rhs: Add("+") {
                lhs: Sub("-") {
                    lhs: Sub("-") {
                        lhs: Mul("*") {
                            lhs: "R",
                            rhs: "V",
                        },
                        rhs: Mul("*") {
                            lhs: "Q",
                            rhs: "W",
                        },
                    },
                    rhs: Mul("*") {
                        lhs: "g",
                        rhs: FunctionCall {
                            comp: "sin",
                            args: [
                                "theta",
                            ],
                        },
                    },
                },
                rhs: Div("/") {
                    lhs: "Fx",
                    rhs: "m",
                },
            },
        },
        Simple {
            lhs: "der_V",
            rhs: Add("+") {
                lhs: Add("+") {
                    lhs: Add("+") {
                        lhs: Mul("*") {
                            lhs: "R",
                            rhs: "U",
                        },
                        rhs: Mul("*") {
                            lhs: "P",
                            rhs: "W",
                        },
                    },
                    rhs: Mul("*") {
                        lhs: Mul("*") {
                            lhs: "g",
                            rhs: FunctionCall {
                                comp: "sin",
                                args: [
                                    "phi",
                                ],
                            },
                        },
                        rhs: FunctionCall {
                            comp: "cos",
                            args: [
                                "theta",
                            ],
                        },
                    },
                },
                rhs: Div("/") {
                    lhs: "Fy",
                    rhs: "m",
                },
            },
        },
        Simple {
            lhs: "der_W",
            rhs: Add("+") {
                lhs: Add("+") {
                    lhs: Sub("-") {
                        lhs: Mul("*") {
                            lhs: "Q",
                            rhs: "U",
                        },
                        rhs: Mul("*") {
                            lhs: "P",
                            rhs: "V",
                        },
                    },
                    rhs: Mul("*") {
                        lhs: Mul("*") {
                            lhs: "g",
                            rhs: FunctionCall {
                                comp: "cos",
                                args: [
                                    "phi",
                                ],
                            },
                        },
                        rhs: FunctionCall {
                            comp: "cos",
                            args: [
                                "theta",
                            ],
                        },
                    },
                },
                rhs: Div("/") {
                    lhs: "Fz",
                    rhs: "m",
                },
            },
        },
        Simple {
            lhs: "der_phi",
            rhs: UnsignedInteger("0"),
        },
        Simple {
            lhs: "der_theta",
            rhs: UnsignedInteger("0"),
        },
        Simple {
            lhs: "der_psi",
            rhs: UnsignedInteger("0"),
        },
        Simple {
            lhs: Mul("*") {
                lhs: "Lambda",
                rhs: "der_P",
            },
            rhs: UnsignedInteger("0"),
        },
        Simple {
            lhs: Mul("*") {
                lhs: "Jy",
                rhs: "der_Q",
            },
            rhs: UnsignedInteger("0"),
        },
        Simple {
            lhs: Mul("*") {
                lhs: "Lambda",
                rhs: "der_R",
            },
            rhs: UnsignedInteger("0"),
        },
        Simple {
            lhs: "der_m1_omega",
            rhs: Mul("*") {
                lhs: Div("/") {
                    lhs: UnsignedInteger("1"),
                    rhs: "m1_tau",
                },
                rhs: Sub("-") {
                    lhs: "m1_omega_ref",
                    rhs: "m1_omega",
                },
            },
        },
        Simple {
            lhs: "m1_thrust",
            rhs: Mul("*") {
                lhs: Mul("*") {
                    lhs: "m1_Ct",
                    rhs: "m1_omega",
                },
                rhs: "m1_omega",
            },
        },
        Simple {
            lhs: "m1_moment",
            rhs: Mul("*") {
                lhs: "m1_Cm",
                rhs: "m1_thrust",
            },
        },
        Simple {
            lhs: "der_m2_omega",
            rhs: Mul("*") {
                lhs: Div("/") {
                    lhs: UnsignedInteger("1"),
                    rhs: "m2_tau",
                },
                rhs: Sub("-") {
                    lhs: "m2_omega_ref",
                    rhs: "m2_omega",
                },
            },
        },
        Simple {
            lhs: "m2_thrust",
            rhs: Mul("*") {
                lhs: Mul("*") {
                    lhs: "m2_Ct",
                    rhs: "m2_omega",
                },
                rhs: "m2_omega",
            },
        },
        Simple {
            lhs: "m2_moment",
            rhs: Mul("*") {
                lhs: "m2_Cm",
                rhs: "m2_thrust",
            },
        },
        Simple {
            lhs: "der_m3_omega",
            rhs: Mul("*") {
                lhs: Div("/") {
                    lhs: UnsignedInteger("1"),
                    rhs: "m3_tau",
                },
                rhs: Sub("-") {
                    lhs: "m3_omega_ref",
                    rhs: "m3_omega",
                },
            },
        },
        Simple {
            lhs: "m3_thrust",
            rhs: Mul("*") {
                lhs: Mul("*") {
                    lhs: "m3_Ct",
                    rhs: "m3_omega",
                },
                rhs: "m3_omega",
            },
        },
        Simple {
            lhs: "m3_moment",
            rhs: Mul("*") {
                lhs: "m3_Cm",
                rhs: "m3_thrust",
            },
        },
        Simple {
            lhs: "der_m4_omega",
            rhs: Mul("*") {
                lhs: Div("/") {
                    lhs: UnsignedInteger("1"),
                    rhs: "m4_tau",
                },
                rhs: Sub("-") {
                    lhs: "m4_omega_ref",
                    rhs: "m4_omega",
                },
            },
        },
        Simple {
            lhs: "m4_thrust",
            rhs: Mul("*") {
                lhs: Mul("*") {
                    lhs: "m4_Ct",
                    rhs: "m4_omega",
                },
                rhs: "m4_omega",
            },
        },
        Simple {
            lhs: "m4_moment",
            rhs: Mul("*") {
                lhs: "m4_Cm",
                rhs: "m4_thrust",
            },
        },
    ],
    fz: [],
    fm: [],
    fr: {},
}
