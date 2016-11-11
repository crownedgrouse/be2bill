{application, be2bill, [
	{description, "Erlang be2bill API"},
	{vsn, "0.0.1"},
	{modules, ['be2bill_app','be2bill_fsm','be2bill_lib','be2bill_simplesup','be2bill_srv','be2bill_sup']},
	{registered, [be2bill_sup]},
	{applications, [kernel,stdlib,crypto]},
	{mod, {be2bill_app, []}}
]}.
