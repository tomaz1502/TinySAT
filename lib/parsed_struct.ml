type literal = int

type parsed_clause = literal array

type parsed_formula = parsed_clause array

type parsed_instance =
  { form    : parsed_formula
  ; n_vars  : int
  }
