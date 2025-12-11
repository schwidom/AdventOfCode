"""
https://www.youtube.com/watch?v=EacYNe7moSs
Z3 Explained - Satisfiability Modulo Theories & SMT Solvers
"""

import z3
from z3 import *

class MySolver() :

 def __init__( self) :
  print( f"python MySolver __init__")
  self.solver = Solver()

 def add_int( self, name) :
  print( f"python add_int {name}")
  Int( name)

 def add_ge( self, name1, name2) :
  if type( name1) == str :
   name1 = Int( name1)
  if type( name2) == str :
   name2 = Int( name2)
  self.solver.add( name1 >= name2)

 def add_eq_list( self, name1, name2) :
  name2 = [ Int(x) for x in name2 ]
  self.solver.add( name1 == sum( name2))

 def solve( self, INFO) :
  
  solver = self.solver

  print(solver)
  
  print( "INFO")
  print( INFO)

  print( solver.check()) 
  
  if solver.check() == sat :
   model = solver.model()
   print( model)
   return True
  else :
   return False

