---[[
GrLang.add_node_type('Revision')
GrLang.add_node_type('TransDeps')
GrLang.add_node_type('Branch')

GrLang.add_edge_type('transDeps', 'Revision', 'TransDeps')
GrLang.add_edge_type('dep', 'TransDeps', 'Revision')
GrLang.add_edge_type('tip', 'Branch', 'Revision') 
--]]

-- require "git.tg"

foo = Graph [[
  head : Branch
  r1 r2 : Revision
  d1 d2 : TransDeps

  head -:tip-> r2

  r1 -:transDeps-> d1
  r2 -:transDeps-> d2

  d2 -:dep-> r1
]]

print(foo)
print(foo:to_dot('foo'))
--foo:view()
help(foo)
