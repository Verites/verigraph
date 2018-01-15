---[[
node_type('Revision')
node_type('TransDeps')
node_type('Branch')

edge_type('transDeps', 'Revision', 'TransDeps')
edge_type('dep', 'TransDeps', 'Revision')
edge_type('tip', 'Branch', 'Revision') 
--]]

-- require "git.tg"

foo = graph [[
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
foo:view()
