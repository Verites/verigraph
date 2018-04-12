GrLang.add_node_type('N')

foo = Graph [[ n1 n2 : N ]]

f = Morphism(foo, foo) [[
  n1 -> n2
  n2 -> n1
]]

print('f =', f)
print('f .. f = ', f .. f)
print('foo.identity() = ', foo:identity())
print('composed == identity: ', f .. f == foo:identity())