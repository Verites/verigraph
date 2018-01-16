require "git-types"

foo = Graph [[
  head : Branch
  r1 r2 : Revision
  d1 d2 : Dependencies

  head -:tip-> r2

  r1 -:deps-> d1
  r2 -:deps-> d2

  d2 -:dependsOn-> r1
]]

print('Printing foo:')
print(foo)
print()

print('Printing foo as dot:')
print(foo:to_dot('foo'))
print()

--foo:view()
help(foo)
