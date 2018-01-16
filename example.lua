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

bar = Graph [[
  head : Branch
  r1 r2 : Revision
  d1 d2 : Dependencies

  head -:tip-> r2

  r1 -:deps-> d1
  r2 -:deps-> d2

  d2 -:dependsOn-> r1
]]

print(foo)
print()
--print('foo == bar -> ', foo == bar)
print()
print(foo:to_dot('foo'))
--foo:view()
print()
help(foo)
