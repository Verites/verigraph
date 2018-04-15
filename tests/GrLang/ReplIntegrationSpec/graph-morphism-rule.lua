require("git-types")

Foo = Graph [[
  r1 r2 : Revision
  dr1 dr2 : Dependencies

  r1 -d1:deps-> dr1
  r2 -d2:deps-> dr2

  dr2 -d22:dependsOn-> r2
  dr2 -d21:dependsOn-> r1
]]

Bar = Graph [[
  r1 r2 r3 : Revision
  dr1 dr2 dr3 : Dependencies

  r1 -d1:deps-> dr1
  r2 -d2:deps-> dr2
  r3 -d3:deps-> dr3

  dr3 -d33:dependsOn-> r3
  dr3 -d32:dependsOn-> r2
  dr3 -d31:dependsOn-> r1
  dr2 -d22:dependsOn-> r2
  dr2 -d21:dependsOn-> r1
]]

fb = Morphism(Foo, Bar) [[
  r1 -> r1; r2 -> r2
  dr1 -> dr1; dr2 -> dr2
  d1 -> d1; d2 -> d2
  d22 -> d22; d21 -> d21
]]

bf = Morphism(Bar, Foo) [[
  r1 -> r1; r2 -> r2; r3 -> r2
  dr1 -> dr1; dr2 -> dr2; dr3 -> dr2
  d1 -> d1; d2 -> d2; d3 -> d2
  d33 -> d22; d32 -> d22; d31 -> d21
  d22 -> d22; d21 -> d21
]]

print("Foo == fb:dom()", Foo == fb:dom())
print("Foo ~= fb:cod()", Foo ~= fb:cod())
print("fb .. bf ~= Bar:identity()", fb .. bf ~= Bar:identity())
print("bf .. fb = Foo:identity()", bf .. fb == Foo:identity())

Amend = Rule [[
  match {
    r1 : Revision
    tr1 : Dependencies
    r1 -:deps-> tr1
  }

  forbid {
    tr2 : Dependencies
    tr2 -:dependsOn-> r1
  }

  clone r1 as r2
  clone tr1 as tr2
  create r2 -:deps-> tr2
]]

--[[
rule Bar {
  require r1 : Revision
  require tr1 : Dependencies
  require r1 -:Deps-> tr1

  forbid tr1-:dependsOn->r1
}--]]